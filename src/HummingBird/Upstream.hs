{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

module HummingBird.Upstream where

import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Exception (Exception)
import Control.Lens (makeClassy, view, (^.))
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.IORef (IORef, readIORef, newIORef)

import Katip (KatipContext, Severity(..), logTM, showLS)

import Network.DNS

import HummingBird.Config
import HummingBird.Router
import HummingBird.Cache

data UpstreamError
    = UpstreamDnsError          DNSError
    | UpstreamInvalidQuery      String
    | UpstreamUnsupportError    String
    | UpstreamEmptyError        String
    | UpstreamUnknownError      String
    deriving (Show, Eq)

instance Exception UpstreamError

data UpstreamEnv = UpstreamEnv
    { _upstreamRouter   :: IORef Router
    , _upstreamConcur   :: IORef Bool
    , _upstreamCache    :: Maybe Cache
    }
makeClassy ''UpstreamEnv

type UpstreamProvision c e m = 
    ( MonadIO m
    , MonadThrow m
    , KatipContext m
    , MonadBaseControl IO m
    , MonadReader c m, HasUpstreamEnv c
    )

buildUpstreamEnv :: (MonadIO m, MonadThrow m) => UpstreamConfig -> m UpstreamEnv
buildUpstreamEnv config = do
    routers <- liftIO $ newIORef newRouter
    concur  <- liftIO $ newIORef (config ^. upstreamConfigConcurrent)
    cache   <- if cacheConfig ^. cacheConfigEnable
        then Just <$> mkCache 
            (cacheConfig ^. cacheConfigMaxSize) 
            (cacheConfig ^. cacheConfigMaxTTL)
            (cacheConfig ^. cacheConfigMinTTL)
        else pure Nothing
    pure $ UpstreamEnv routers concur cache
    where
        cacheConfig = config ^. upstreamConfigCache

resolve :: UpstreamProvision c e m => DNSMessage -> m DNSMessage
resolve query = do
    cache <- view upstreamCache
    case cache of
        Nothing -> resolveFromUpstream query
        Just  c -> resolveWithCache query c

resolveWithCache :: UpstreamProvision c e m => DNSMessage -> Cache -> m DNSMessage
resolveWithCache query cache = do
    v <- lookupCache key cache
    i <- case v of
        Nothing             -> do
            response <- resolveFromUpstream query
            case (rcode . flags . header) response of
                NoErr -> do
                    _ <- insertCache response cache
                    pure $ buildItem response
                _     -> pure $ buildItem response

        Just (_ttl, item)   -> pure item
    pure $ withRRs i $ toResponse query
    where
        key = buildKey query

toResponse :: DNSMessage -> DNSMessage
toResponse DNSMessage{..} = case op header of
    QR_Query    -> DNSMessage
        { header        = DNSHeader 
            { identifier = identifier header
            , flags      = defaultDNSFlags
                { qOrR          = QR_Response
                , recDesired    = rd header
                , chkDisable    = cd header
                }
            }
        , ednsHeader    = NoEDNS
        , question      = question
        , answer        = []
        , authority     = []
        , additional    = []
        }
    QR_Response -> error "try to build response from response"

    where 
        cd DNSHeader{..} = chkDisable flags 
        rd DNSHeader{..} = recDesired flags
        op DNSHeader{..} = qOrR       flags

withRRs :: Item -> DNSMessage -> DNSMessage
withRRs Item {..} msg = msg 
    { header        = header' { flags = flags' 
        { authenData    = _itemHeaderAD 
        , authAnswer    = True
        , recAvailable  = _itemHeaderRD
        }}
    , answer        = _itemAnswer
    , authority     = _itemAuthority
    , additional    = _itemAdditional
    }
    where
        header' = header msg
        flags'  = flags header'

resolveFromUpstream :: UpstreamProvision c e m => DNSMessage -> m DNSMessage
resolveFromUpstream q = do
    router' <- view upstreamRouter
    router  <- liftIO $ readIORef router'
    qd      <- qName q
    case findResolvers router qd of
        Nothing -> do
            throwM $ UpstreamEmptyError ("No upstreams found for " <> show qd)
        Just rs -> do
            $(logTM) DebugS ("Found " <> showLS (length rs) <> " resolvers for " <> showLS qd)
            concur' <- view upstreamConcur
            concur  <- liftIO $ readIORef concur'
            if concur then concurResolve rs q else seqResolve rs q
            
qName :: UpstreamProvision c e m => DNSMessage -> m Domain 
qName DNSMessage {question = qs} = case qs of
    []      -> throwM $ UpstreamInvalidQuery "question is empty"
    [q]     -> pure $ qname q
    (_:_)   -> throwM $ UpstreamInvalidQuery "question is more than 1"

qType :: UpstreamProvision c e m => DNSMessage -> m TYPE
qType DNSMessage {question = qs} = case qs of
    []      -> throwM $ UpstreamInvalidQuery "question is empty"
    [q]     -> pure $ qtype q
    (_:_)   -> throwM $ UpstreamInvalidQuery "question is more than 1"

seqResolve :: UpstreamProvision c e m => [(Upstream, Resolver)] -> DNSMessage -> m DNSMessage
seqResolve ((upstream, r):rs) q = do
    qd <- qName q
    qt <- qType q
    $(logTM) DebugS ("sequence resolve " <> showLS qd <> " from " <> showLS upstream)
    rv <- liftIO $ lookupRaw r qd qt
    case rv of
        Left  e -> if null rs
            then throwM $ UpstreamDnsError e
            else seqResolve rs q
        Right v -> pure v
seqResolve [] _ = throwM $ UpstreamEmptyError "No upstream"

concurResolve :: UpstreamProvision c e m => [(Upstream, Resolver)] -> DNSMessage -> m DNSMessage
concurResolve rs q = do
    qd <- qName q
    qt <- qType q
    $(logTM) DebugS ("concurrent resolve " <> showLS qd <> " from " <> showLS (map fst rs))
    queries <- mapM (\(_, r) -> liftIO $ async $ lookupRaw r qd qt) rs
    (_, rv) <- liftIO $ waitAnyCancel queries
    case rv of
        Left  e -> throwM $ UpstreamDnsError e
        Right v -> pure v
    