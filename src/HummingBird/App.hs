{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE LambdaCase                 #-}

module HummingBird.App 
    ( app
    , buildAppEnv
    , appEnvConfig
    , AppEnv(..)
    , AppError(..)
    ) where

import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM (newTChanIO, TChan, atomically, readTChan, writeTChan)
import Control.Exception (Exception)
import Control.Lens (makeClassy, makeClassyPrisms, view, (#))
import Control.Monad (forever)
import Control.Monad.Except (MonadError(throwError, catchError))
import Control.Monad.Logger.CallStack (MonadLogger, logInfo, logDebug, logError)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader)
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Bifunctor (bimap)
import Data.IORef (IORef, atomicModifyIORef')
import Data.Text (pack)

import qualified Network.DNS as DNS

import HummingBird.Config
import HummingBird.Event
import HummingBird.Upstream
import HummingBird.Server
import HummingBird.Types
import HummingBird.Router (Router)
import qualified HummingBird.Router as Router
import HummingBird.Parser.ServerConfig (routesFromFile)

data AppError 
    = AppServerError    ServerError
    | AppUpstreamError  UpstreamError
    | AppConfigError    String
    deriving (Show, Eq)
makeClassyPrisms ''AppError

instance Exception AppError

data AppEnv = AppEnv 
    { _appEnvConfig         :: Config 
    , _appEnvUpstream       :: UpstreamEnv
    , _appEnvServer         :: ServerEnv
    }
makeClassy ''AppEnv

buildAppEnv :: (MonadIO m) => Config -> m (Either AppError AppEnv)
buildAppEnv config = do
    upstream <- buildUpstreamEnv config
    server <- buildServerEnv config
    pure $ bimap (_AppUpstreamError #) build upstream 
         >>= (\b -> bimap (_AppServerError #) b server)
    where
        build u s = AppEnv
            { _appEnvConfig     = config
            , _appEnvUpstream   = u
            , _appEnvServer     = s
            }

instance HasServerEnv AppEnv where
    serverEnv = appEnv . appEnvServer

instance AsServerError AppError where
    _ServerError = _AppServerError . _ServerError

instance HasUpstreamEnv AppEnv where
    upstreamEnv = appEnv . appEnvUpstream

instance AsUpstreamError AppError where
    _UpstreamError = _AppUpstreamError . _UpstreamError

type AppProvision m = 
    ( MonadIO m
    , MonadLogger m
    , MonadBaseControl IO m
    , MonadReader AppEnv m 
    , MonadError AppError m
    )

app :: AppProvision m =>  m ()
app = do
    logInfo "initialize server enviroment"
    initializeUpstreamsEnv
    logInfo "start humming bird server"
    ch  <- liftIO newTChanIO
    serve ch
    logInfo "start event loop"
    forever $ eventLoop ch `catchError` \case
        AppUpstreamError e -> do
            logError $ pack ("upstream error: " <> show e)
            pure ()
        e                  -> throwError e

eventLoop :: AppProvision m => TChan Event -> m ()
eventLoop ch = do
    event <- liftIO $ atomically $ readTChan ch
    case event of
         RequestEvent ctx -> do
            tid <- fork (handleRequest ctx)
            logDebug $ pack ("start thread " <> show tid <> " to handle request")
         -- TODO
         TimeoutEvent systime timeout -> undefined
         ListenerExit reason -> undefined

handleRequest :: AppProvision m => RequestContext -> m ()
handleRequest RequestContext{..} = do
    logDebug $ pack ("handle request event: " <> show requestContextMessage)
    resp <- handle requestContextMessage `catchError` \case 
        AppUpstreamError e -> do
            logError $ pack ("unexpected error: " <> show e)
            buildErrorResponse requestContextMessage e
        e                  -> throwError e
    rc   <- case requestContextAddr of
        Nothing     -> pure $ ResponseTcp $ TcpResponse resp
        Just addr   -> pure $ ResponseUdp $ UdpResponse resp addr
    liftIO $ atomically $ writeTChan requestContextChannel rc
    where
        handle r = preprocess r >>= process >>= postprocess

preprocess :: AppProvision m => DNS.DNSMessage -> m DNS.DNSMessage
preprocess r@DNS.DNSMessage{ DNS.question = qs } = do
    refuseAny <- view (appEnvConfig . configRefuseAny)
    if refuseAny
        then    pure r { DNS.question = filter nonany qs }
        else    pure r
    
    where nonany DNS.Question { DNS.qtype = typ } = typ /= DNS.ANY

process :: AppProvision m => DNS.DNSMessage -> m DNS.DNSMessage
process DNS.DNSMessage{DNS.header = hd, DNS.question = qs} = case qs of
    [q] -> do
        rc <- resolve q
        pure rc { DNS.header = (DNS.header rc) { DNS.identifier = DNS.identifier hd } }
    _   -> throwError $ _UpstreamUnsupportError # ("invalid question number " <> show (length qs))

buildErrorResponse :: MonadIO m => DNS.DNSMessage -> UpstreamError -> m DNS.DNSMessage
buildErrorResponse DNS.DNSMessage{DNS.header = hd, DNS.question = qs} err = 
    pure DNS.defaultResponse 
        { DNS.header    = (DNS.header DNS.defaultResponse) 
            { DNS.identifier = DNS.identifier hd
            , DNS.flags = (DNS.flags $ DNS.header DNS.defaultResponse) { DNS.rcode = rcode }
            }
        , DNS.question  = qs
        }
    where
        rcode = case err of
            UpstreamDnsError e          -> rcode' e
            UpstreamUnsupportError _    -> DNS.FormatErr
            _                           -> DNS.ServFail
            
        rcode' e = case e of
            DNS.FormatError         -> DNS.FormatErr
            DNS.ServerFailure       -> DNS.ServFail
            _                       -> DNS.BadRCODE 
    
postprocess :: Applicative m => DNS.DNSMessage -> m DNS.DNSMessage
postprocess = pure
    
initializeUpstreamsEnv :: AppProvision m => m ()
initializeUpstreamsEnv = initializeDefaultUpstreams >> initializeUpstreamFromFiles

initializeDefaultUpstreams :: (MonadIO m, MonadReader AppEnv m) => m ()
initializeDefaultUpstreams = do
    ups     <- view (appEnvConfig . configUpstreams)
    seeds   <- liftIO $ mapM (DNS.makeResolvSeed . buildResolveConf) ups
    router  <- view (appEnvUpstream . upstreamRouter)
    liftIO $ mapM_ (insert router . ("", )) seeds


buildResolveConf :: Upstream -> DNS.ResolvConf
buildResolveConf (Upstream ip mport) = DNS.defaultResolvConf
    { DNS.resolvInfo = case mport of
        Nothing   -> DNS.RCHostName (show ip)
        Just port -> DNS.RCHostPort (show ip) port
    }

initializeUpstreamFromFiles 
    :: (MonadIO m, MonadReader AppEnv m, MonadError AppError m)
    => m ()
initializeUpstreamFromFiles = do
    files  <- view (appEnvConfig . configUpstreamFiles)
    routes <- concat <$> mapM readroutes files
    seeds  <- concat <$> mapM buildseeds routes
    router  <- view (appEnvUpstream . upstreamRouter)
    liftIO $ mapM_ (insert router) seeds
    where
        readroutes path = do
            rs' <- liftIO $ routesFromFile path
            case rs' of
                Left   e -> throwError $ _AppConfigError # ("error parse routes from " <> path <> ": " <> show e)
                Right rs -> pure rs

        buildseeds (Route domains upstream) = do
            liftIO $ mapM (\(d, c) -> (d, ) <$> DNS.makeResolvSeed c) rs
            where
                rs = map (, buildResolveConf upstream) domains

insert :: IORef Router -> (DNS.Domain, DNS.ResolvSeed) -> IO ()
insert rr (dn, seed) = DNS.withResolver seed (\resolver -> 
    atomicModifyIORef' rr (\router -> (Router.insert router dn resolver, ())))