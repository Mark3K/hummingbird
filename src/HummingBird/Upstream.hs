{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE UndecidableInstances #-}

module HummingBird.Upstream where

import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Exception (Exception)
import Control.Lens (makeClassyPrisms, makeClassy, view, (^.), (#))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader)

import Data.IORef (IORef, readIORef, newIORef)

import Katip (KatipContext, Severity(..), logTM, showLS)

import qualified Network.DNS as DNS

import HummingBird.Config
import HummingBird.Router

data UpstreamError
    = UpstreamDnsError          DNS.DNSError
    | UpstreamUnsupportError    String
    | UpstreamEmptyError        String
    | UpstreamUnknownError      String
    deriving (Show, Eq)
makeClassyPrisms ''UpstreamError

instance Exception UpstreamError

data UpstreamEnv = UpstreamEnv
    { _upstreamRouter   :: IORef Router
    , _upstreamConcur   :: IORef Bool
    }
makeClassy ''UpstreamEnv

type UpstreamProvision c e m = 
    ( MonadIO m
    , KatipContext m
    , MonadReader c m, HasUpstreamEnv c
    , MonadError e m, AsUpstreamError e
    )

buildUpstreamEnv :: (MonadIO m) => Config -> m (Either UpstreamError UpstreamEnv)
buildUpstreamEnv config = do
    routers  <- liftIO $ newIORef newRouter
    concur   <- liftIO $ newIORef (config ^. configUpstream . upstreamConfigConcurrent)
    pure $ Right $ UpstreamEnv routers concur

-- | select a upstream from routers, if no upstream found, use the defaults
resolve :: UpstreamProvision c e m => DNS.Question -> m DNS.DNSMessage
resolve q@(DNS.Question qd _) = do
    router' <- view upstreamRouter
    router  <- liftIO $ readIORef router'
    case findResolvers router qd of
        Nothing -> do
            throwError $ _UpstreamEmptyError # ("No upstreams found for " <> show qd)
        Just rs -> do
            $(logTM) DebugS ("Found " <> showLS (length rs) <> " resolvers for " <> showLS qd)
            concur' <- view upstreamConcur
            concur  <- liftIO $ readIORef concur'
            if concur then concurResolve rs q else seqResolve rs q

seqResolve :: UpstreamProvision c e m => [(Upstream, DNS.Resolver)] -> DNS.Question -> m DNS.DNSMessage
seqResolve ((upstream, r):rs) q@(DNS.Question qd qt) = do
    $(logTM) DebugS ("sequence resolve " <> showLS qd <> " from " <> showLS upstream)
    rv <- liftIO $ DNS.lookupRaw r qd qt
    case rv of
        Left  e -> if null rs
            then throwError $ _UpstreamDnsError # e
            else seqResolve rs q
        Right v -> pure v
seqResolve [] (DNS.Question qd _) = throwError $ _UpstreamEmptyError # ("No upstream found for " <> show qd)

concurResolve :: UpstreamProvision c e m => [(Upstream, DNS.Resolver)] -> DNS.Question -> m DNS.DNSMessage
concurResolve rs (DNS.Question qd qt) = do
    $(logTM) DebugS ("concurrent resolve " <> showLS qd <> " from " <> showLS (map fst rs))
    queries <- mapM (\(_, r) -> liftIO $ async $ DNS.lookupRaw r qd qt) rs
    (_, rv) <- liftIO $ waitAnyCancel queries
    case rv of
        Left  e -> throwError $ _UpstreamDnsError # e
        Right v -> pure v
    