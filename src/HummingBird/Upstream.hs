{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE UndecidableInstances #-}

module HummingBird.Upstream where

import Control.Concurrent.Async (race)
import Control.Exception (Exception)
import Control.Lens (makeClassyPrisms, makeClassy, view, (#))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger.CallStack (MonadLogger, logDebug)
import Control.Monad.Reader (MonadReader)

import Data.IORef (IORef, readIORef, newIORef)
import Data.Text (pack)

import qualified Network.DNS as DNS

import HummingBird.Config
import HummingBird.Router (Router)
import qualified HummingBird.Router as Router

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
    , MonadLogger m
    , MonadReader c m, HasUpstreamEnv c
    , MonadError e m, AsUpstreamError e
    )

buildUpstreamEnv :: (MonadIO m) => Config -> m (Either UpstreamError UpstreamEnv)
buildUpstreamEnv _config = do
    routers  <- liftIO $ newIORef Router.new
    concur   <- liftIO $ newIORef False
    pure $ Right $ UpstreamEnv routers concur

-- | select a upstream from routers, if no upstream found, use the defaults
resolve :: UpstreamProvision c e m => DNS.Question -> m DNS.DNSMessage
resolve q@(DNS.Question qd _) = do
    router' <- view upstreamRouter
    router  <- liftIO $ readIORef router'
    case Router.find router qd of
        Nothing -> do
            throwError $ _UpstreamEmptyError # ("No upstreams found for " <> show qd)
        Just rs -> do
            logDebug $ pack ("Found " <> show (length rs) <> " resolvers for " <> show qd)
            concur' <- view upstreamConcur
            concur  <- liftIO $ readIORef concur'
            if concur then concurResolve rs q else seqResolve rs q

seqResolve :: UpstreamProvision c e m => [DNS.Resolver] -> DNS.Question -> m DNS.DNSMessage
seqResolve (r:rs) q@(DNS.Question qd qt) = do
    rv <- liftIO $ DNS.lookupRaw r qd qt
    case rv of
        Left  e -> if null rs
            then throwError $ _UpstreamDnsError # e
            else seqResolve rs q
        Right v -> pure v
seqResolve [] (DNS.Question qd _) = throwError $ _UpstreamEmptyError # ("No upstream found for " <> show qd)

concurResolve :: [DNS.Resolver] -> DNS.Question -> m DNS.DNSMessage
concurResolve = undefined
    -- ior <- view upstreamResolvers
    -- rs  <- liftIO $ readIORef ior
    -- r   <- select rs
    -- rv  <- liftIO $ DNS.lookupRaw r qd qt
    -- case rv of
    --     Left  e -> throwError $ _UpstreamDnsError # e
    --     Right v -> pure v
    
    -- where
    --     select :: UpstreamProvision c e m => [DNS.Resolver] -> m DNS.Resolver
    --     select xs = case length xs of
    --         0 -> throwError $ _UpstreamEmptyError # "No upstream avaiable"
    --         1 -> pure (head xs)
    --         _ -> do
    --             rgen    <- view upstreamRadomGen
    --             gen     <- liftIO $ readIORef rgen
    --             let (i, gen') = randomR (0, length xs - 1) gen
    --             _       <- liftIO $ atomicWriteIORef rgen gen'
    --             pure (xs !! i)
