{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TemplateHaskell    #-}

module HummingBird.Upstream where

import Control.Exception (Exception)
import Control.Lens (makeClassyPrisms, makeClassy, view, (#))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger.CallStack (MonadLogger)
import Control.Monad.Reader (MonadReader)

import Data.IORef (IORef, readIORef, atomicWriteIORef, newIORef)

import qualified Network.DNS as DNS
import System.Random (StdGen, randomR, initStdGen)

import HummingBird.Config

data UpstreamError
    = UpstreamDnsError          DNS.DNSError
    | UpstreamUnsupportError    String
    | UpstreamEmptyError        String
    | UpstreamUnknownError      String
    deriving (Show, Eq)
makeClassyPrisms ''UpstreamError

instance Exception UpstreamError

data UpstreamEnv = UpstreamEnv
    { _upstreamResolvers    :: IORef [DNS.Resolver]
    , _upstreamRadomGen     :: IORef StdGen
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
    resolvers <- liftIO $ newIORef mempty
    randomgen <- initStdGen
    refgen    <- liftIO $ newIORef randomgen
    pure $ Right $ UpstreamEnv resolvers refgen

resolve :: UpstreamProvision c e m => DNS.Question -> m DNS.DNSMessage
resolve (DNS.Question qd qt) = do
    ior <- view upstreamResolvers
    rs  <- liftIO $ readIORef ior
    r   <- select rs
    rv  <- liftIO $ DNS.lookupRaw r qd qt
    case rv of
        Left  e -> throwError $ _UpstreamDnsError # e
        Right v -> pure v
    
    where
        select :: UpstreamProvision c e m => [DNS.Resolver] -> m DNS.Resolver
        select xs = case length xs of
            0 -> throwError $ _UpstreamEmptyError # "No upstream avaiable"
            1 -> pure (head xs)
            _ -> do
                rgen    <- view upstreamRadomGen
                gen     <- liftIO $ readIORef rgen
                let (i, gen') = randomR (0, length xs - 1) gen
                _       <- liftIO $ atomicWriteIORef rgen gen'
                pure (xs !! i)
