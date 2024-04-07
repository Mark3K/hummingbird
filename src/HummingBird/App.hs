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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE RecordWildCards            #-}

module HummingBird.App 
    ( app
    , defaultAppEnv
    , AppEnv(..)
    , AppError(..)
    , appEnvConfig
    ) where

import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM (newTChanIO, TChan, atomically, readTChan, writeTChan)
import Control.Lens (makeClassy, makeClassyPrisms, view, (^.), (#))
import Control.Monad (forever)
import Control.Monad.Logger.CallStack (logInfo, logDebug)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader)

import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Text (pack)

import qualified Network.DNS as DNS

import HummingBird.Config
import HummingBird.Event
import HummingBird.Upstream
import HummingBird.Server
import HummingBird.Types
import Control.Monad.Except (MonadError(throwError))

data AppError 
    = AppServerError ServerError
    | AppDNSError DNS.DNSError
    | AppUpstreamError UpstreamError
    | AppInvalidConfigError String
    deriving (Show, Eq)
makeClassyPrisms ''AppError

data AppEnv = AppEnv 
    { _appEnvConfig         :: Config 
    , _appEnvResolvers      :: IORef [DNS.Resolver]
    }
makeClassy ''AppEnv

defaultAppEnv :: (MonadIO m) => m AppEnv
defaultAppEnv = do
    resolvers <- liftIO $ newIORef mempty
    pure AppEnv 
        { _appEnvConfig     = defaultConfig 
        , _appEnvResolvers  = resolvers
        }

instance HasServerEnv AppEnv where
    serverEnv f ev = ev <$ f ev'
        where
            ev' = ServerEnv
                { _serverEnvHost        = ev ^. appEnvConfig . cfgListenAddr
                , _serverEnvPort        = ev ^. appEnvConfig . cfgListenPort
                , _serverEnvEableTCP    = ev ^. appEnvConfig . cfgEnableTCP
                }

instance AsServerError AppError where
    _ServerError = _AppServerError . _ServerError

instance HasUpstreamEnv AppEnv where
    upstreamEnv f ev = ev <$ f ev'
        where ev' = UpstreamEnv { _upstreamResolvers = ev ^. appEnvResolvers }

instance AsUpstreamError AppError where
    _UpstreamError = _AppUpstreamError . _UpstreamError

type AppProvision c e m = (HasAppEnv c, AsAppError e, ServerProvision c e m, UpstreamProvision c e m)

-- | TODO
-- 1, start server
-- 2, start upstream
-- 3, start event loop to process messages
app :: AppProvision c e m => m ()
app = do
    initializeUpstreamsEnv
    logInfo "start humming bird server ..."
    ch  <- liftIO newTChanIO
    _   <- fork $ serve ch
    logInfo "start event loop ..."
    forever $ eventLoop ch

eventLoop :: AppProvision c e m => TChan Event -> m ()
eventLoop ch = do
    event <- liftIO $ atomically $ readTChan ch
    case event of
         RequestEvent RequestContext{..} -> do
            logDebug $ pack ("request event: " <> show requestContextMessage)
            rv <- process requestContextMessage
            case rv of
                Left    e -> throwError $ _AppDNSError # e
                Right msg -> do
                    liftIO $ atomically $ writeTChan requestContextChannel $ ResponseContext msg requestContextAddr
         TimeoutEvent systime timeout -> undefined
         ListenerExit reason -> undefined
    pure()

    where
        process DNS.DNSMessage{ DNS.header = hd, DNS.question = q } = do
            rrs <- resolve $ head q
            case rrs of
                Left   e -> pure (Left e)
                Right rs -> do
                    let hd0 = DNS.header DNS.defaultResponse
                        resp = DNS.defaultResponse {
                            DNS.header      = hd0 { DNS.identifier = DNS.identifier hd },
                            DNS.question    = q,
                            DNS.answer      = rs
                        }
                    pure (Right resp)

initializeUpstreamsEnv :: AppProvision c e m => m ()
initializeUpstreamsEnv = do
    ups     <- view (appEnvConfig . cfgUpstreams)
    seeds   <- liftIO $ mapM DNS.makeResolvSeed (resolveConfs ups)
    rslvs   <- view appEnvResolvers
    liftIO $ mapM_ (`DNS.withResolver` insert rslvs) seeds

    where
        toResolveConf ip mport = case mport of
            Nothing   -> DNS.RCHostName ip
            Just port -> DNS.RCHostPort ip port
        
        resolveConfs ups = 
            [ DNS.defaultResolvConf {DNS.resolvInfo = toResolveConf (show ip) mport} 
            | (ip, mport) <- ups ]

        insert :: IORef [DNS.Resolver] -> DNS.Resolver -> IO ()
        insert m r = atomicModifyIORef' m (\m' -> (r:m', ()))
