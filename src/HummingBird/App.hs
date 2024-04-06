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

module HummingBird.App 
    ( app
    , defaultAppEnv
    , AppEnv(..)
    , AppError(..)
    , appEnvConfig
    ) where

import qualified Data.Map as Map

import Control.Concurrent.STM (TVar, TChan, newTVarIO, newTChanIO)
import Control.Lens (makeClassy, makeClassyPrisms, (^.), view)
import Control.Monad.Logger.CallStack (logDebug)
import Control.Monad.Reader (MonadIO (liftIO))

import Network.DNS

import HummingBird.Config
import HummingBird.Event
import HummingBird.Server

data AppError 
    = AppServerError ServerError
    | AppOtherError String
    deriving (Show, Eq)
makeClassyPrisms ''AppError

data AppEnv = AppEnv
    { _appEnvConfig     :: Config
    , _appEnvQueries    :: TVar (Map.Map Identifier DNSMessage)
    , _appEnvEventCh    :: TChan Event
    }
makeClassy ''AppEnv

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

type AppProvision c e m = (HasAppEnv c, AsAppError e, ServerProvision c e m)

-- | TODO
-- 1, start server
-- 2, start upstream
-- 3, start event loop to process messages
app :: AppProvision c e m => m ()
app = do
    logDebug "start humming bird server ..."
    ch <- view (appEnv . appEnvEventCh)
    serve ch 

defaultAppEnv :: (MonadIO m) => m AppEnv
defaultAppEnv = do
    queries <- liftIO $ newTVarIO Map.empty
    eventCh <- liftIO newTChanIO
    pure AppEnv
        { _appEnvConfig     = defaultConfig
        , _appEnvQueries    = queries
        , _appEnvEventCh    = eventCh
        }