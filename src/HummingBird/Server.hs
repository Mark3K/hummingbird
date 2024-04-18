{-# OPTIONS_GHC -Wno-orphans            #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

module HummingBird.Server where

import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM (TChan)
import Control.Exception.Lifted (catch, SomeException)
import Control.Lens (makeClassy, makeClassyPrisms, view, (^.), (#))
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO)

import Data.Bifunctor (bimap)

import Katip (Severity(..), logTM, showLS)

import HummingBird.Config
import HummingBird.Event
import qualified HummingBird.Server.UDP as UDP
import qualified HummingBird.Server.TCP as TCP

data ServerError 
    = ServerUdpError UDP.UdpServerError
    | ServerTcpError TCP.TcpServerError
    deriving (Show, Eq)
makeClassyPrisms ''ServerError

instance (AsServerError e) => UDP.AsUdpServerError e where
    _UdpServerError = _ServerError . _ServerUdpError

instance (AsServerError e) => TCP.AsTcpServerError e where
    _TcpServerError = _ServerError . _ServerTcpError

data ServerEnv = ServerEnv 
    { _serverEnvUdp         :: UDP.UdpServerEnv
    , _serverEnvTcp         :: TCP.TcpServerEnv
    , _serverEnvEnableTcp   :: Bool
    }
makeClassy ''ServerEnv

instance (HasServerEnv m) => UDP.HasUdpServerEnv m where
    udpServerEnv = serverEnv . serverEnvUdp
       
instance (HasServerEnv m) => TCP.HasTcpServerEnv m where
    tcpServerEnv = serverEnv . serverEnvTcp

type ServerProvision c e m =
    ( HasServerEnv c
    , AsServerError e
    , UDP.UdpServerProvision c e m
    , TCP.TcpServerProvision c e m
    )

buildServerEnv :: (MonadIO m) => Config -> m (Either ServerError ServerEnv)
buildServerEnv config = do
    udp <- UDP.buildUdpServerEnv config
    tcp <- TCP.buildTcpServerEnv config
    pure $ bimap (_ServerUdpError #) build udp 
         >>= (\b -> bimap (_ServerTcpError #) b tcp)
    where 
        build u t = ServerEnv
            { _serverEnvUdp         = u
            , _serverEnvTcp         = t
            , _serverEnvEnableTcp   = config ^. configEnableTCP
            }

serve :: ServerProvision c e m => TChan Event -> m ()
serve ch = do
    $(logTM) DebugS "begin to start servers ..."
    _   <- fork $ catch (UDP.serve ch) (\(e :: SomeException) -> do
        $(logTM) ErrorS ("UDP server error: " <> showLS e))
    tcp <- view serverEnvEnableTcp
    when tcp $ void $ fork $ catch (TCP.serve ch) (\(e :: SomeException) -> do
        $(logTM) ErrorS ("TCP server error: " <> showLS e))