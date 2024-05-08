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
import Control.Lens (makeClassy)
import Control.Exception.Lifted (catch, Exception, SomeException(..))
import Control.Monad.Catch (MonadThrow (throwM))

import Katip (Severity(..), logTM, showLS)

import HummingBird.Config
import HummingBird.Event
import qualified HummingBird.Server.UDP as UDP
import qualified HummingBird.Server.TCP as TCP

data ServerError 
    = ServerUdpError UDP.UdpServerError
    | ServerTcpError TCP.TcpServerError
    | ServerUnexpectedError String
    deriving (Show, Eq)

instance Exception ServerError

data ServerEnv = ServerEnv 
    { _serverEnvUdp         :: UDP.UdpServerEnv
    , _serverEnvTcp         :: TCP.TcpServerEnv
    }
makeClassy ''ServerEnv

instance (HasServerEnv m) => UDP.HasUdpServerEnv m where
    udpServerEnv = serverEnv . serverEnvUdp
       
instance (HasServerEnv m) => TCP.HasTcpServerEnv m where
    tcpServerEnv = serverEnv . serverEnvTcp

type ServerProvision c e m =
    ( HasServerEnv c
    , UDP.UdpServerProvision c e m
    , TCP.TcpServerProvision c e m
    )

buildServerEnv :: (MonadThrow m) => ServerConfig -> m ServerEnv
buildServerEnv config = do
    udp <- UDP.buildUdpServerEnv config
    tcp <- TCP.buildTcpServerEnv config
    pure $ ServerEnv udp tcp

serve :: ServerProvision c e m => TChan Event -> m ()
serve ch = do
    $(logTM) DebugS "begin to start servers ..."
    _   <- fork $ UDP.serve ch `catch` (\(SomeException e) -> do
        $(logTM) ErrorS ("UDP server error: " <> showLS e)
        throwM $ ServerUnexpectedError (show e))
    _   <- fork $ TCP.serve ch `catch` (\(SomeException e) -> do
        $(logTM) ErrorS ("TCP server error: " <> showLS e)
        throwM $ ServerUnexpectedError (show e))
    pure ()