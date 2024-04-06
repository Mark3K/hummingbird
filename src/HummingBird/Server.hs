{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HummingBird.Server where

import Control.Concurrent.STM (TChan)
import Control.Lens (makeClassy, makeClassyPrisms, (^.))
import Control.Monad.Logger.CallStack (logDebug)

import Network.Socket

import HummingBird.Event
import qualified HummingBird.Server.UDP as UDP

data ServerError 
    = ServerUdpError UDP.UdpServerError
    | ServerTcpError Int
    deriving (Show, Eq)
makeClassyPrisms ''ServerError

instance (AsServerError e) => UDP.AsUdpServerError e where
    _UdpServerError = _ServerError . _ServerUdpError

data ServerEnv = ServerEnv 
    { _serverEnvHost        :: HostName
    , _serverEnvPort        :: ServiceName
    , _serverEnvEableTCP    :: Bool 
    } deriving (Show)
makeClassy ''ServerEnv

instance (HasServerEnv m) => UDP.HasUdpServerEnv m where
    udpServerEnv f m = m <$ f ev'
        where
            ev' = UDP.UdpServerEnv 
                { UDP._udpServerEnvHost = m ^. serverEnv . serverEnvHost
                , UDP._udpServerEnvPort = m ^. serverEnv . serverEnvPort
                }

type ServerProvision c e m = 
    ( HasServerEnv c
    , AsServerError e
    , UDP.UdpServerProvision c e m
    )

serve :: ServerProvision c e m => TChan Event -> m ()
serve ch = do
    logDebug "begin to start servers ..."
    UDP.serve ch