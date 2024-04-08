{-# LANGUAGE TemplateHaskell #-}
module HummingBird.Server.TCP where

import Control.Lens (makeClassyPrisms, makeClassy)
import Network.Socket (HostName, ServiceName)

data TcpServerError 
    = NoAddrAvailable (HostName, ServiceName)
    | SocketError String
    deriving (Show, Eq)
makeClassyPrisms ''TcpServerError

data TcpServerEnv = TcpServerEnv
    { _tcpServerEnvHost     :: HostName
    , _tcpServerEnvPort     :: ServiceName
    } deriving (Show, Eq)
makeClassy ''TcpServerEnv