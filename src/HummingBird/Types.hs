{-# LANGUAGE RecordWildCards #-}

module HummingBird.Types 
    ( RequestContext (..) 
    , ResponseContext (..)
    , TcpResponse (..)
    , UdpResponse (..)
    ) where

import Control.Concurrent.STM (TChan)

import Network.DNS (DNSMessage)
import Network.Socket (SockAddr)

data RequestContext = RequestContext
    { requestContextMessage     :: DNSMessage 
    , requestContextAddr        :: Maybe SockAddr
    , requestContextChannel     :: TChan ResponseContext
    }

instance Show RequestContext where
    show RequestContext {..} = show requestContextMessage

data ResponseContext
    = ResponseTcp TcpResponse
    | ResponseUdp UdpResponse
    deriving (Show)

newtype TcpResponse = TcpResponse
    { trMessage     :: DNSMessage
    } deriving (Show)

data UdpResponse = UdpResponse
    { urMessage     :: DNSMessage
    , urAddr        :: SockAddr
    } deriving (Show)
