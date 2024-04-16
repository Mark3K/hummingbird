{-# LANGUAGE RecordWildCards #-}

module HummingBird.Types 
    ( RequestContext (..) 
    , ResponseContext (..)
    , TcpResponse (..)
    , UdpResponse (..)
    , Upstream (..)
    , Route (..)
    ) where

import Control.Concurrent.STM (TChan)

import Data.IP (IP)

import Network.DNS (Domain, DNSMessage)
import Network.Socket (SockAddr, PortNumber)

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

data Upstream   = Upstream IP (Maybe PortNumber) deriving (Show, Eq, Ord)

data Route      = Route [Domain] Upstream deriving (Show, Eq)