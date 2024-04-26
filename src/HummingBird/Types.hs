{-# LANGUAGE RecordWildCards #-}

module HummingBird.Types
    ( RequestContext (..) 
    , TcpRequest (..)
    , TcpResponse (..)
    , UdpRequest (..)
    , UdpResponse (..)
    , Upstream (..)
    , Route (..)
    ) where

import Control.Concurrent.STM (TMVar, TChan)

import Data.IP (IP)

import Network.DNS (Domain, DNSMessage)
import Network.Socket (SockAddr, PortNumber)

data RequestContext 
    = RequestTcp TcpRequest
    | RequestUdp UdpRequest
    deriving (Show)

data TcpRequest = TcpRequest
    { tcpRequestMessage     :: DNSMessage 
    , tcpRequestResponseVar :: TMVar TcpResponse
    }

instance Show TcpRequest where
    show TcpRequest{..} = "TcpRequest { message = " 
        <> show tcpRequestMessage
        <> "}"

data UdpRequest = UdpRequest
    { udpRequestMessage     :: DNSMessage
    , udpRequestAddr        :: SockAddr
    , udpRequestResponseCh  :: TChan UdpResponse
    }

instance Show UdpRequest where
    show UdpRequest{..} = "UdpRequest { message = " 
        <> show udpRequestMessage 
        <> ", addr = " 
        <> show udpRequestAddr
        <> "}"

newtype TcpResponse = TcpResponse
    { tcpResponseMessage     :: DNSMessage
    } deriving (Show)

data UdpResponse = UdpResponse
    { udpResponseMessage     :: DNSMessage
    , udpResponseAddr        :: SockAddr
    } deriving (Show)

data Upstream   = Upstream IP (Maybe PortNumber) deriving (Show, Eq, Ord)

data Route      = Route [Domain] Upstream deriving (Show, Eq)