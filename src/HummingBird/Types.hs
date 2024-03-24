{-# LANGUAGE RecordWildCards #-}

module HummingBird.Types 
    ( RequestContext (..) 
    , ResponseContext (..)
    ) where

import Control.Concurrent.STM (TChan)

import Network.DNS (DNSMessage)
import Network.Socket (SockAddr)

data RequestContext = RequestContext
    { requestContextMessage     :: DNSMessage 
    , requestContextAddr        :: SockAddr
    , requestContextChannel     :: TChan ResponseContext
    }

instance Show RequestContext where
    show RequestContext {..} = show requestContextMessage

data ResponseContext = ResponseContext
    { responseContextMessage    :: DNSMessage
    , responseContextAddr       :: SockAddr
    } deriving (Show)

