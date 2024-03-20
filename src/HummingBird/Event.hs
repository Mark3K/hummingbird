module HummingBird.Event 
    ( Event (..)
    , Timeout (..)
    ) where

import Data.Time.Clock.System (SystemTime)
import Network.DNS (DNSMessage)

data Event 
    = MessageEvent DNSMessage
    | TimeoutEvent SystemTime Timeout
    | ListenerExit (Maybe String)
    deriving (Show)

data Timeout
    = QueriesTimeout
    deriving (Show)