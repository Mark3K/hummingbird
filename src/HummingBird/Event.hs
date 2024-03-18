module HummingBird.Event 
    ( Event (..)
    , Timeout (..)
    ) where

import Data.Time.Clock.System (SystemTime)
import Network.DNS (DNSMessage)

data Event 
    = MessageEvent DNSMessage
    | TimeoutEvent SystemTime Timeout

data Timeout
    = QueriesTimeout
    deriving (Show)