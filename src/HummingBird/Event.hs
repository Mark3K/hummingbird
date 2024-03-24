module HummingBird.Event 
    ( Event (..)
    , Timeout (..)
    ) where

import Data.Time.Clock.System (SystemTime)

import HummingBird.Types (RequestContext)

data Event 
    = RequestEvent RequestContext
    | TimeoutEvent SystemTime Timeout
    | ListenerExit (Maybe String)
    deriving (Show)

data Timeout
    = QueriesTimeout
    deriving (Show)