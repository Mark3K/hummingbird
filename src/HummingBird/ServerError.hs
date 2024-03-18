module HummingBird.ServerError 
    (ServerError (..)
    ) where

import Data.Text (Text)

data ServerError 
    = ErrorInitializeLogFile Text
    | InvalidUpstream Text
    deriving (Show)
