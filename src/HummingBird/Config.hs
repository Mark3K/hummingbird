module HummingBird.Config 
    ( defaultConfig
    , LogOutput (..)
    , Config (..)
    ) where

import Control.Monad.Logger (LogLevel(..))
import Data.Int (Int16)


data LogOutput = FileOutput FilePath
               | Stdout
               deriving (Show)

data Config = Config 
    { cfgLogLevel       :: LogLevel
    , cfgLogOutput      :: LogOutput
    , cfgListenAddrs    :: [String]
    , cfgListenPorts    :: [Int16]
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config 
    { cfgLogLevel       = LevelInfo
    , cfgLogOutput      = Stdout
    , cfgListenAddrs    = ["0.0.0.0"]
    , cfgListenPorts    = [53]
    }