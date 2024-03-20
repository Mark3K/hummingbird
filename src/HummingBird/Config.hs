module HummingBird.Config 
    ( defaultConfig
    , LogOutput (..)
    , Config (..)
    ) where

import Control.Monad.Logger (LogLevel(..))

data LogOutput = FileOutput FilePath
               | Stdout
               deriving (Show)

data Config = Config 
    { cfgLogLevel       :: LogLevel
    , cfgLogOutput      :: LogOutput
    , cfgListenAddrs    :: [String]
    , cfgListenPorts    :: [String]
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config 
    { cfgLogLevel       = LevelInfo
    , cfgLogOutput      = Stdout
    , cfgListenAddrs    = ["127.0.0.1"]
    , cfgListenPorts    = ["domain"]
    }