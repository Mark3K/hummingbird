{-# LANGUAGE TemplateHaskell #-}

module HummingBird.Config 
    ( defaultConfig
    , LogOutput (..)
    , Config (..)
    , cfgLogLevel
    , cfgLogOutput
    , cfgListenAddr
    , cfgListenPort
    , cfgEnableTCP
    , cfgUpstreams
    , cfgRefuseAny
    ) where

import Control.Lens (makeLenses)
import Control.Monad.Logger (LogLevel(..))

import Data.IP (IP)
import Network.Socket (PortNumber)

data LogOutput = FileOutput FilePath
               | Stdout
               deriving (Show)

data Config = Config 
    { _cfgLogLevel      :: LogLevel
    , _cfgLogOutput     :: LogOutput
    , _cfgListenAddr    :: String
    , _cfgListenPort    :: String
    , _cfgEnableTCP     :: Bool
    , _cfgUpstreams     :: [(IP, Maybe PortNumber)]
    , _cfgRefuseAny     :: Bool
    } deriving (Show)
makeLenses ''Config


defaultConfig :: Config
defaultConfig = Config 
    { _cfgLogLevel      = LevelError
    , _cfgLogOutput     = Stdout
    , _cfgListenAddr    = "127.0.0.1"
    , _cfgListenPort    = "domain"
    , _cfgEnableTCP     = True
    , _cfgUpstreams     = []
    , _cfgRefuseAny     = False
    }
