{-# OPTIONS_GHC -Wno-orphans    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}

module HummingBird.Config 
    ( defaultConfig
    , Config (..)
    , LogConfig (..)
    , Upstream (..)
    -- LogConfig accessor
    , logConfigFile
    , logConfigLevel
    -- Config accessor
    , configLog
    , configListenAddr
    , configListenPort
    , configEnableTCP
    , configUpstreams
    , configRefuseAny
    -- utils function
    , fromFile
    , withDefault
    ) where

import Control.Lens (makeLenses)
import Control.Monad.Logger (LogLevel(..))

import Data.Bifunctor (bimap)
import Data.IP (IP)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Data.Text (unpack, split)

import Network.Socket (PortNumber)

import Text.Read (readEither)

newtype Upstream = Upstream (IP, Maybe PortNumber) deriving (Show, Eq)

data LogConfig = LogConfig
    { _logConfigLevel :: LogLevel
    , _logConfigFile  :: Maybe FilePath
    } deriving (Show, Eq)
makeLenses ''LogConfig

instance FromJSON LogLevel where
    parseJSON (Y.String v) = case v of
        "debug" -> pure LevelDebug
        "info"  -> pure LevelInfo
        "warn"  -> pure LevelWarn
        "error" -> pure LevelError
        _       -> fail ("Invalid log level: " <> unpack v)
    parseJSON _ = mempty

instance FromJSON LogConfig where
    parseJSON (Y.Object v) = do
        level <- v .: "level"
        file  <- v .: "file"
        pure $ LogConfig level file 
    parseJSON           _  = mempty

data Config = Config 
    { _configLog           :: LogConfig
    , _configListenAddr    :: String
    , _configListenPort    :: String
    , _configEnableTCP     :: Bool
    , _configUpstreams     :: [Upstream]
    , _configRefuseAny     :: Bool
    } deriving (Show, Eq)
makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config 
    { _configLog           = LogConfig LevelError Nothing
    , _configListenAddr    = "127.0.0.1"
    , _configListenPort    = "domain"
    , _configEnableTCP     = True
    , _configUpstreams     = []
    , _configRefuseAny     = False
    }

instance FromJSON Config where
    parseJSON (Y.Object v) = do
        logs        <- v .: "log"
        addr        <- v .: "listen_addr"
        port        <- v .: "listen_port"
        enableTCP   <- v .: "enable_tcp"
        upstreams   <- v .: "upstreams"
        refuseAny   <- v .: "refuse_any"
        pure $ Config 
            logs
            addr
            port
            enableTCP
            upstreams
            refuseAny

    parseJSON _            = fail "Expected Object for Config value"

instance FromJSON IP where
    parseJSON (Y.String v) = case readEither $ unpack v of
        Left  e -> fail ("Invalid IP Address " <> unpack v <> ": " <> show e)
        Right i -> pure i
    parseJSON _          = mempty

instance FromJSON PortNumber where
    parseJSON (Y.String v) = case readEither (unpack v) of
        Left  e -> fail ("Invalid Port Number: " <> unpack v <> ": " <> show e)
        Right i -> pure i
    parseJSON _          = mempty

instance FromJSON Upstream where
    parseJSON (Y.String v) = case split (==':') v of
        [ip]        -> Upstream . (, Nothing) <$> parseJSON (Y.String ip)
        [ip, port]  -> do
            i <- parseJSON (Y.String ip)
            p <- parseJSON (Y.String port)
            pure $ Upstream (i, Just p)
        xs           -> fail ("Invalid End Point: " <> show xs)
    parseJSON _          = mempty

fromFile :: FilePath -> IO (Either String Config)
fromFile path = bimap show withDefault <$> Y.decodeFileEither path

withDefault :: Config -> Config
withDefault = id -- TODO: merge with default