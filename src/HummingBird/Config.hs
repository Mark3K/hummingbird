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
    ) where

import Control.Lens (makeLenses, (^.))
import Control.Monad.Logger (LogLevel(..))

import Data.Bifunctor (first)
import Data.IP (IP)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:?))
import Data.Scientific (toBoundedInteger)
import Data.Text (unpack, split)

import Network.Socket (PortNumber)

import Text.Read (readEither)
import Data.Maybe (fromMaybe)

newtype Upstream = Upstream (IP, Maybe PortNumber) deriving (Show, Eq)

data LogConfig = LogConfig
    { _logConfigLevel :: LogLevel
    , _logConfigFile  :: Maybe FilePath
    } deriving (Show, Eq)
makeLenses ''LogConfig

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig
    { _logConfigLevel   = LevelInfo
    , _logConfigFile    = Nothing
    }

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
        level <- fromMaybe (defaultLogConfig ^. logConfigLevel) <$> (v .:?  "level")
        file  <- v .:?  "file"
        pure $ LogConfig level file
    parseJSON           _  = mempty

data Config = Config 
    { _configLog           :: LogConfig
    , _configListenAddr    :: IP
    , _configListenPort    :: PortNumber
    , _configEnableTCP     :: Bool
    , _configUpstreams     :: [Upstream]
    , _configRefuseAny     :: Bool
    } deriving (Show, Eq)
makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config 
    { _configLog           = defaultLogConfig
    , _configListenAddr    = "127.0.0.1"
    , _configListenPort    = 53
    , _configEnableTCP     = True
    , _configUpstreams     = []
    , _configRefuseAny     = False
    }

instance FromJSON Config where
    parseJSON (Y.Object v) = do
        logs        <- fromMaybe defaultLogConfig <$> v .:? "log"
        addr        <- fromMaybe (defaultConfig ^. configListenAddr) <$> v .:? "listen_addr"
        port        <- fromMaybe (defaultConfig ^. configListenPort) <$> v .:? "listen_port"
        enableTCP   <- fromMaybe (defaultConfig ^. configEnableTCP)  <$> v .:? "enable_tcp"
        upstreams   <- fromMaybe (defaultConfig ^. configUpstreams)  <$> v .:? "upstreams"
        refuseAny   <- fromMaybe (defaultConfig ^. configRefuseAny)  <$> v .:? "refuse_any"
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
    parseJSON (Y.Number v) = case toBoundedInteger v of
        Nothing -> fail ("Invalid Port Number: " <> show v)
        Just  n -> pure n
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
fromFile path = first show <$> Y.decodeFileEither path