{-# OPTIONS_GHC -Wno-orphans    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}

module HummingBird.Config 
    ( defaultConfig
    , Config (..)
    , LogConfig (..)
    , Upstream (..)
    -- LogConfig accessors
    , logConfigFile
    , logConfigLevel
    , logConfigVerbosity
    -- UpstreamConfig accessors
    , upstreamConfigDefaults
    , upstreamConfigFiles
    , upstreamConfigConcurrent
    -- Config accessors
    , configLog
    , configListenAddr
    , configListenPort
    , configEnableTCP
    , configRefuseAny
    , configUpstream
    -- utils function
    , fromFile
    ) where

import Control.Lens (makeLenses, (^.))

import Data.Bifunctor (first)
import Data.IP (IP)
import qualified Data.Yaml as Y
import Data.Scientific (toBoundedInteger)
import Data.Text (unpack, split)
import Data.Maybe (fromMaybe)
import Data.Yaml (FromJSON(..), (.:?))

import Katip (Severity(..), Verbosity(..))
import Network.Socket (PortNumber)
import Text.Read (readEither)

import HummingBird.Types

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
        [ip]        -> (`Upstream` Nothing) <$> parseJSON (Y.String ip)
        [ip, port]  -> do
            i <- parseJSON (Y.String ip)
            p <- parseJSON (Y.String port)
            pure $ Upstream i (Just p)
        xs           -> fail ("Invalid End Point: " <> show xs)
    parseJSON _          = mempty

data LogConfig = LogConfig
    { _logConfigLevel       :: Severity
    , _logConfigVerbosity   :: Verbosity
    , _logConfigFile        :: Maybe FilePath
    } deriving (Show, Eq)
makeLenses ''LogConfig

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig
    { _logConfigLevel       = InfoS
    , _logConfigVerbosity   = V0
    , _logConfigFile        = Nothing
    }

instance FromJSON LogConfig where
    parseJSON (Y.Object v) = do
        level <- fromMaybe (defaultLogConfig ^. logConfigLevel)     <$> (v .:?  "level")
        verb  <- fromMaybe (defaultLogConfig ^. logConfigVerbosity) <$> (v .:? "verbosity")
        file  <- v .:?  "file"
        pure $ LogConfig level verb file
    parseJSON           _  = mempty

data UpstreamConfig = UpstreamConfig
    { _upstreamConfigDefaults       :: [Upstream]
    , _upstreamConfigFiles          :: [FilePath]
    , _upstreamConfigConcurrent     :: Bool
    , _upstreamConfigCacheEnable    :: Bool
    , _upstreamConfigCacheTTL       :: Int
    , _upstreamConfigCacheSize      :: Int
    } deriving (Show, Eq)
makeLenses ''UpstreamConfig

defaultUpstreamConfig :: UpstreamConfig
defaultUpstreamConfig = UpstreamConfig
    { _upstreamConfigDefaults       = []
    , _upstreamConfigFiles          = []
    , _upstreamConfigConcurrent     = False
    , _upstreamConfigCacheEnable    = True
    , _upstreamConfigCacheTTL       = 30
    , _upstreamConfigCacheSize      = 32
    }

instance FromJSON UpstreamConfig where
    parseJSON (Y.Object v) = do
        defaults    <- parse upstreamConfigDefaults     "defaults"
        files       <- parse upstreamConfigFiles        "files"
        concur      <- parse upstreamConfigConcurrent   "concurrent"
        cache       <- parse upstreamConfigCacheEnable  "cache_enable"
        ttl         <- parse upstreamConfigCacheTTL     "cache_ttl"
        size        <- parse upstreamConfigCacheSize    "cache_size"
        pure $ UpstreamConfig defaults files concur cache ttl size
        where
            parse lens key = fromMaybe (defaultUpstreamConfig ^. lens) <$> (v .:? key)
    parseJSON _ = mempty

data Config = Config 
    { _configLog            :: LogConfig
    , _configListenAddr     :: IP
    , _configListenPort     :: PortNumber
    , _configEnableTCP      :: Bool
    , _configRefuseAny      :: Bool
    , _configUpstream       :: UpstreamConfig
    } deriving (Show, Eq)
makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config 
    { _configLog            = defaultLogConfig
    , _configListenAddr     = "127.0.0.1"
    , _configListenPort     = 53
    , _configEnableTCP      = True
    , _configRefuseAny      = False
    , _configUpstream       = defaultUpstreamConfig
    }

instance FromJSON Config where
    parseJSON (Y.Object v) = do
        logs        <- fromMaybe (defaultConfig ^. configLog)        <$> v .:? "log"
        addr        <- fromMaybe (defaultConfig ^. configListenAddr) <$> v .:? "listen_addr"
        port        <- fromMaybe (defaultConfig ^. configListenPort) <$> v .:? "listen_port"
        enableTCP   <- fromMaybe (defaultConfig ^. configEnableTCP)  <$> v .:? "enable_tcp"
        refuseAny   <- fromMaybe (defaultConfig ^. configRefuseAny)  <$> v .:? "refuse_any"
        upstream    <- fromMaybe (defaultConfig ^. configUpstream)   <$> v .:? "upstream"
        pure $ Config 
            logs
            addr
            port
            enableTCP
            refuseAny
            upstream
    parseJSON _            = fail "Expected Object for Config value"

fromFile :: FilePath -> IO (Either String Config)
fromFile path = first show <$> Y.decodeFileEither path