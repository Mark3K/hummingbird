{-# OPTIONS_GHC -Wno-orphans    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}

module HummingBird.Config 
    ( Upstream (..)

    , LogConfig (..)
    -- LogConfig accessors
    , logConfigFile
    , logConfigLevel
    , logConfigVerbosity

    , UpstreamConfig (..)
    -- UpstreamConfig accessors
    , upstreamConfigDefaults
    , upstreamConfigFiles
    , upstreamConfigConcurrent
    , upstreamConfigCache

    -- CacheConfig accessors
    , cacheConfigEnable
    , cacheConfigMaxTTL
    , cacheConfigMinTTL
    , cacheConfigMaxSize

    , ServerConfig (..)
    -- ServerConfig accessors
    , serverConfigListenAddr
    , serverConfigListenPort
    , serverConfigTcpTimeout

    , Config (..)
    -- Config accessors
    , configLog
    , configServer
    , configRefuseAny
    , configUpstream
    -- utils function
    , fromFile
    , defaultConfig
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

data CacheConfig = CacheConfig 
    { _cacheConfigEnable    :: Bool
    , _cacheConfigMaxTTL    :: Int
    , _cacheConfigMinTTL    :: Int
    , _cacheConfigMaxSize   :: Int
    } deriving (Show, Eq)
makeLenses ''CacheConfig

defaultCacheConfig :: CacheConfig
defaultCacheConfig = CacheConfig
    { _cacheConfigEnable    = True
    , _cacheConfigMaxTTL    = 300
    , _cacheConfigMinTTL    = 30
    , _cacheConfigMaxSize   = 32
    }

instance FromJSON CacheConfig where
    parseJSON (Y.Object v) = do
        enable      <- parse cacheConfigEnable  "enable"
        maxTTL      <- parse cacheConfigMaxTTL  "max_ttl"
        minTTL      <- parse cacheConfigMinTTL  "min_ttl"
        maxSize     <- parse cacheConfigMaxSize "max_size"
        pure $ CacheConfig enable maxTTL minTTL maxSize
        where
            parse lens key = fromMaybe (defaultCacheConfig ^. lens) <$> (v .:? key)
    parseJSON _ = mempty

data UpstreamConfig = UpstreamConfig
    { _upstreamConfigDefaults       :: [Upstream]
    , _upstreamConfigFiles          :: [FilePath]
    , _upstreamConfigConcurrent     :: Bool
    , _upstreamConfigCache          :: CacheConfig
    } deriving (Show, Eq)
makeLenses ''UpstreamConfig

defaultUpstreamConfig :: UpstreamConfig
defaultUpstreamConfig = UpstreamConfig
    { _upstreamConfigDefaults       = []
    , _upstreamConfigFiles          = []
    , _upstreamConfigConcurrent     = False
    , _upstreamConfigCache          = defaultCacheConfig
    }

instance FromJSON UpstreamConfig where
    parseJSON (Y.Object v) = do
        defaults    <- parse upstreamConfigDefaults     "defaults"
        files       <- parse upstreamConfigFiles        "files"
        concur      <- parse upstreamConfigConcurrent   "concurrent"
        cache       <- parse upstreamConfigCache        "cache"
        pure $ UpstreamConfig defaults files concur cache
        where
            parse lens key = fromMaybe (defaultUpstreamConfig ^. lens) <$> (v .:? key)
    parseJSON _ = mempty

data ServerConfig = ServerConfig
    { _serverConfigListenAddr   :: IP
    , _serverConfigListenPort   :: PortNumber
    , _serverConfigTcpTimeout   :: Int
    } deriving (Show, Eq)
makeLenses ''ServerConfig

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
    { _serverConfigListenAddr = "127.0.0.1"
    , _serverConfigListenPort = 53
    , _serverConfigTcpTimeout = 3
    }

instance FromJSON ServerConfig where
    parseJSON (Y.Object v) = do
        addr        <- parse serverConfigListenAddr "listen_addr"
        port        <- parse serverConfigListenPort "listen_port"
        timeout     <- parse serverConfigTcpTimeout "tcp_timeout"
        pure $ ServerConfig addr port timeout
        where
            parse lens key = fromMaybe (defaultServerConfig ^. lens) <$> (v .:? key)
    parseJSON _ = mempty

data Config = Config 
    { _configLog            :: LogConfig
    , _configServer         :: ServerConfig
    , _configRefuseAny      :: Bool
    , _configUpstream       :: UpstreamConfig
    } deriving (Show, Eq)
makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config 
    { _configLog            = defaultLogConfig
    , _configServer         = defaultServerConfig
    , _configRefuseAny      = False
    , _configUpstream       = defaultUpstreamConfig
    }

instance FromJSON Config where
    parseJSON (Y.Object v) = do
        logs        <- fromMaybe (defaultConfig ^. configLog)        <$> v .:? "log"
        server      <- fromMaybe (defaultConfig ^. configServer)     <$> v .:? "server"
        refuseAny   <- fromMaybe (defaultConfig ^. configRefuseAny)  <$> v .:? "refuse_any"
        upstream    <- fromMaybe (defaultConfig ^. configUpstream)   <$> v .:? "upstream"
        pure $ Config logs server refuseAny upstream
    parseJSON _            = mempty

fromFile :: FilePath -> IO (Either String Config)
fromFile path = first show <$> Y.decodeFileEither path