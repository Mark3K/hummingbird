{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

import Control.Lens ((.~), (^.), set)
import Control.Monad.Logger (LogLevel(..), LogSource, LoggingT (runLoggingT), filterLogger, defaultOutput)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Data.IP (IP(..))
import Data.Maybe (fromMaybe)
import Network.Socket (PortNumber)
import Text.RawString.QQ

import HummingBird.Config

configYaml :: ByteString
configYaml = [r|
log:
  level: info
  file: hummingbird.log

listen_addr: 127.0.0.1
listen_port: 1053

enable_tcp: false

upstreams:
  - 114.114.114.114
  - 8.8.8.8

refuse_any: true
|]

main :: IO ()
main = do
    config <- Y.decodeThrow configYaml
    print (config :: Config)
