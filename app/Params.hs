module Params where

import Data.IP (IP(..))
import Network.DNS (Domain)
import Network.Socket (PortNumber)
import Options.Applicative
import Text.Read (readEither)

data Params = Params
    { configPath    :: Maybe String
    , logFile       :: Maybe String
    , listenAddr    :: Maybe IP
    , listenPort    :: Maybe PortNumber
    , upstreams     :: [(IP, Maybe PortNumber)]
    , refuseAny     :: Bool
    , enableTcp     :: Bool
    , noResolv      :: Bool
    , verbose       :: Int
    , version       :: Bool
    } deriving (Show)

params :: Parser Params
params = Params 
    <$> optional configPathParser
    <*> optional logFileParser
    <*> optional listenAddrParser
    <*> optional listenPortParser
    <*> upstreamsParser
    <*> refuseAnyParser
    <*> enableTcpParser
    <*> noResolvParser
    <*> (length <$> many (flag' () (short 'v' <> help "Verbose output (-v|-vv|-vvv)")))
    <*> switch (long "version" <> help "Show the program verion")

configPathParser :: Parser String
configPathParser = strOption 
    (  short 'C'
    <> long "config-path" 
    <> metavar "<PATH>" 
    <> help "The configuration file path" 
    )

logFileParser :: Parser String
logFileParser = strOption 
    (  long "log-file" 
    <> metavar "<PATH>" 
    <> help "The log file path" 
    )

listenAddrParser :: Parser IP
listenAddrParser = readopt "Invalid IP Address"
    (  short 'l' 
    <> long "listen" 
    <> metavar "<IP>"
    <> help "Listening address"
    )

listenPortParser :: Parser PortNumber
listenPortParser = readopt "Invalid Port Number"
    (  short 'p' 
    <> long "port" 
    <> metavar "<PORT>"
    <> help "Listening port"
    )

upstreamsParser :: Parser [(IP, Maybe PortNumber)]
upstreamsParser = many $ upstreamOption 
    (  short 'u' 
    <> long "up-stream" 
    <> metavar "<IP:Port>" 
    <> help "An upstream to be used (can be specified multiple times)"
    )

upstreamOption :: Mod OptionFields (IP, Maybe PortNumber) -> Parser (IP, Maybe PortNumber)
upstreamOption = option $ eitherReader $ \s -> case split ':' s of
    [x]     -> case readEither x of
        Left  _ -> Left "Invalid IP Address"
        Right v -> pure (v, Nothing)

    [x,y]   -> case readEither x of
        Left   _ -> Left "Invalid IP Address"
        Right v0 -> case readEither y of
            Left   _ -> Left "Invalid Port Number"
            Right v1 -> pure (v0, v1)

    _       -> Left "Upstream should be (ip | ip:port)"

    where
        split :: Char -> String -> [String]
        split sep xs = case break (==sep) xs of
            (s, "")     -> [s]
            (s, _:rs)   -> s : split sep rs

refuseAnyParser :: Parser Bool
refuseAnyParser = switch (long "refuse-any" <> help "If specified, refuse ANY requests")

enableTcpParser :: Parser Bool
enableTcpParser = switch (long "enable-tcp" <> help "If specified, enable TCP server")

noResolvParser  :: Parser Bool
noResolvParser  = switch (long "no-resolv"  <> help "")

addressParser   :: Parser ([Domain], IP)
addressParser = undefined

readopt :: Read a => String -> Mod OptionFields a -> Parser a
readopt msg = option $ eitherReader $ \s -> case readEither s of
    Left  _ -> Left msg
    Right v -> pure v