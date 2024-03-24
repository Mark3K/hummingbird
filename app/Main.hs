{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE RecordWildCards    #-}

module Main (main) where

import Control.Monad.Logger (LogLevel(..), LoggingT (runLoggingT), defaultOutput)
import Data.Maybe (fromMaybe)
import Options.Applicative

import System.IO (withFile, IOMode (AppendMode), hSetBuffering, BufferMode (LineBuffering), stdout)

import HummingBird

data Params = Params
    { configPath    :: String
    , logFile       :: String
    , verbose       :: Int
    , version       :: Bool
    } deriving (Show)

params :: Parser Params
params = Params 
    <$> strOption (long "config-path" <> metavar "<PATH>" <> help "The configuration file path" <> showDefault <> value "")
    <*> strOption (long "log-file" <> metavar "<PATH>" <> help "The log file path" <> showDefault <> value "")
    <*> (length <$> many (flag' () (short 'v' <> help "verborsity")))
    <*> switch (long "version" <> help "Show the program verion")

verboseToLogLevel :: Int -> Maybe LogLevel
verboseToLogLevel 0 = Just LevelError
verboseToLogLevel 1 = Just LevelWarn
verboseToLogLevel 2 = Just LevelInfo
verboseToLogLevel 3 = Just LevelDebug
verboseToLogLevel _ = Nothing

buildConfig :: Params -> IO Config
buildConfig Params{..} = do
    cfg <- loadConfig configPath
    -- TODO: merge defaultConfig with config from file
    pure cfg
        { cfgLogLevel   = fromMaybe (cfgLogLevel defaultConfig) (verboseToLogLevel verbose)
        , cfgLogOutput  = if logFile /= "" then FileOutput logFile else Stdout 
        }

loadConfig :: FilePath -> IO Config
loadConfig fp = pure defaultConfig

run :: Params -> IO ()
run vars = do
    cfg  <- buildConfig vars
    case cfgLogOutput cfg of
        FileOutput fp -> withFile fp AppendMode $ \h -> 
            hSetBuffering h LineBuffering >> runLoggingT (runUdpDownstreamT downstreamContext (runUdpUpstreamT upstreamContext (runServer cfg))) (defaultOutput h)
        Stdout        -> runLoggingT (runUdpDownstreamT downstreamContext (runUdpUpstreamT upstreamContext (runServer cfg))) (defaultOutput stdout)

    where
        upstreamContext = UdpUpstreamContext 
            { udpUpstreamContextHostName    = "114.114.114.114" 
            , udpUpstreamContextServiceName = "domain"
            , udpUpstreamContextTimeout     = 3000 
            }

        downstreamContext = UdpDownstreamContext
            { udpDownstreamContextHostName      = "127.0.0.1"
            , udpDownstreamContextServiceName   = "domain"
            , udpDownstreamContextBufferSize    = 1024
            }

main :: IO ()
main = run =<< execParser opts
    where
        opts = info (params <**> helper)
                    (  fullDesc 
                    <> progDesc "HummingBird" 
                    <> header "HummingBird - A simple dns proxy implemented in Haskell"
                    )
