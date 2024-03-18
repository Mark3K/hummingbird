{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE RecordWildCards    #-}

module Main (main) where

import Control.Monad.Logger (LogLevel(..))
import Data.Maybe (fromMaybe)
import Options.Applicative

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

buildConfig :: Params -> Config
buildConfig Params{..} = defaultConfig
    { cfgLogLevel   = fromMaybe (cfgLogLevel defaultConfig) (verboseToLogLevel verbose)
    , cfgLogOutput  = if logFile /= "" then FileOutput logFile else Stdout 
    }

run :: Params -> IO ()
run params = do
    env' <- initializeEnv cfg
    case env' of
        Left  err -> error $ show err
        Right env -> runServer env humming
    where
        cfg = buildConfig params

main :: IO ()
main = run =<< execParser opts
    where
        opts = info (params <**> helper)
                    (  fullDesc 
                    <> progDesc "HummingBird" 
                    <> header "HummingBird - A simple dns proxy implemented in Haskell"
                    )
