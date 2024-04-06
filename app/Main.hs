{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Lens ((.~), (^.))
import Control.Monad.Except (ExceptT,runExceptT)
import Control.Monad.Reader (ReaderT,runReaderT)
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
    pure $ cfgLogLevel .~ logLevel cfg
         $ cfgLogOutput .~ logOutput 
         $ cfg
    where
        logLevel cfg    = fromMaybe (cfg ^. cfgLogLevel) (verboseToLogLevel verbose)
        logOutput       = if logFile /= "" then FileOutput logFile else Stdout

loadConfig :: FilePath -> IO Config
loadConfig path = pure defaultConfig

run :: Params -> IO ()
run vars = do
    rv <- run' vars
    case rv of
        Left e  -> putStrLn ("AppError: " <> show e)
        Right _ -> pure ()

run' :: Params -> IO (Either AppError ())
run' vars = do
    cfg <- buildConfig vars
    env <- (appEnvConfig .~ cfg) <$> defaultAppEnv
    case env ^. appEnvConfig . cfgLogOutput of
        FileOutput fp -> withFile fp AppendMode $ \h -> 
            hSetBuffering h LineBuffering >> runLoggingT (runApp env app) (defaultOutput h)
        Stdout        -> runLoggingT (runApp env app) (defaultOutput stdout)

    where
        runApp  :: AppEnv 
                -> ExceptT AppError (ReaderT AppEnv (LoggingT IO)) a 
                -> LoggingT IO (Either AppError a)
        runApp env = flip runReaderT env . runExceptT

main :: IO ()
main = run =<< execParser opts
    where
        opts = info (params <**> helper)
                    (  fullDesc 
                    <> progDesc "HummingBird" 
                    <> header "HummingBird - A simple dns proxy implemented in Haskell"
                    )