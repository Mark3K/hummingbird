{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Lens ((.~), (^.))
import Control.Monad.Except (ExceptT,runExceptT)
import Control.Monad.Reader (ReaderT,runReaderT)
import Control.Monad.Logger (LogLevel(..), LoggingT (runLoggingT), defaultOutput)

import Data.Maybe (fromMaybe, isNothing)

import Options.Applicative
import System.IO (withFile, IOMode (AppendMode), hSetBuffering, BufferMode (LineBuffering), stdout)

import HummingBird
import Params

verboseToLogLevel :: Int -> Maybe LogLevel
verboseToLogLevel 0 = Just LevelError
verboseToLogLevel 1 = Just LevelWarn
verboseToLogLevel 2 = Just LevelInfo
verboseToLogLevel 3 = Just LevelDebug
verboseToLogLevel _ = Nothing

buildConfig :: Params -> IO Config
buildConfig Params{..} = do
    cfg <- loadConfig configPath
    pure $ cfgLogLevel  .~ logLevel cfg
         $ cfgLogOutput .~ logOutput 
         $ cfgUpstreams .~ upstreams
         $ cfgRefuseAny .~ refuseAny
         $ setip $ setport cfg
    where
        logLevel cfg    = fromMaybe (cfg ^. cfgLogLevel) (verboseToLogLevel verbose)
        logOutput       = maybe Stdout FileOutput logFile
        setip           = case listenAddr of
            Nothing  -> id
            Just  ip -> cfgListenAddr .~ show ip
        setport         = case listenPort of
            Nothing   -> id
            Just port -> cfgListenPort .~ show port

loadConfig :: Maybe FilePath -> IO Config
loadConfig Nothing  = pure defaultConfig
loadConfig (Just _) = pure defaultConfig -- TODO

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