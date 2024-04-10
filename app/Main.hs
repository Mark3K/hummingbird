{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception (throwIO)
import Control.Lens ((.~), (^.), set)
import Control.Monad.Except (ExceptT,runExceptT)
import Control.Monad.Reader (ReaderT,runReaderT)
import Control.Monad.Logger (LogLevel(..), LogSource, LoggingT (runLoggingT), filterLogger, defaultOutput)

import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)

import Options.Applicative
import System.IO (withFile, IOMode (AppendMode), hSetBuffering, BufferMode (LineBuffering), stdout)

import HummingBird
import Params

verboseToLogLevel :: Int -> LogLevel
verboseToLogLevel 0 = LevelError
verboseToLogLevel 1 = LevelWarn
verboseToLogLevel 2 = LevelInfo
verboseToLogLevel _ = LevelDebug

buildConfig :: Params -> IO (Either AppError Config)
buildConfig Params{..} = do
    ecfg <- maybe (pure $ Right defaultConfig) fromFile configPath
    case ecfg of
        Left  e -> pure $ Left (AppConfigError e)
        Right c -> pure $ Right 
            ( setLogLevel
            $ setLogFile
            $ setUpstreams
            $ setRefuseAny
            $ setEnableTcp
            $ setIP 
            $ setPort c
            )
    where
        setLogLevel = if verbose > 0
            then set (configLog . logConfigLevel) (verboseToLogLevel verbose)
            else id

        setLogFile      = case logFile of
            Nothing     -> id
            Just path   -> set (configLog . logConfigFile) (Just path)

        setIP           = case listenAddr of
            Nothing     -> id
            Just  ip    -> configListenAddr .~ ip

        setPort         = case listenPort of
            Nothing     -> id
            Just port   -> configListenPort .~ port

        setRefuseAny    = if refuseAny
            then set configRefuseAny refuseAny
            else id

        setUpstreams    = case length upstreams of
            0           -> id
            _           -> set configUpstreams [Upstream u | u <- upstreams]

        setEnableTcp    = if enableTcp
            then set configEnableTCP enableTcp
            else id

buildEnv :: Params -> IO (Either AppError AppEnv)
buildEnv vars = do
    cfg' <- buildConfig vars
    case cfg' of
        Left e -> pure $ Left e
        Right cfg -> buildAppEnv cfg

run :: Params -> IO ()
run vars = do 
    env' <- buildEnv vars
    case env' of
        Left    e -> putStrLn ("error building appenv: " <> show e)
        Right env -> do
            rv <- runWithAppEnv env
            case rv of
                Left  e -> putStrLn ("error run hummingbird: " <> show e)
                Right _ -> pure ()

runWithAppEnv :: AppEnv -> IO (Either AppError ())
runWithAppEnv env = case logfile of
    Just   path -> withFile path AppendMode $ \h -> 
        hSetBuffering h LineBuffering >> 
        runLoggingT (filterLogger withLogLevel (runApp env app)) (defaultOutput h)

    Nothing     -> 
        runLoggingT (filterLogger withLogLevel (runApp env app)) (defaultOutput stdout)
    where
        runApp  :: AppEnv 
                -> ExceptT AppError (ReaderT AppEnv (LoggingT IO)) a 
                -> LoggingT IO (Either AppError a)
        runApp ev = flip runReaderT ev . runExceptT

        withLogLevel :: LogSource -> LogLevel -> Bool
        withLogLevel _ = isLogLevelValid loglevel

        loglevel = env ^. appEnvConfig . configLog . logConfigLevel
        logfile  = env ^. appEnvConfig . configLog . logConfigFile

isLogLevelValid :: LogLevel -> LogLevel -> Bool
isLogLevelValid setting target = case setting of
    LevelDebug      -> True
    LevelInfo       -> target /= LevelDebug
    LevelWarn       -> target `notElem` [LevelDebug, LevelInfo]
    LevelError      -> target `notElem` [LevelDebug, LevelInfo, LevelWarn]
    LevelOther    _ -> False

main :: IO ()
main = run =<< execParser opts
    where
        opts = info (params <**> helper)
                    (  fullDesc 
                    <> progDesc "HummingBird" 
                    <> header "HummingBird - A simple dns proxy implemented in Haskell"
                    )