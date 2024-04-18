{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Main (main) where

import Control.Lens ((.~), (^.), set)
import Katip
import Options.Applicative

import HummingBird
import Params

toVerbosity :: Int -> Verbosity 
toVerbosity 0 = V0
toVerbosity 1 = V1
toVerbosity 2 = V2
toVerbosity _ = V3

buildConfig :: Params -> IO (Either AppError Config)
buildConfig Params{..} = do
    ecfg <- maybe (pure $ Right defaultConfig) fromFile configPath
    case ecfg of
        Left  e -> pure $ Left (AppConfigError e)
        Right c -> pure $ Right 
            ( setLogLevel
            $ setLogVerbosity
            $ setLogFile
            $ setUpstreams
            $ setRefuseAny
            $ setEnableTcp
            $ setIP 
            $ setPort c
            )
    where
        setLogLevel     = case logLevel of
            Nothing     -> id
            Just v      -> set (configLog . logConfigLevel) v

        setLogVerbosity = if verbose > 0
            then set (configLog . logConfigVerbosity) (toVerbosity verbose)
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
            _           -> set (configUpstream . upstreamConfigDefaults) [Upstream ip mp | (ip, mp) <- upstreams]

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
            rv <- runApp env app
            case rv of
                Left  e -> putStrLn ("error run hummingbird: " <> show e)
                Right _ -> pure ()

main :: IO ()
main = run =<< execParser opts
    where
        opts = info (params <**> helper)
                    (  fullDesc 
                    <> progDesc "HummingBird" 
                    <> header "HummingBird - A simple dns proxy implemented in Haskell"
                    )
