{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Main (main) where

import Control.Exception (try, throwIO, SomeException)
import Control.Lens ((.~), set)

import Katip

import Options.Applicative

import HummingBird

import App
import Params

toVerbosity :: Int -> Verbosity 
toVerbosity 0 = V0
toVerbosity 1 = V1
toVerbosity 2 = V2
toVerbosity _ = V3

buildConfig :: Params -> IO Config
buildConfig Params{..} = do
    ecfg <- maybe (pure $ Right defaultConfig) fromFile configPath
    case ecfg of
        Left  e -> throwIO $ AppConfigError e
        Right c -> pure 
            ( setLogLevel
            $ setLogVerbosity
            $ setLogFile
            $ setUpstreams
            $ setRefuseAny
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
            Just  ip    -> (configServer . serverConfigListenAddr) .~ ip

        setPort         = case listenPort of
            Nothing     -> id
            Just port   -> (configServer . serverConfigListenPort) .~ port

        setRefuseAny    = if refuseAny
            then set configRefuseAny refuseAny
            else id

        setUpstreams    = case length upstreams of
            0           -> id
            _           -> set (configUpstream . upstreamConfigDefaults) [Upstream ip mp | (ip, mp) <- upstreams]

buildEnv :: Params -> IO (Either SomeException AppEnv)
buildEnv vars = buildConfig vars >>= try . buildAppEnv

run :: Params -> IO ()
run vars = do 
    env' <- buildEnv vars
    case env' of
        Left    e -> putStrLn ("error building appenv: " <> show e)
        Right env -> runApp env app

main :: IO ()
main = run =<< execParser opts
    where
        opts = info (params <**> helper)
                    (  fullDesc 
                    <> progDesc "HummingBird" 
                    <> header "HummingBird - A simple dns proxy implemented in Haskell"
                    )
