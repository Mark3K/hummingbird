{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE RecordWildCards            #-}

module HummingBird.App 
    ( app
    , buildAppEnv
    , AppEnv(..)
    , AppError(..)
    , appEnvConfig
    ) where

import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM (newTChanIO, TChan, atomically, readTChan, writeTChan)
import Control.Exception.Lifted (catch, Exception)
import Control.Lens (makeClassy, makeClassyPrisms, view, (#))
import Control.Monad (forever)
import Control.Monad.Except (MonadError(throwError))
import Control.Monad.Logger.CallStack (logInfo, logDebug, logError)
import Control.Monad.Reader (MonadIO (liftIO))

import Data.IORef (IORef, atomicModifyIORef')
import Data.Text (pack)

import qualified Network.DNS as DNS


import HummingBird.Config
import HummingBird.Event
import HummingBird.Upstream
import HummingBird.Server
import HummingBird.Types

data AppError 
    = AppServerError    ServerError
    | AppUpstreamError  UpstreamError
    | AppConfigError    String
    deriving (Show, Eq)
makeClassyPrisms ''AppError

instance Exception AppError

data AppEnv = AppEnv 
    { _appEnvConfig         :: Config 
    , _appEnvUpstream       :: UpstreamEnv
    , _appEnvServer         :: ServerEnv
    }
makeClassy ''AppEnv

buildAppEnv :: (MonadIO m) => Config -> m (Either AppError AppEnv)
buildAppEnv config = do
    upstream' <- buildUpstreamEnv config
    case upstream' of
        Left         e -> pure $ Left $ _AppUpstreamError # e
        Right upstream -> do
            server' <- buildServerEnv config
            case server' of
                Left e -> pure $ Left $ _AppServerError # e
                Right server -> pure $ Right $ AppEnv 
                    { _appEnvConfig     = config 
                    , _appEnvUpstream   = upstream
                    , _appEnvServer     = server
                    }

instance HasServerEnv AppEnv where
    serverEnv = appEnv . appEnvServer

instance AsServerError AppError where
    _ServerError = _AppServerError . _ServerError

instance HasUpstreamEnv AppEnv where
    upstreamEnv = appEnv . appEnvUpstream

instance AsUpstreamError AppError where
    _UpstreamError = _AppUpstreamError . _UpstreamError

type AppProvision c e m = (HasAppEnv c, AsAppError e, ServerProvision c e m, UpstreamProvision c e m)

app :: AppProvision c e m => m ()
app = do
    logInfo "initialize server enviroment"
    initializeUpstreamsEnv
    logInfo "start humming bird server"
    ch  <- liftIO newTChanIO
    serve ch
    logInfo "start event loop"
    forever $ eventLoop ch

eventLoop :: AppProvision c e m => TChan Event -> m ()
eventLoop ch = do
    event <- liftIO $ atomically $ readTChan ch
    case event of
         RequestEvent ctx -> do
            tid <- fork (handleRequest ctx)
            logDebug $ pack ("start thread " <> show tid <> " to handle request")
         -- TODO
         TimeoutEvent systime timeout -> undefined
         ListenerExit reason -> undefined

handleRequest :: AppProvision c e m => RequestContext -> m ()
handleRequest RequestContext{..} = do
    logDebug $ pack ("handle request event: " <> show requestContextMessage)
    resp <- catch (handle requestContextMessage) (onerror requestContextMessage)
    rc   <- case requestContextAddr of
        Nothing     -> pure $ ResponseTcp $ TcpResponse resp
        Just addr   -> pure $ ResponseUdp $ UdpResponse resp addr
    liftIO $ atomically $ writeTChan requestContextChannel rc
    where
        handle r = preprocess r >>= process >>= postprocess

        onerror DNS.DNSMessage{DNS.header = hd, DNS.question = q} err = do
            logError $ pack ("error resolve request " <> show (DNS.identifier hd) <> ": " <> show err)
            pure DNS.defaultResponse 
                { DNS.header    = (DNS.header DNS.defaultResponse) 
                    { DNS.identifier = DNS.identifier hd
                    , DNS.flags = (DNS.flags $ DNS.header DNS.defaultResponse) { DNS.rcode = rcode err }
                    }
                , DNS.question  = q
                }

        rcode err = case err of
            UpstreamDnsError e          -> rcode' e
            UpstreamUnsupportError _    -> DNS.FormatErr
            _                           -> DNS.ServFail
            
        rcode' err = case err of
            DNS.FormatError         -> DNS.FormatErr
            DNS.ServerFailure       -> DNS.ServFail
            _                       -> DNS.BadRCODE

preprocess :: AppProvision c e m => DNS.DNSMessage -> m DNS.DNSMessage
preprocess r@DNS.DNSMessage{ DNS.question = qs } = do
    refuseAny <- view (appEnvConfig . configRefuseAny)
    if refuseAny
        then    pure r { DNS.question = filter nonany qs }
        else    pure r
    
    where nonany DNS.Question { DNS.qtype = typ } = typ /= DNS.ANY

process :: AppProvision c e m => DNS.DNSMessage -> m DNS.DNSMessage
process DNS.DNSMessage{ DNS.header = hd, DNS.question = qs } = case qs of
    [q] -> do
        rc <- resolve q
        pure rc { DNS.header = (DNS.header rc) { DNS.identifier = DNS.identifier hd } }
    _   -> throwError $ _UpstreamUnsupportError # ("invalid question number " <> show (length qs))
    
postprocess :: AppProvision c e m => DNS.DNSMessage -> m DNS.DNSMessage
postprocess = pure
    
initializeUpstreamsEnv :: AppProvision c e m => m ()
initializeUpstreamsEnv = do
    ups     <- view (appEnvConfig . configUpstreams)
    seeds   <- liftIO $ mapM DNS.makeResolvSeed (resolveConfs ups)
    rslvs   <- view (appEnvUpstream . upstreamResolvers)
    liftIO $ mapM_ (`DNS.withResolver` insert rslvs) seeds

    where
        toResolveConf ip mport = case mport of
            Nothing   -> DNS.RCHostName ip
            Just port -> DNS.RCHostPort ip port
        
        resolveConfs ups = 
            [ DNS.defaultResolvConf {DNS.resolvInfo = toResolveConf (show ip) mport} 
            | Upstream (ip, mport) <- ups ]

        insert :: IORef [DNS.Resolver] -> DNS.Resolver -> IO ()
        insert m r = atomicModifyIORef' m (\m' -> (r:m', ()))
