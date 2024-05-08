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
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE LambdaCase                 #-}

module HummingBird.App 
    ( app
    , buildAppEnv

    , appEnvConfig
    , appEnvLogger

    , loggerEnv
    , loggerContexts
    , loggerNamespace

    , AppEnv(..)
    , AppError(..)
    ) where

import Control.Concurrent.Lifted (forkFinally)
import Control.Concurrent.STM (newTChanIO, TChan, atomically, readTChan, writeTChan, putTMVar)
import Control.Exception.Lifted (Exception, catch, SomeException (SomeException))
import Control.Lens (makeClassy, view, (^.))
import Control.Monad (forever)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Reader (MonadIO (liftIO), MonadReader)
import Control.Monad.Trans.Control(MonadBaseControl(..))

import Data.IORef (IORef, atomicModifyIORef')

import Katip
    ( defaultScribeSettings
    , initLogEnv
    , permitItem
    , registerScribe
    , showLS
    , logTM
    , mkFileScribe
    , mkHandleScribe
    , Severity(ErrorS, InfoS)
    , LogEnv
    , Namespace
    , KatipContext(..)
    , LogContexts
    , ColorStrategy(ColorIfTerminal)
    )
import qualified Network.DNS as DNS
import System.IO (stdout)

import HummingBird.Config
import HummingBird.Event
import HummingBird.Upstream
import HummingBird.Server
import HummingBird.Types
import HummingBird.Router
import HummingBird.Parser.ServerConfig (routesFromFile)

data AppError 
    = AppServerError    ServerError
    | AppUpstreamError  UpstreamError
    | AppConfigError    String
    deriving (Show, Eq)

instance Exception AppError

data Logger = Logger 
    { _loggerEnv        :: LogEnv
    , _loggerContexts   :: LogContexts
    , _loggerNamespace  :: Namespace
    }
makeClassy ''Logger

buildLogger :: (MonadIO m, MonadThrow m) => LogConfig -> m Logger
buildLogger config = do
    scribe <- liftIO buildScribe
    env    <- liftIO $ do
        ev <- initLogEnv "HummingBird" "production"
        registerScribe "main" scribe defaultScribeSettings ev
    pure $ Logger env mempty "App"
    where
        logLevel = config ^. logConfigLevel
        logVerb  = config ^. logConfigVerbosity
        logFile  = config ^. logConfigFile

        buildScribe = case logFile of
            Just path -> mkFileScribe path (permitItem logLevel) logVerb
            Nothing   -> mkHandleScribe ColorIfTerminal stdout (permitItem logLevel) logVerb

data AppEnv = AppEnv 
    { _appEnvConfig         :: Config 
    , _appEnvUpstream       :: UpstreamEnv
    , _appEnvServer         :: ServerEnv
    , _appEnvLogger         :: Logger
    }
makeClassy ''AppEnv

buildAppEnv :: (MonadIO m, MonadThrow m) => Config -> m AppEnv
buildAppEnv config = do
    logEnv   <- buildLogger         (config ^. configLog)
    upstream <- buildUpstreamEnv    (config ^. configUpstream)
    server   <- buildServerEnv      (config ^. configServer)
    pure $ AppEnv config upstream server logEnv

instance HasServerEnv AppEnv where
    serverEnv = appEnv . appEnvServer

instance HasUpstreamEnv AppEnv where
    upstreamEnv = appEnv . appEnvUpstream

type AppProvision m = 
    ( MonadIO m
    , MonadThrow m
    , KatipContext m
    , MonadReader AppEnv m 
    , MonadBaseControl IO m
    )

app :: AppProvision m =>  m ()
app = do
    $(logTM) InfoS "initialize server enviroment"
    initializeUpstreamsEnv
    $(logTM) InfoS "start humming bird server"
    ch  <- liftIO newTChanIO
    serve ch
    $(logTM) InfoS "start event loop"
    forever $ eventLoop ch

eventLoop :: AppProvision m => TChan Event -> m ()
eventLoop ch = do
    event <- liftIO $ atomically $ readTChan ch
    case event of
         RequestEvent ctx -> do
            tid <- forkFinally (handleRequest ctx) (\case
                Left  e -> $(logTM) ErrorS ("error handle request: " <> showLS (rid ctx) <> ": " <> showLS e)
                Right _ -> pure ()
                )
            $(logTM) InfoS ("start thread " <> showLS tid <> " to handle request")
         -- TODO
         TimeoutEvent systime timeout -> undefined
         ListenerExit reason -> undefined
    where
        rid (RequestTcp TcpRequest{..}) = (DNS.identifier . DNS.header) tcpRequestMessage
        rid (RequestUdp UdpRequest{..}) = (DNS.identifier . DNS.header) udpRequestMessage

handleRequest :: AppProvision m => RequestContext -> m ()
handleRequest (RequestTcp TcpRequest{..}) = do
    $(logTM) InfoS ("tcp request: " <> showLS tcpRequestMessage)
    response <- chainproc tcpRequestMessage `catch` \(SomeException e) -> do
        $(logTM) ErrorS ("tcp handler unexpected error: " <> showLS e)
        onException tcpRequestMessage
    liftIO $ atomically $ putTMVar tcpRequestResponseVar $ TcpResponse response
handleRequest (RequestUdp UdpRequest{..}) = do
    $(logTM) InfoS ("udp request " <> showLS udpRequestMessage)
    response <- chainproc udpRequestMessage `catch` \(SomeException e) -> do 
        $(logTM) ErrorS ("udp handler unexpected error: " <> showLS e)
        onException udpRequestMessage
    liftIO $ atomically $ writeTChan udpRequestResponseCh $ UdpResponse response udpRequestAddr

onException :: MonadIO m => DNS.DNSMessage -> m DNS.DNSMessage
onException DNS.DNSMessage{DNS.header = hd, DNS.question = qs} = 
    pure DNS.defaultResponse 
        { DNS.header    = (DNS.header DNS.defaultResponse) 
            { DNS.identifier = DNS.identifier hd
            , DNS.flags = (DNS.flags $ DNS.header DNS.defaultResponse) { DNS.rcode = DNS.ServFail }
            }
        , DNS.question  = qs
        }
   
chainproc :: (AppProvision m) => DNS.DNSMessage -> m DNS.DNSMessage
chainproc m = preprocess m >>= process >>= postprocess
   
preprocess :: (MonadReader AppEnv m) => DNS.DNSMessage -> m DNS.DNSMessage
preprocess r@DNS.DNSMessage{ DNS.question = qs } = do
    refuseAny <- view (appEnvConfig . configRefuseAny)
    if refuseAny
        then    pure r { DNS.question = filter nonany qs }
        else    pure r
    
    where nonany DNS.Question { DNS.qtype = typ } = typ /= DNS.ANY

process :: AppProvision m => DNS.DNSMessage -> m DNS.DNSMessage
process = resolve

postprocess :: Applicative m => DNS.DNSMessage -> m DNS.DNSMessage
postprocess = pure
    
initializeUpstreamsEnv :: AppProvision m => m ()
initializeUpstreamsEnv = initializeDefaultUpstreams >> initializeUpstreamFromFiles

initializeDefaultUpstreams :: (MonadIO m, MonadReader AppEnv m) => m ()
initializeDefaultUpstreams = do
    upstreams     <- view (appEnvConfig . configUpstream . upstreamConfigDefaults)
    seeds   <- liftIO $ mapM buildSeed upstreams
    router  <- view (appEnvUpstream . upstreamRouter)
    liftIO $ mapM_ (insert' router) seeds
    where
        buildSeed upstream = do
            seed <- DNS.makeResolvSeed $ buildResolveConf upstream
            pure (upstream, seed)

        insert' router (upstream, seed) = insert router (Route [""] upstream, seed)

initializeUpstreamFromFiles :: AppProvision m => m ()
initializeUpstreamFromFiles = do
    files  <- view (appEnvConfig . configUpstream . upstreamConfigFiles)
    routes <- concat <$> mapM readRoutes files
    seeds  <- mapM buildSeeds routes
    router <- view (appEnvUpstream . upstreamRouter)
    liftIO $ mapM_ (insert router) seeds
    where
        readRoutes path = do
            rs' <- liftIO $ routesFromFile path
            case rs' of
                Left   e -> throwM $ AppConfigError ("error parse routes from " <> path <> ": " <> show e)
                Right rs -> pure rs

        buildSeeds r@(Route _ upstream) = 
            liftIO $ (r, ) <$> DNS.makeResolvSeed (buildResolveConf upstream)

buildResolveConf :: Upstream -> DNS.ResolvConf
buildResolveConf (Upstream ip mport) = DNS.defaultResolvConf
    { DNS.resolvInfo = case mport of
        Nothing   -> DNS.RCHostName (show ip)
        Just port -> DNS.RCHostPort (show ip) port
    }

insert :: IORef Router -> (Route, DNS.ResolvSeed) -> IO ()
insert rr (route, seed) = DNS.withResolver seed (\resolver -> 
    atomicModifyIORef' rr (\router -> (insertRoute router route resolver, ())))


