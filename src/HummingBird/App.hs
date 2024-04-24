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
    , runApp
    , buildAppEnv

    , appEnvConfig
    , appEnvLogger

    , loggerEnv
    , loggerContexts
    , loggerNamespace

    , AppT(..)
    , AppEnv(..)
    , AppError(..)
    ) where

import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM (newTChanIO, TChan, atomically, readTChan, writeTChan)
import Control.Exception.Lifted (Exception, catch, SomeException)
import Control.Lens (makeClassy, makeClassyPrisms, view, over, (^.), (#))
import Control.Monad (forever)
import Control.Monad.Base (MonadBase (liftBase), liftBaseDefault)
import Control.Monad.Except (MonadError(throwError, catchError), ExceptT, runExceptT)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (local), ReaderT (runReaderT), MonadTrans(lift))
import Control.Monad.Trans.Control
    ( defaultLiftBaseWith
    , defaultLiftWith2
    , defaultRestoreM
    , defaultRestoreT2
    , ComposeSt
    , MonadTransControl(..)
    , MonadBaseControl(..)
    )

import Data.Bifunctor (bimap)
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
    , Katip(..)
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
import Data.Time (getCurrentTime, diffUTCTime)

data AppError 
    = AppServerError    ServerError
    | AppUpstreamError  UpstreamError
    | AppConfigError    String
    deriving (Show, Eq)
makeClassyPrisms ''AppError

instance Exception AppError

data Logger = Logger 
    { _loggerEnv        :: LogEnv
    , _loggerContexts   :: LogContexts
    , _loggerNamespace  :: Namespace
    }
makeClassy ''Logger

buildLogger :: (MonadIO m) => LogConfig -> m Logger
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

buildAppEnv :: (MonadIO m) => Config -> m (Either AppError AppEnv)
buildAppEnv config = do
    logEnv   <- buildLogger         (config ^. configLog)
    upstream <- buildUpstreamEnv    (config ^. configUpstream)
    server   <- buildServerEnv      config

    pure $ bimap (_AppUpstreamError #) (build logEnv) upstream 
         >>= (\b -> bimap (_AppServerError #) b server)
    where
        build l u s = AppEnv
            { _appEnvConfig     = config
            , _appEnvUpstream   = u
            , _appEnvServer     = s
            , _appEnvLogger     = l
            }

instance HasServerEnv AppEnv where
    serverEnv = appEnv . appEnvServer

instance AsServerError AppError where
    _ServerError = _AppServerError . _ServerError

instance HasUpstreamEnv AppEnv where
    upstreamEnv = appEnv . appEnvUpstream

instance AsUpstreamError AppError where
    _UpstreamError = _AppUpstreamError . _UpstreamError

type AppProvision m = 
    ( MonadIO m
    , KatipContext m
    , MonadBaseControl IO m
    , MonadReader AppEnv m 
    , MonadError AppError m
    )

app :: AppProvision m =>  m ()
app = do
    $(logTM) InfoS "initialize server enviroment"
    initializeUpstreamsEnv
    $(logTM) InfoS "start humming bird server"
    ch  <- liftIO newTChanIO
    serve ch
    $(logTM) InfoS "start event loop"
    forever $ eventLoop ch `catchError` \case
        AppUpstreamError e -> do
            $(logTM) ErrorS ("upstream error: " <> showLS e)
            pure ()
        e                  -> throwError e

eventLoop :: AppProvision m => TChan Event -> m ()
eventLoop ch = do
    event <- liftIO $ atomically $ readTChan ch
    case event of
         RequestEvent ctx -> do
            tid <- fork (handleRequest ctx)
            $(logTM) InfoS ("start thread " <> showLS tid <> " to handle request")
         -- TODO
         TimeoutEvent systime timeout -> undefined
         ListenerExit reason -> undefined

handleRequest :: AppProvision m => RequestContext -> m ()
handleRequest RequestContext{..} = do
    $(logTM) InfoS ("handle request " <> showLS rid)
    begin <- liftIO getCurrentTime
    resp <- catch (handle requestContextMessage) $ \(e :: SomeException) -> do 
        $(logTM) ErrorS ("unexpected error: " <> showLS e)
        onException requestContextMessage
    rc   <- case requestContextAddr of
        Nothing     -> pure $ ResponseTcp $ TcpResponse resp
        Just addr   -> pure $ ResponseUdp $ UdpResponse resp addr
    liftIO $ atomically $ writeTChan requestContextChannel rc
    end <- liftIO getCurrentTime
    $(logTM) InfoS ("request " <> showLS rid <> " costs " <> showLS (diffUTCTime end begin))
    where
        rid = (DNS.identifier . DNS.header) requestContextMessage
        handle r = preprocess r >>= process >>= postprocess

onException :: MonadIO m => DNS.DNSMessage -> m DNS.DNSMessage
onException DNS.DNSMessage{DNS.header = hd, DNS.question = qs} = 
    pure DNS.defaultResponse 
        { DNS.header    = (DNS.header DNS.defaultResponse) 
            { DNS.identifier = DNS.identifier hd
            , DNS.flags = (DNS.flags $ DNS.header DNS.defaultResponse) { DNS.rcode = DNS.ServFail }
            }
        , DNS.question  = qs
        }
   
preprocess :: (MonadReader AppEnv m) => DNS.DNSMessage -> m DNS.DNSMessage
preprocess r@DNS.DNSMessage{ DNS.question = qs } = do
    refuseAny <- view (appEnvConfig . configRefuseAny)
    if refuseAny
        then    pure r { DNS.question = filter nonany qs }
        else    pure r
    
    where nonany DNS.Question { DNS.qtype = typ } = typ /= DNS.ANY

process :: (KatipContext m, MonadBaseControl IO m, MonadReader AppEnv m, MonadError AppError m) => DNS.DNSMessage -> m DNS.DNSMessage
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
                Left   e -> throwError $ _AppConfigError # ("error parse routes from " <> path <> ": " <> show e)
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

newtype AppT m a = AppT
    { unAppT :: ExceptT AppError (ReaderT AppEnv m) a 
    } deriving (Functor, Applicative, Monad, MonadIO, MonadError AppError, MonadReader AppEnv)

runApp :: AppEnv -> AppT m a -> m (Either AppError a)
runApp env m = runReaderT (runExceptT $ unAppT m) env

instance MonadTrans AppT where
    lift = AppT . lift .lift

instance MonadBase b m => MonadBase b (AppT m) where
    liftBase = liftBaseDefault

instance MonadTransControl AppT where
  type StT AppT a = StT (ExceptT AppError) (StT (ReaderT AppEnv) a)
  liftWith = defaultLiftWith2 AppT unAppT

  restoreT = defaultRestoreT2 AppT

instance MonadBaseControl b m => MonadBaseControl b (AppT m) where
  type StM (AppT m) a = ComposeSt AppT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance (MonadIO m) => Katip (AppT m) where
    getLogEnv = view (appEnvLogger . loggerEnv)

    localLogEnv f (AppT m) = AppT $ local (over (appEnvLogger . loggerEnv) f) m

instance (MonadIO m) => KatipContext (AppT m) where
    getKatipContext = view (appEnvLogger . loggerContexts)

    localKatipContext f (AppT m) = AppT $ local (over (appEnvLogger . loggerContexts) f) m

    getKatipNamespace = view (appEnvLogger . loggerNamespace)

    localKatipNamespace f (AppT m) = AppT $ local (over (appEnvLogger . loggerNamespace) f) m
