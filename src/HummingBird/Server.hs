{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module HummingBird.Server where

import qualified Data.Map as Map

import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM (TVar, newTVarIO, TChan, newTChanIO, atomically, writeTChan, readTChan)
import Control.Exception (SomeException)
import Control.Monad.Base (MonadBase)
import Control.Monad.Logger (LoggingT (runLoggingT), MonadLogger, defaultOutput)
import Control.Monad.Logger.CallStack (logInfo, logDebug, logError)
import Control.Monad.Reader (ReaderT (runReaderT), MonadIO (liftIO), MonadReader, asks)
import Control.Monad.Trans.Control 

import Data.Text (pack)

import Network.DNS (DNSMessage, Identifier, decode)
import Network.Socket (HostName, ServiceName)

import System.IO (stdout, IOMode (AppendMode), hSetBuffering, BufferMode (LineBuffering), withFile)

import HummingBird.Config (Config (cfgLogOutput, cfgListenAddrs, cfgListenPorts), LogOutput(..))
import HummingBird.ServerError (ServerError)
import HummingBird.Event (Event (MessageEvent, ListenerExit, TimeoutEvent))
import qualified HummingBird.UDP as UDP


data ServerEnv = ServerEnv
    { envConfig     :: Config
    , envQueries    :: TVar (Map.Map Identifier DNSMessage)
    , envEventCh    :: TChan Event
    }

newtype ServerT m a = ServerT { unServerT :: ReaderT ServerEnv m a }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader ServerEnv, MonadLogger)

deriving instance (MonadBase IO m) => MonadBase IO (ServerT m)
deriving instance (MonadBaseControl IO m) => MonadBaseControl IO (ServerT m)

newtype Server a = Server { unServer :: ServerT (LoggingT IO) a }

runServerT :: ServerEnv -> ServerT m a -> m a 
runServerT env s = runReaderT (unServerT s) env

run :: ServerEnv -> IO () 
run env = do
    case cfgLogOutput c of
        FileOutput fp -> withFile fp AppendMode $ \h -> 
            hSetBuffering h LineBuffering >> runLoggingT m (defaultOutput h)
        Stdout        -> runLoggingT m (defaultOutput stdout)
    where
        c = envConfig env
        m = runReaderT (unServerT humming) env

humming :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) => ServerT m ()
humming = do
    logInfo $ pack "start humming bird server"
    addrs   <- asks $ cfgListenAddrs . envConfig
    ports   <- asks $ cfgListenPorts . envConfig
    ch      <- asks envEventCh
    _ <- serve addrs ports ch
    _ <- loop
    pure ()
    where
        loop = do
            event <- liftIO . atomically . readTChan =<< asks envEventCh
            case event of
                MessageEvent message -> do
                    logDebug $ pack ("[Event] message: " <> show message)

                TimeoutEvent systime timeout -> do
                    logDebug $ pack ("[Event] timeout at " <> show systime <> ": " <> show timeout)

                ListenerExit reason -> case reason of
                    Just  e -> do
                        logError $ pack ("Listener exited unexpected: " <> show e)
                        pure ()
                    Nothing -> do
                        logDebug $ pack "Listener exited normally"
                        pure ()
            loop

serve :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) => [HostName] -> [ServiceName] -> TChan Event -> ServerT m ()
serve addrs ports ch = mapM_ (fork . serve') [(a, p) | a <- addrs, p <- ports]
    where
        serve' (addr, port) = do
            logInfo $ pack ("serving on " <> addr <> ":" <> port)
            rv <- UDP.serve addr port 1024 handler
            case rv of
                Left (e :: SomeException) -> liftIO $ atomically $ writeTChan ch (ListenerExit (Just $ show e))
                Right _ -> liftIO $ atomically $ writeTChan ch (ListenerExit Nothing)

        handler (raw, remoteAddr) = do
            putStrLn ("new message from " <> show remoteAddr)
            case decode raw of
                Left  err -> error $ "Failed to receive dns message" <> show err
                Right msg -> atomically $ writeTChan ch (MessageEvent msg)

initializeEnv :: Config -> IO (Either ServerError ServerEnv)
initializeEnv cfg = do
    queries <- newTVarIO Map.empty
    eventCh <- newTChanIO
    pure $ Right $ ServerEnv
        { envConfig         = cfg
        , envQueries        = queries
        , envEventCh        = eventCh
        }