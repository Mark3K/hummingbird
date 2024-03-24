{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module HummingBird.Server 
    ( runServer
    , runServerT
    ) where

import qualified Data.Map as Map

import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM (TVar, newTVarIO, TChan, newTChanIO, atomically, writeTChan, readTChan)
import Control.Monad.Base (MonadBase)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Logger.CallStack (logInfo, logDebug, logError)
import Control.Monad.Reader (ReaderT (runReaderT), MonadIO (liftIO), MonadReader, asks)
import Control.Monad.Trans.Control 
import Control.Monad.Trans.Class

import Data.Text (pack)

import Network.DNS

import HummingBird.Config
import HummingBird.Downstream 
import HummingBird.Event 
import HummingBird.ServerError 
import HummingBird.Types 
import HummingBird.Upstream 


data ServerEnv = ServerEnv
    { envConfig     :: Config
    , envQueries    :: TVar (Map.Map Identifier DNSMessage)
    , envEventCh    :: TChan Event
    }

newtype ServerT m a = ServerT { unServerT :: ReaderT ServerEnv m a }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader ServerEnv, MonadLogger)

deriving instance (MonadBase IO m) => MonadBase IO (ServerT m)
deriving instance (MonadBaseControl IO m) => MonadBaseControl IO (ServerT m)

instance MonadTrans ServerT where
    lift = ServerT . lift

runServerT :: ServerEnv -> ServerT m a -> m a 
runServerT env s = runReaderT (unServerT s) env

runServer :: 
    ( MonadIO m
    , MonadLogger m
    , MonadBaseControl IO m
    , Upstream m, Downstream m
    )
    => Config
    -> m ()
runServer cfg = do
    logInfo $ pack "start humming bird server"
    env' <- liftIO $ initializeEnv cfg
    case env' of
        Left    e -> do
            logError $ pack ("error initialize server env: " <> show e)
            pure ()
        Right env -> do
            _ <- fork $ do
                rv <- listen (envEventCh env)
                case rv of
                    Left  e -> logError $ pack ("listener exited unexpected: " <> show e)
                    Right _ -> logInfo $ pack "listener exited normally"
            runServerT env runEventLoop

runEventLoop ::
    ( MonadIO m
    , MonadLogger m
    , Upstream m
    )
    => ServerT m ()
runEventLoop = do
    event <- liftIO . atomically . readTChan =<< asks envEventCh
    case event of
        RequestEvent ctx -> do
            logDebug $ pack ("[Event] message: " <> show (requestContextMessage ctx))
            response <- lift $ proxy (requestContextMessage ctx)
            case response of
                Left  err -> do
                    logError $ pack ("error proxy message to upstream " <> show err)

                Right msg -> do
                    logDebug $ pack ("response message: " <> show msg)
                    liftIO $ atomically $ writeTChan (requestContextChannel ctx) (ResponseContext msg (requestContextAddr ctx))

        TimeoutEvent systime timeout -> do
            logDebug $ pack ("[Event] timeout at " <> show systime <> ": " <> show timeout)

        ListenerExit reason -> case reason of
            Just  e -> do
                logError $ pack ("Listener exited unexpected: " <> show e)
                pure ()
            Nothing -> do
                logDebug $ pack "Listener exited normally"
                pure ()
    -- Continue
    runEventLoop

initializeEnv :: Config -> IO (Either ServerError ServerEnv)
initializeEnv cfg = do
    queries <- newTVarIO Map.empty
    eventCh <- newTChanIO
    pure $ Right $ ServerEnv
        { envConfig         = cfg
        , envQueries        = queries
        , envEventCh        = eventCh
        }