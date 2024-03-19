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

import Control.Concurrent (forkIO)
import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM (TVar, newTVarIO, TChan, newTChanIO, atomically, writeTChan, readTChan)
import Control.Exception (Exception)
import Control.Monad.Base (MonadBase)
import Control.Monad.Logger (LoggingT (runLoggingT), MonadLogger, defaultOutput)
import Control.Monad.Logger.CallStack (logInfo, logDebug, logError)
import Control.Monad.Reader (ReaderT (runReaderT), MonadIO (liftIO), MonadReader, asks)
import Control.Monad.Trans.Control 

import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (pack)

import Network.DNS (DNSMessage, Identifier, decode)
import Network.Socket (HostName, ServiceName)

import System.IO (stdout, IOMode (AppendMode), hSetBuffering, BufferMode (LineBuffering), withFile, hPutStrLn)

import HummingBird.Config (Config (cfgLogOutput, cfgListenAddrs, cfgListenPorts), LogOutput(..))
import HummingBird.ServerError (ServerError)
import HummingBird.Event (Event (MessageEvent))
import qualified HummingBird.UDP as UDP


data ServerEnv = ServerEnv
    { envConfig     :: Config
    , envQueries    :: TVar (Map.Map Identifier DNSMessage)
    , envEventCh    :: TChan Event
    }

newtype Server a = Server { unServer :: ReaderT ServerEnv (LoggingT IO) a }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader ServerEnv, MonadLogger)

deriving instance MonadBase IO Server
deriving instance MonadBaseControl IO Server

runServer :: ServerEnv -> Server a -> IO a 
runServer env server = do
    case cfgLogOutput cfg of
        FileOutput fp -> withFile fp AppendMode $ \h -> 
            hSetBuffering h LineBuffering >> runLoggingT action (defaultOutput h)
        Stdout        -> runLoggingT action (defaultOutput stdout)
    where
        cfg     = envConfig env
        action  = runReaderT (unServer server) env

humming :: Server ()
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
            logDebug $ pack ("new event: " <> show event)
            loop

serve :: [HostName] -> [ServiceName] -> TChan Event -> Server ()
serve addrs ports ch = mapM_ (fork . serve' ch) [(a, p) | a <- addrs, p <- ports]
    where
        serve' :: TChan Event -> (HostName, ServiceName) -> Server () 
        serve' ch' (addr, port) = do
            logInfo $ pack ("serving on " <> addr <> ":" <> port)
            UDP.serve addr port 1024 $ \(raw, remoteAddr) -> do
                putStrLn ("new message from " <> show remoteAddr)
                case decode raw of
                    Left  err -> error $ "Failed to receive dns message" <> show err
                    Right msg -> atomically $ writeTChan ch' (MessageEvent msg)

initializeEnv :: Config -> IO (Either ServerError ServerEnv)
initializeEnv cfg = do
    queries <- newTVarIO Map.empty
    eventCh <- newTChanIO
    pure $ Right $ ServerEnv
        { envConfig         = cfg
        , envQueries        = queries
        , envEventCh        = eventCh
        }