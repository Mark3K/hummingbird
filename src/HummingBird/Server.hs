{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE InstanceSigs #-}

module HummingBird.Server where

import qualified Data.Map as Map

import Control.Concurrent.STM (TVar, newTVarIO, TChan, newTChanIO, atomically, writeTChan)
import Control.Monad.Reader (ReaderT (runReaderT), MonadIO, MonadReader, asks)
import Control.Monad.Logger (LoggingT (runLoggingT), MonadLogger, defaultOutput)
import Control.Monad.Logger.CallStack (logInfo, logDebug)

import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Serialize as S
import Data.Text (pack)

import Network.DNS (DNSMessage, Identifier, decode)
import qualified Network.Simple.TCP as NS

import System.IO (stdout, IOMode (AppendMode), hSetBuffering, BufferMode (LineBuffering), withFile)

import HummingBird.Config (Config (cfgLogOutput, cfgListenAddrs, cfgListenPorts), LogOutput(..))
import HummingBird.ServerError (ServerError)
import HummingBird.Event (Event (MessageEvent))


data ServerEnv = ServerEnv
    { envConfig     :: Config
    , envQueries    :: TVar (Map.Map Identifier DNSMessage)
    , envEventCh    :: TChan Event
    }

newtype Server a = Server { unServer :: ReaderT ServerEnv (LoggingT IO) a }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader ServerEnv, MonadLogger)

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
    cfg <- asks envConfig
    logDebug $ pack ("configuration: " <> show cfg)
    serve
    pure ()

serve :: Server ()
serve = do
    addrs   <- asks $ cfgListenAddrs . envConfig
    ports   <- asks $ cfgListenPorts . envConfig
    echan   <- asks envEventCh
    mapM_ (serve' echan) [(a, show p) | a <- addrs, p <- ports]
    where
        serve' ch (addr, port) = do
            logInfo $ pack ("serving on " <> addr <> ":" <> port)
            NS.serve (NS.Host addr) port $ \(sock, remoteAddr) -> do
                message' <- receive sock
                case message' of
                    Nothing  -> error "Failed to receive dns message"
                    Just msg -> atomically $ writeTChan ch (MessageEvent msg)

instance S.Serialize DNSMessage where
    get :: S.Get DNSMessage
    get = undefined

    put :: S.Putter DNSMessage
    put = undefined

receive :: S.Serialize a => NS.Socket -> IO (Maybe a)
receive sock = do
    rv <- go $ S.runGetPartial S.get
    case rv of
        S.Fail {}       -> pure Nothing
        S.Done v _      -> pure $ Just v
        S.Partial {}    -> pure Nothing
    where
        go getPartial = do
            bytes <- fromMaybe BS.empty <$> NS.recv sock 4096
            case getPartial bytes of
                S.Partial next -> go next
                x              -> pure x

initializeEnv :: Config -> IO (Either ServerError ServerEnv)
initializeEnv cfg = do
    queries <- newTVarIO Map.empty
    eventCh <- newTChanIO
    pure $ Right $ ServerEnv
        { envConfig         = cfg
        , envQueries        = queries
        , envEventCh        = eventCh
        }