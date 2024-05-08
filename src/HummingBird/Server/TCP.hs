{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}

module HummingBird.Server.TCP where

import Control.Concurrent.Async.Lifted (async, waitEitherCatch)
import Control.Concurrent.Lifted (forkFinally)
import Control.Concurrent.STM ( TChan, atomically, writeTChan, newEmptyTMVarIO, takeTMVar, TMVar, STM, orElse)
import Control.Exception.Lifted (SomeException(..), try, Exception (fromException))
import Control.Lens (makeClassy, view, (^.))
import Control.Monad (forever, when)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Map (Map)
import qualified Data.Map as Map

import GHC.IO.Exception (IOException(ioe_type), IOErrorType (EOF))

import Katip (KatipContext, Severity(..), logTM, showLS)

import qualified Network.DNS as DNS
import Network.Socket 

import System.Timeout.Lifted (timeout)

import HummingBird.Event
import HummingBird.Types
import HummingBird.Config
import HummingBird.Server.Common

data TcpServerError 
    = NoAddrAvailable (HostName, ServiceName)
    | TcpSocketError String
    deriving (Show, Eq)

data Response 
    = OK DNS.DNSMessage
    | Dead

data TcpServerEnv = TcpServerEnv
    { _tcpServerEnvHost     :: HostName
    , _tcpServerEnvPort     :: ServiceName
    , _tcpServerEnvTimeout  :: Int
    } deriving (Eq)
makeClassy ''TcpServerEnv

type TcpServerProvision c e m =
    ( MonadIO m
    , MonadThrow m
    , KatipContext m
    , MonadBaseControl IO m
    , MonadReader c m, HasTcpServerEnv c
    )

buildTcpServerEnv :: (MonadThrow m) => ServerConfig -> m TcpServerEnv
buildTcpServerEnv config = do
    pure $ TcpServerEnv
        { _tcpServerEnvHost     = show $ config ^. serverConfigListenAddr
        , _tcpServerEnvPort     = show $ config ^. serverConfigListenPort
        , _tcpServerEnvTimeout  =        config ^. serverConfigTcpTimeout
        }

serve :: TcpServerProvision c e m => TChan Event -> m ()
serve ch = do
    host <- view tcpServerEnvHost
    port <- view tcpServerEnvPort
    rv   <- openSock Stream host port
    serveOnSock ch rv
            
serveOnSock :: TcpServerProvision c e m => TChan Event -> (Socket, SockAddr) -> m ()
serveOnSock ch (sock, addr) = do
    _ <- liftIO $ setSocketOption sock ReuseAddr 1
    _ <- liftIO $ listen sock 5
    $(logTM) DebugS ("TCP serve at " <> showLS addr)
    forever $ do
        (clientSock, clientAddr) <- liftIO $ accept sock
        tid <- forkFinally (serveClient clientSock ch mempty) (\rv -> do
            closeSock clientSock
            case rv of
                Left  e -> $(logTM) ErrorS ("error handle client " <> showLS clientAddr <> ": " <> showLS e)
                Right _ -> pure ()
            )
        $(logTM) DebugS ("new tcp client " <> showLS clientAddr <> " handled by thread " <> showLS tid)

serveClient :: TcpServerProvision c e m  => Socket -> TChan Event -> Map DNS.Identifier (TMVar TcpResponse) -> m ()
serveClient sock ch responses
    | Map.null responses    = do
        message <- try receive
        handleRequest message

    | otherwise             = do
        v0 <- async receive
        v1 <- async $ liftIO $ atomically $ takeAnyTMVar (fromList $ Map.elems responses)
        va <- waitEitherCatch v0 v1
        case va of
            Left  a -> handleRequest a
            Right b -> handleResponse b
    where
        receive = do
            tt <- view tcpServerEnvTimeout
            timeout (tt * 10^6) $ liftIO $ DNS.receiveVC sock

        handleRequest message = case message of
            Left e -> case fromException e of
                Just (DNS.NetworkFailure ioe) -> if ioe_type ioe == EOF
                    then $(logTM) DebugS ("connection " <> showLS sock <> " closed")
                    else $(logTM) ErrorS ("network failure: " <> showLS e)
                _ -> do
                    $(logTM) ErrorS ("unexpected socket exception: " <> showLS e)
            Right (msg :: Maybe DNS.DNSMessage) -> for_ msg onMessage
        
        onMessage message = when (DNS.qOrR (DNS.flags $ DNS.header message) == DNS.QR_Query) $ do
            responseVar  <- liftIO newEmptyTMVarIO
            liftIO $ atomically $ writeTChan ch (RequestEvent $ RequestTcp $ TcpRequest message responseVar)
            serveClient sock ch (Map.insert (DNS.identifier . DNS.header $ message) responseVar responses)

        handleResponse response = case response of
            Left (SomeException e) -> do
                $(logTM) WarningS ("unexpected exception: " <> showLS e)
                pure()
            Right TcpResponse {tcpResponseMessage = msg} -> do
                $(logTM) DebugS ("TCP server response: " <> showLS msg)
                liftIO $ DNS.sendVC sock $ DNS.encode msg
                serveClient sock ch (Map.delete (DNS.identifier . DNS.header $ msg) responses)
            
takeAnyTMVar :: NonEmpty (TMVar a) -> STM a
takeAnyTMVar = foldr1 orElse . fmap takeTMVar