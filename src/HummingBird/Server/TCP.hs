{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module HummingBird.Server.TCP where

import Control.Concurrent.Lifted (forkFinally, fork)
import Control.Exception.Lifted (SomeException(..), catch)
import Control.Concurrent.STM ( TChan, atomically, writeTChan, newEmptyTMVarIO, takeTMVar)
import Control.Lens (makeClassyPrisms, makeClassy, view, (^.), (#))
import Control.Monad (forever, when)
import Control.Monad.Except (MonadError(throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Bifunctor (bimap)

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
makeClassyPrisms ''TcpServerError

data Response 
    = OK DNS.DNSMessage
    | Dead

data TcpServerEnv = TcpServerEnv
    { _tcpServerEnvHost     :: HostName
    , _tcpServerEnvPort     :: ServiceName
    } deriving (Eq)
makeClassy ''TcpServerEnv

type TcpServerProvision c e m =
    ( MonadIO m
    , KatipContext m
    , MonadBaseControl IO m
    , MonadReader c m, HasTcpServerEnv c
    , MonadError e m, AsTcpServerError e
    )

buildTcpServerEnv :: (MonadIO m) => Config -> m (Either TcpServerError TcpServerEnv)
buildTcpServerEnv config = do
    pure $ Right $ TcpServerEnv
        { _tcpServerEnvHost     = show $ config ^. configListenAddr
        , _tcpServerEnvPort     = show $ config ^. configListenPort
        }

serve :: TcpServerProvision c e m => TChan Event -> m ()
serve ch = do
    host <- view tcpServerEnvHost
    port <- view tcpServerEnvPort
    rv   <- openSock Stream host port
    case bimap (_TcpSocketError #) (serveOnSock ch) rv of
        Left  e -> throwError e
        Right v -> v
            
serveOnSock :: TcpServerProvision c e m => TChan Event -> (Socket, SockAddr) -> m ()
serveOnSock ch (sock, addr) = do
    _ <- liftIO $ setSocketOption sock ReuseAddr 1
    _ <- liftIO $ listen sock 5
    $(logTM) DebugS ("TCP serve at " <> showLS addr)
    forever $ do
        (clientSock, clientAddr) <- liftIO $ accept sock
        tid <- forkFinally (serveClient clientSock ch) (\rv -> do
            closeSock clientSock
            case rv of
                Left  e -> $(logTM) ErrorS ("error handle client " <> showLS clientAddr <> ": " <> showLS e)
                Right _ -> pure ()
            )
        $(logTM) DebugS ("new tcp client " <> showLS clientAddr <> " handled by thread " <> showLS tid)

-- TODO: 
-- 1) support timeout
-- 2) support query pipelining
serveClient :: TcpServerProvision c e m  => Socket -> TChan Event -> m ()
serveClient sock ch = do
    message <- liftIO $ DNS.receiveVC sock
    when (DNS.qOrR (DNS.flags $ DNS.header message) == DNS.QR_Query) $ do
        tid <- fork $ handleQuery sock ch message `catch` (\(SomeException e) -> do
            $(logTM) ErrorS ("error handle query " <> showLS (rid message) <> ": " <> showLS e) 
            )
        $(logTM) InfoS ("start thread " <> showLS tid <> " to handle query " <> showLS (rid message))
    where rid = DNS.identifier . DNS.header

handleQuery :: TcpServerProvision c e m  => Socket -> TChan Event -> DNS.DNSMessage -> m ()
handleQuery sock ch message = do
    $(logTM) DebugS ("TCP DNS message: " <> showLS message)
    responseVar  <- liftIO newEmptyTMVarIO
    liftIO $ atomically $ writeTChan ch (RequestEvent $ RequestTcp $ TcpRequest message responseVar)
    TcpResponse{..} <- liftIO $ atomically $ takeTMVar responseVar
    $(logTM) DebugS ("TCP server response: " <> showLS tcpResponseMessage)
    liftIO $ DNS.sendVC sock $ DNS.encode tcpResponseMessage