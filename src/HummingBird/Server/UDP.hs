{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module HummingBird.Server.UDP where

import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM (TChan, newTChanIO, atomically, writeTChan, readTChan)
import Control.Exception.Lifted (SomeException, catch, Exception)
import Control.Lens (makeClassy, view, (^.))
import Control.Monad (forever)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Control (MonadBaseControl)

import Katip (KatipContext, Severity(..), logTM, showLS)
import qualified Network.DNS as DNS
import Network.Socket
import Network.Socket.ByteString (recvFrom, sendAllTo)

import HummingBird.Event
import HummingBird.Types
import HummingBird.Config 
import HummingBird.Server.Common

data UdpServerError 
    = NoAddrAvailable (HostName, ServiceName)
    | UdpSocketError String
    deriving (Show, Eq)

instance Exception UdpServerError

data UdpServerEnv = UdpServerEnv 
    { _udpServerEnvHost :: HostName
    , _udpServerEnvPort :: ServiceName
    } deriving (Show, Eq)
makeClassy ''UdpServerEnv

type UdpServerProvision c e m = 
    ( MonadIO m
    , MonadThrow m
    , KatipContext m
    , MonadBaseControl IO m
    , MonadReader c m, HasUdpServerEnv c
    )

buildUdpServerEnv :: (MonadThrow m) => ServerConfig -> m UdpServerEnv
buildUdpServerEnv config = pure $ UdpServerEnv
    { _udpServerEnvHost = show $ config ^. serverConfigListenAddr
    , _udpServerEnvPort = show $ config ^. serverConfigListenPort
    }

serve :: UdpServerProvision c e m => TChan Event -> m ()
serve ch = do
    host <- view udpServerEnvHost
    port <- view udpServerEnvPort

    (sock, addr) <- openSock Datagram host port
    $(logTM) DebugS ("UDP serve at " <> showLS addr)
    respCh <- liftIO newTChanIO
    _ <- fork (forever $ sender sock respCh)
    forever $ catch (process sock respCh) 
                    (\(se :: SomeException) -> do
                        $(logTM) ErrorS ("unexpected error: " <> showLS se)
                        throwM (UdpSocketError (show se))
                        )
    where
        process sock sch = do
            (raw, addr) <- liftIO $ recvFrom sock (fromIntegral DNS.maxUdpSize)
            $(logTM) DebugS ("raw message received: " <> showLS addr)
            case parse raw of
                Left  err -> do
                    $(logTM) ErrorS ("dns error: " <> showLS err)
                Right msg -> do
                    $(logTM) DebugS ("UDP DNS message: " <> showLS msg)
                    liftIO $ atomically $ writeTChan ch (RequestEvent $ RequestUdp $ UdpRequest msg addr sch)
            
        sender sock sch = do
            UdpResponse {..} <- liftIO $ atomically $ readTChan sch
            $(logTM) DebugS ("UDP server response: " <> showLS udpResponseMessage)
            -- TODO: check message length and set TC bit
            liftIO $ sendAllTo sock (DNS.encode udpResponseMessage) udpResponseAddr

        parse raw = do
            m <- DNS.decode raw
            if DNS.qOrR (DNS.flags $ DNS.header m) == DNS.QR_Query
                then pure m
                else Left DNS.FormatError
