{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module HummingBird.Server.UDP where

import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM (TChan, newTChanIO, atomically, writeTChan, readTChan)
import Control.Exception.Lifted (SomeException, catch)
import Control.Lens (makeClassy, makeClassyPrisms, view, (^.), (#))
import Control.Monad (forever)
import Control.Monad.Except (MonadError(throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger.CallStack
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Text (pack)

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
makeClassyPrisms ''UdpServerError

data UdpServerEnv = UdpServerEnv 
    { _udpServerEnvHost :: HostName
    , _udpServerEnvPort :: ServiceName
    } deriving (Show, Eq)
makeClassy ''UdpServerEnv

type UdpServerProvision c e m = 
    ( MonadIO m
    , MonadLogger m
    , MonadBaseControl IO m
    , MonadReader c m, HasUdpServerEnv c
    , MonadError e m, AsUdpServerError e
    )

buildUdpServerEnv :: (Applicative m) => Config -> m (Either UdpServerError UdpServerEnv)
buildUdpServerEnv config = pure $ Right $ UdpServerEnv
    { _udpServerEnvHost = show $ config ^. configListenAddr
    , _udpServerEnvPort = show $ config ^. configListenPort
    }

serve :: UdpServerProvision c e m => TChan Event -> m ()
serve ch = do
    host <- view udpServerEnvHost
    port <- view udpServerEnvPort

    rv   <- openSock Datagram host port
    case rv of
        Left             s -> throwError $ _UdpSocketError # s
        Right (sock, addr) -> do
            logDebug $ pack ("UDP serve at " <> show addr)
            respCh <- liftIO newTChanIO
            _ <- fork (forever $ sender sock respCh)
            forever $ catch (process sock respCh) 
                            (\(se :: SomeException) -> do
                                logError $ pack ("unexpected error: " <> show se)
                                throwError (_UdpSocketError # show se)
                                )
    where
        process sock sch = do
            (raw, addr) <- liftIO $ recvFrom sock (fromIntegral DNS.maxUdpSize)
            logDebug $ pack ("raw message received: " <> show addr)
            case parse raw of
                Left  err -> do
                    logError $ pack ("dns error: " <> show err)
                Right msg -> do
                    logDebug $ pack ("UDP DNS message: " <> show msg)
                    liftIO $ atomically $ writeTChan ch (RequestEvent $ RequestContext msg (Just addr) sch)
            
        sender sock sch = do
            resp <- liftIO $ atomically $ readTChan sch
            case resp of
                ResponseTcp               _ -> logWarn $ pack "UDP server received TCP response"
                ResponseUdp UdpResponse{..} -> do
                    logDebug $ pack ("UDP server response: " <> show urMessage)
                    liftIO $ sendAllTo sock (DNS.encode urMessage) urAddr

        parse raw = do
            m <- DNS.decode raw
            if DNS.qOrR (DNS.flags $ DNS.header m) == DNS.QR_Query
                then pure m
                else Left DNS.FormatError
