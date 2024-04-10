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
import Control.Exception.Lifted (SomeException, catch, bracketOnError)
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
import HummingBird.Config (Config, configListenAddr, configListenPort)

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

    (sock, addr) <- bindSock host port
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

bindSock :: UdpServerProvision c e m => HostName -> ServiceName -> m (Socket, SockAddr)
bindSock host port = do
    addrs <- liftIO $ getAddrInfo (Just hints) (Just host) (Just port)
    logDebug $ pack ("UDP available address: " <> show addrs)
    tryAddrs addrs
    where
        hints :: AddrInfo
        hints = defaultHints 
            { addrFlags         = [AI_PASSIVE, AI_NUMERICHOST, AI_NUMERICSERV]
            , addrSocketType    = Datagram 
            }

        tryAddrs :: (MonadIO m, MonadBaseControl IO m, MonadError e m, AsUdpServerError e) 
                 => [AddrInfo] -> m (Socket, SockAddr)
        tryAddrs = \case
            []      -> throwError $ _NoAddrAvailable # (host, port)
            [x]     -> useAddr x
            (x:xs)  -> catch (useAddr x) (\(_ :: IOError) -> tryAddrs xs)
        
        useAddr :: (MonadIO m, MonadBaseControl IO m) 
                => AddrInfo -> m (Socket, SockAddr)
        useAddr addr = bracketOnError (newSocket addr) closeSocket $ \sock -> do
            let sockAddr = addrAddress addr
            liftIO (bind sock sockAddr)
            pure (sock, sockAddr)

newSocket :: MonadIO m => AddrInfo -> m Socket
newSocket AddrInfo{..} = do
    liftIO (socket addrFamily addrSocketType addrProtocol)

closeSocket :: MonadIO m => Socket -> m ()
closeSocket sock = liftIO $ do
    catch (close sock) (\(_ :: SomeException) -> pure ())