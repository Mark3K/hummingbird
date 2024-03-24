{-# LANGUAGE FlexibleContexts               #-}
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE GeneralisedNewtypeDeriving     #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
{-# LANGUAGE RecordWildCards                #-}
{-# LANGUAGE StandaloneDeriving             #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE TypeFamilies                   #-}
{-# LANGUAGE UndecidableInstances           #-}

module HummingBird.Downstream.UDP 
    ( module HummingBird.Downstream
    , UdpDownstreamT (..)
    , UdpDownstreamContext (..)
    , runUdpDownstreamT
    ) where


import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM (atomically, newTChanIO, writeTChan, readTChan)
import Control.Exception (Exception, SomeException, bracketOnError, try)
import Control.Exception.Lifted (catch)
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT (runReaderT), MonadIO (liftIO), MonadReader, asks, MonadTrans (lift))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Logger.CallStack (logError, logDebug)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Text (Text, pack)

import Network.DNS (decode, encode)
import Network.Socket
import Network.Socket.ByteString (recvFrom, sendAllTo)

import HummingBird.Downstream
import HummingBird.Event 
import HummingBird.Types
import HummingBird.Upstream (Upstream (UpstreamException, proxy))


data UdpDownstreamContext = UdpDownstreamContext
    { udpDownstreamContextHostName      :: HostName
    , udpDownstreamContextServiceName   :: ServiceName
    , udpDownstreamContextBufferSize    :: Int
    } deriving (Show)

newtype UdpDownstreamException = UdpDownstreamException Text deriving Show

instance Exception UdpDownstreamException

newtype UdpDownstreamT m a = UdpDownstreamT
    { unUdpDownstreamT :: ReaderT UdpDownstreamContext m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader UdpDownstreamContext, MonadTrans)

deriving instance (MonadLogger m) => MonadLogger (UdpDownstreamT m)
deriving instance (MonadBase IO m) => MonadBase IO (UdpDownstreamT m)
deriving instance (MonadBaseControl IO m) => MonadBaseControl IO (UdpDownstreamT m)

instance (Upstream m) => Upstream (UdpDownstreamT m) where
    type UpstreamException (UdpDownstreamT m)  = UpstreamException m
    proxy = lift . proxy

runUdpDownstreamT :: UdpDownstreamContext -> UdpDownstreamT m a -> m a
runUdpDownstreamT ctx m = runReaderT (unUdpDownstreamT m) ctx 

instance (MonadIO m, MonadLogger m, MonadBaseControl IO m) => Downstream (UdpDownstreamT m) where
    type DownstreamException (UdpDownstreamT m) = UdpDownstreamException

    listen ch = do
        host <- asks udpDownstreamContextHostName
        port <- asks udpDownstreamContextServiceName
        bufs <- asks udpDownstreamContextBufferSize
        bindv <- liftIO $ try $ bindSock host port
        case bindv of
            Left e -> do
                logError $ pack ("error binding to " <> host <> ":" <> port <> ": " <> show e)
                pure $ Left e
            Right (sock, addr) -> do
                logDebug $ pack ("UDP.downstream listen at " <> show addr)
                sch <- liftIO newTChanIO
                _ <- fork (forever $ sender sock sch)
                forever $ catch (process sock bufs sch) (\(se :: SomeException) -> do 
                    logError $ pack ("error process packet: " <> show se)
                    pure ())
        where
            process sock bufs sch = do
                (raw, addr) <- liftIO $ recvFrom sock bufs
                logDebug $ pack ("new message from UDP.downstream: " <> show addr)
                case decode raw of
                    Left  err -> do
                        logError $ pack ("error decoding dns message: " <> show err)
                    Right msg -> do
                        logDebug $ pack ("decoded message: " <> show msg)
                        liftIO $ atomically $ writeTChan ch (RequestEvent $ RequestContext msg addr sch)
            
            sender sock sch = do
                ctx <- liftIO $ atomically $ readTChan sch
                logDebug $ pack ("UDP.downstream received response: " <> show ctx)
                liftIO $ sendAllTo sock (encode $ responseContextMessage ctx) (responseContextAddr ctx)

bindSock :: MonadIO m => HostName -> ServiceName -> m (Socket, SockAddr)
bindSock host port = liftIO $ do
    addrs <- getAddrInfo (Just hints) (Just host) (Just port)
    tryAddrs addrs
    where
        hints :: AddrInfo
        hints = defaultHints 
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Datagram 
            }

        tryAddrs :: [AddrInfo] -> IO (Socket, SockAddr)
        tryAddrs = \case
            []      -> fail "UDP.bindSock: No addresses available"
            [x]     -> useAddr x
            (x:xs)  -> catch (useAddr x) (\(_ :: IOError) -> tryAddrs xs)
        
        useAddr :: AddrInfo -> IO (Socket, SockAddr)
        useAddr addr = bracketOnError (newSocket addr) closeSocket $ \sock -> do
            let sockAddr = addrAddress addr
            bind sock sockAddr
            pure (sock, sockAddr)

newSocket :: AddrInfo -> IO Socket
newSocket AddrInfo{..} = socket addrFamily addrSocketType addrProtocol

closeSocket :: MonadIO m => Socket -> m ()
closeSocket sock = liftIO $ do
    catch (close sock) (\(_ :: SomeException) -> pure ())