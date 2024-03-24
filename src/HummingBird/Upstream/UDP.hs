{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module HummingBird.Upstream.UDP
    ( module HummingBird.Upstream
    , UdpUpstreamT (..)
    , UdpUpstreamContext (..)
    , runUdpUpstreamT
    ) where

import Control.Exception (Exception, bracket, SomeException, catch)
import Control.Monad.Base (MonadBase)
import Control.Monad.Reader (ReaderT (runReaderT), MonadIO (liftIO), MonadReader, asks, MonadTrans (lift))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Text (Text)

import Network.Socket hiding (listen)
import Network.Socket.ByteString (sendAll)
import qualified Network.DNS as DNS (encode, receive)

import System.Timeout (timeout)

import HummingBird.Upstream
import HummingBird.Downstream.UDP (Downstream (listen, DownstreamException))

data UdpUpstreamContext = UdpUpstreamContext
    { udpUpstreamContextHostName    :: HostName
    , udpUpstreamContextServiceName :: ServiceName
    , udpUpstreamContextTimeout     :: Int
    } deriving (Show)

newtype UdpUpstreamException = UdpUpstreamException Text deriving Show

instance Exception UdpUpstreamException

newtype UdpUpstreamT m a = UdpUpstreamT
    { unUdpUpstreamT :: ReaderT UdpUpstreamContext m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader UdpUpstreamContext, MonadTrans)

deriving instance (MonadLogger m) => MonadLogger (UdpUpstreamT m)
deriving instance (MonadBase IO m) => MonadBase IO (UdpUpstreamT m)
deriving instance (MonadBaseControl IO m) => MonadBaseControl IO (UdpUpstreamT m)

instance (Downstream m) => Downstream (UdpUpstreamT m) where
    type DownstreamException (UdpUpstreamT m) = DownstreamException m
    listen = lift . listen

runUdpUpstreamT :: UdpUpstreamContext -> UdpUpstreamT m a -> m a
runUdpUpstreamT ctx m = runReaderT (unUdpUpstreamT m) ctx

instance (MonadIO m) => Upstream (UdpUpstreamT m) where
    type UpstreamException (UdpUpstreamT m) = UdpUpstreamException
    proxy request = do
        host <- asks udpUpstreamContextHostName
        port <- asks udpUpstreamContextServiceName
        tt   <- asks udpUpstreamContextTimeout

        addrInfos <- liftIO $ getAddrInfo 
                        (Just defaultHints {addrFlags = [AI_PASSIVE]})
                        (Just host)
                        (Just port)

        case addrInfos of
            []      -> pure $ Left $ UdpUpstreamException "no address available"
            (ai:_)  -> do
                rv <- liftIO $ timeout (tt*1000) $ bracket (dial ai) hangup $ \sock -> do
                    sendAll sock (DNS.encode request)
                    DNS.receive sock

                case rv of
                    Nothing     -> pure $ Left $ UdpUpstreamException "udp proxy timeout"
                    Just msg    -> pure $ Right msg
        where
            dial :: AddrInfo -> IO Socket
            dial ai = do
                sock <- socket (addrFamily ai) Datagram (addrProtocol ai)
                connect sock (addrAddress ai)
                pure sock
            
            hangup :: Socket -> IO ()
            hangup sock = do
                catch (close sock) (\(_ :: SomeException) -> pure ())
