{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module HummingBird.Upstream.UDP
    ( module HummingBird.Upstream
    , UdpUpstreamT (..)
    , UdpUpstreamContext (..)
    , runUdpStreamT
    ) where

import Control.Exception (Exception, bracket)
import Control.Monad.Base (MonadBase)
import Control.Monad.Reader (ReaderT (runReaderT), MonadIO (liftIO), MonadReader, asks)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Text (Text)

import Network.Socket
import Network.Socket.ByteString (sendAll)
import qualified Network.DNS as DNS (encode, receive)

import System.Timeout (timeout)

import HummingBird.Upstream


data UdpUpstreamContext = UdpUpstreamContext
    { udpUpstreamContextHostName    :: HostName
    , udpUpstreamContextServiceName :: ServiceName
    , udpUpstreamContextTimeout     :: Int
    } deriving (Show)

newtype UdpUpstreamException = UdpUpstreamException Text deriving Show

instance Exception UdpUpstreamException

newtype UdpUpstreamT m a = UdpUpstreamT
    { unUdpUpStreamT :: ReaderT UdpUpstreamContext m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader UdpUpstreamContext)

deriving instance (MonadLogger m) => MonadLogger (UdpUpstreamT m)
deriving instance (MonadBase IO m) => MonadBase IO (UdpUpstreamT m)
deriving instance (MonadBaseControl IO m) => MonadBaseControl IO (UdpUpstreamT m)

runUdpStreamT :: UdpUpstreamContext -> UdpUpstreamT m a -> m a
runUdpStreamT ctx mt = runReaderT (unUdpUpStreamT mt) ctx

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
                rv <- liftIO $ timeout (tt*1000) $ bracket (dial ai) close $ \sock -> do
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
