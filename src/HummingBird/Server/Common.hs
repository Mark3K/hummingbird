{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE RecordWildCards        #-}

module HummingBird.Server.Common where

import Control.Exception.Lifted (catch, bracketOnError, SomeException)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger.CallStack (MonadLogger, logDebug)
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Text (pack)

import Network.Socket 

openSock 
    :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
    => SocketType -> HostName -> ServiceName -> m (Either String (Socket, SockAddr))
openSock socktyp host port = do
    addrs <- liftIO $ getAddrInfo (Just hints) (Just host) (Just port)
    logDebug $ pack ("TCP available address: " <> show addrs)
    tryAddrs addrs
    where
        hints = defaultHints
            { addrFlags         = [AI_PASSIVE, AI_NUMERICHOST, AI_NUMERICSERV]
            , addrSocketType    = socktyp
            }

        tryAddrs = \case
            []      -> pure $ Left ("Address not available: " <> show (host, port))
            [x]     -> useAddr x
            (x:xs)  -> catch (useAddr x) (\(_ :: IOError) -> tryAddrs xs)
        
        useAddr addr = bracketOnError (newSock addr) closeSock $ \sock -> do
            let sockAddr = addrAddress addr
            liftIO (bind sock sockAddr)
            pure $ Right (sock, sockAddr)

newSock :: MonadIO m => AddrInfo -> m Socket
newSock AddrInfo{..} = do
    liftIO (socket addrFamily addrSocketType addrProtocol)

closeSock :: MonadIO m => Socket -> m ()
closeSock sock = liftIO $ do
    catch (close sock) (\(_ :: SomeException) -> pure ())