{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module HummingBird.Server.Common where

import Control.Exception.Lifted (catch, bracketOnError, SomeException)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control (MonadBaseControl)

import GHC.IO.Exception (IOErrorType(NoSuchThing), IOException (IOError))

import Katip (KatipContext, Severity(..), logTM, showLS)

import Network.Socket 
import Control.Monad.Catch (MonadThrow (throwM))

openSock 
    :: (MonadThrow m, KatipContext m, MonadBaseControl IO m) 
    => SocketType -> HostName -> ServiceName -> m (Socket, SockAddr)
openSock socktyp host port = do
    addrs <- liftIO $ getAddrInfo (Just hints) (Just host) (Just port)
    $(logTM) DebugS ("TCP available address: " <> showLS addrs)
    tryAddrs addrs
    where
        hints = defaultHints
            { addrFlags         = [AI_PASSIVE, AI_NUMERICHOST, AI_NUMERICSERV]
            , addrSocketType    = socktyp
            }

        tryAddrs = \case
            []      -> throwM $ IOError Nothing NoSuchThing "" ("Address not available: " <> show (host, port)) Nothing Nothing
            [x]     -> useAddr x
            (x:xs)  -> catch (useAddr x) (\(_ :: IOError) -> tryAddrs xs)
        
        useAddr addr = bracketOnError (newSock addr) closeSock $ \sock -> do
            let sockAddr = addrAddress addr
            liftIO (bind sock sockAddr)
            pure (sock, sockAddr)

newSock :: MonadIO m => AddrInfo -> m Socket
newSock AddrInfo{..} = do
    liftIO (socket addrFamily addrSocketType addrProtocol)

closeSock :: MonadIO m => Socket -> m ()
closeSock sock = liftIO $ do
    catch (close sock) (\(_ :: SomeException) -> pure ())