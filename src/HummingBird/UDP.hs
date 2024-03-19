{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module HummingBird.UDP 
    ( bindSock
    , serve
    ) where

import Control.Exception (catch, SomeException, bracketOnError)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Logger.CallStack(logDebug, logError)

import Data.ByteString (ByteString)
import Data.Text(pack)

import Network.Socket
import Network.Socket.Address (recvFrom)
import System.IO (hPutStrLn, stderr)

serve :: (MonadIO m, MonadLogger m) => HostName -> ServiceName -> Int -> ((ByteString, SockAddr) -> IO ()) -> m a 
serve host port bufsize k = do
    (sock, addr) <- liftIO $ bindSock host port
    logDebug $ pack ("UDP.serve bind socket to " <> show addr)
    forever $ liftIO $ catch (void (process sock bufsize)) (\se -> hPutStrLn stderr (err <> show (se :: SomeException)))
    where 
        process sock bs = k =<< recvFrom sock bs
        err = "UDP.serve: Exception processing: "

bindSock :: MonadIO m => HostName -> ServiceName -> m (Socket, SockAddr)
bindSock host port = liftIO $ do
    addrs <- getAddrInfo (Just hints) (Just host) (Just port)
    tryAddrs addrs
    where
        hints :: AddrInfo
        hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Datagram }

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