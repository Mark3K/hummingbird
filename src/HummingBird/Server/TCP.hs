{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module HummingBird.Server.TCP where

import Control.Concurrent.Lifted (forkFinally, fork)
import Control.Concurrent.STM
import Control.Exception.Lifted (catch, SomeException)
import Control.Lens (makeClassyPrisms, makeClassy, view, (^.), (#))
import Control.Monad (forever, when)
import Control.Monad.Except (MonadError(throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger.CallStack (MonadLogger, logDebug, logError, logWarn)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (pack)

import qualified Network.DNS as DNS
import Network.Socket 

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
    , _tcpServerEnvRequests :: TVar (Map DNS.Identifier (TMVar Response))
    } deriving (Eq)
makeClassy ''TcpServerEnv

type TcpServerProvision c e m =
    ( MonadIO m
    , MonadLogger m
    , MonadBaseControl IO m
    , MonadReader c m, HasTcpServerEnv c
    , MonadError e m, AsTcpServerError e
    )

buildTcpServerEnv :: (MonadIO m) => Config -> m (Either TcpServerError TcpServerEnv)
buildTcpServerEnv config = do
    requests <- liftIO $ newTVarIO mempty
    pure $ Right $ TcpServerEnv
        { _tcpServerEnvHost     = show $ config ^. configListenAddr
        , _tcpServerEnvPort     = show $ config ^. configListenPort
        , _tcpServerEnvRequests = requests
        }

serve :: TcpServerProvision c e m => TChan Event -> m ()
serve ch = do
    host <- view tcpServerEnvHost
    port <- view tcpServerEnvPort

    rv   <- openSock Stream host port
    case rv of
        Left             s -> throwError $ _TcpSocketError # s
        Right (sock, addr) -> do
            _ <- liftIO $ listen sock 5
            logDebug $ pack ("TCP serve at " <> show addr)

            respCh <- liftIO newTChanIO
            _ <- fork (forever $ sender respCh)
            forever $ do
                (csock, caddr) <- liftIO $ accept sock
                logDebug $ pack ("A new TCP client " <> show caddr)
                forkFinally 
                    (catch (process csock ch respCh)
                        (\(se :: SomeException) -> do
                                logError $ pack ("error processing tcp request: " <> show se)
                                throwError $ _TcpSocketError # show se)
                    )
                    (\_ -> closeSock csock)

    where
        process sock ch0 ch1 = do
            msg <- liftIO $ DNS.receiveVC sock
            if DNS.qOrR (DNS.flags $ DNS.header msg) == DNS.QR_Query
            then do
                logDebug $ pack ("TCP DNS message: " <> show msg)

                reqsref  <- view tcpServerEnvRequests
                let _id = (DNS.identifier . DNS.header) msg

                respVar <- liftIO $ atomically $ do
                    newRespVar  <- newEmptyTMVar
                    requests    <- readTVar reqsref
                    when (Map.member _id requests) $ case Map.lookup _id requests of
                        Nothing -> pure ()
                        Just rs -> putTMVar rs Dead
                    writeTVar reqsref . Map.insert _id newRespVar $ requests
                    pure newRespVar
                
                liftIO $ atomically $ writeTChan ch0 (RequestEvent $ RequestContext msg Nothing ch1)
                response <- liftIO $ atomically $ takeTMVar respVar
                case response of
                    OK m -> liftIO $ DNS.sendVC sock $ DNS.encode m
                    Dead -> pure ()
                liftIO $ atomically $ modifyTVar reqsref $ \reqs -> Map.delete _id reqs
            else do
                logError $ pack ("NOT a query: " <> show msg) 

        sender tc = do
            resp <- liftIO $ atomically $ readTChan tc
            case resp of
                ResponseUdp               _ -> logWarn $ pack "TCP server received UDP response"
                ResponseTcp TcpResponse{..} -> do
                    logDebug $ pack ("TCP server response: " <> show trMessage)
                    reqsref  <- view tcpServerEnvRequests
                    let _id = (DNS.identifier . DNS.header) trMessage
                    liftIO $ atomically $ do
                        requests <- readTVar reqsref
                        when (Map.member _id requests) $ case Map.lookup _id requests of
                            Nothing -> pure ()
                            Just rs -> putTMVar rs $ OK trMessage
 