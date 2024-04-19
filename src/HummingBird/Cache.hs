{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RecordWildCards    #-}

module HummingBird.Cache where

import Control.Concurrent.Lifted (fork, threadDelay, ThreadId)
import Control.Exception.Lifted (mask_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Foldable (foldl')
import Data.IORef (IORef, atomicModifyIORef', writeIORef, readIORef, newIORef)
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import Data.Time (UTCTime, getCurrentTime, nominalDiffTimeToSeconds, diffUTCTime, addUTCTime, secondsToNominalDiffTime)

import Network.DNS (Domain, TYPE, TTL, RData)
import Control.Monad (join)

type K  = (Domain, TYPE)
type V  = [RData]
type S  = OrdPSQ K UTCTime V

data Cache = Cache
    { _cacheState   :: IORef (Maybe S)
    , _cacheTid     :: IORef (Maybe ThreadId)
    , _cacheMaxSize :: Int
    }

mkCache :: (MonadIO m) => Int -> m Cache
mkCache size = do
    state <- liftIO $ newIORef Nothing
    tid   <- liftIO $ newIORef Nothing
    pure $ Cache state tid size

insertCache :: (MonadIO m, MonadBaseControl IO m) => K -> V -> TTL -> Cache -> m ()
insertCache k v ttl c@Cache{..} = do
    now <- liftIO getCurrentTime
    let expire = addSeconds (fromIntegral ttl) now
    mask_ $ liftIO $ do join $ atomicModifyIORef' _cacheState (cons expire)
    where
        cons expire Nothing  =  let q = PSQ.insert k expire v PSQ.empty
                                in (Just q, spawn c)
        cons expire (Just s) =  let q = PSQ.insert k expire v s
                                in (Just q, pure ())

        spawn c@Cache {..} = do
            tid <- fork $ refresh c
            liftIO $ writeIORef _cacheTid $ Just tid

        refresh c@Cache {..} = do
            now     <- liftIO getCurrentTime
            store   <- liftIO $ atomicModifyIORef' _cacheState swapWithEmpty
            merge   <- prune now store
            item    <- liftIO $ do join $ atomicModifyIORef' _cacheState (check merge)
            case item of
                Nothing         -> pure ()
                Just (_, e, _)  -> do
                    threadDelay (seconds e now)
                    refresh c
            where
                swapWithEmpty Nothing   = error "Swap with no cache"
                swapWithEmpty (Just s)  = (Nothing, s)

                prune now s = do
                    let (_, pruned) = PSQ.atMostView now s
                    pure $ \q -> foldl' f pruned $ PSQ.toList q
                    where 
                        f psq (k, p, v) = PSQ.insert k p v psq

                check _ Nothing         = error "merge with no cache"
                check merge (Just s')   = if PSQ.null s''
                    then (Nothing, do writeIORef _cacheTid Nothing; pure Nothing)
                    else (Just s'', pure $ PSQ.findMin s'')
                    where s'' = merge s'

lookupCache :: (MonadIO m) => K -> Cache -> m (Maybe (TTL, V))
lookupCache k Cache{..} = do
    state <- liftIO $ readIORef _cacheState
    case state of
        Nothing -> pure Nothing
        Just s  -> case PSQ.lookup k s of
            Nothing      -> pure Nothing
            Just (e, v)  -> do
                now <- liftIO getCurrentTime
                pure $ Just (fromIntegral $ seconds e now, v)

seconds :: UTCTime -> UTCTime -> Int
seconds a b = floor $ nominalDiffTimeToSeconds (diffUTCTime a b)

addSeconds :: Int -> UTCTime -> UTCTime
addSeconds secs = addUTCTime (secondsToNominalDiffTime $ fromIntegral secs)