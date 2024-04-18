{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}

module HummingBird.Cache 
    ( Cache (..)
    , CacheSettings (..)
    , mkCache
    , newTTLCache
    , insertTTLCache
    , lookupTTLCache
    ) where

import Control.Concurrent (killThread, ThreadId, forkIO, threadDelay)
import Control.Exception (mask_)
import Control.Monad (join)

import Data.Foldable (for_, Foldable (foldl'))
import Data.IORef (newIORef, readIORef, atomicModifyIORef', IORef, writeIORef)
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import Data.Time (getCurrentTime, UTCTime)

import Network.DNS (Domain, TYPE, RData)

data CacheSettings store item = CacheSettings
    { cacheAction       :: store -> IO (store -> store)
    -- ^ The action to perform on a store
    , cacheRefreshFreq  :: !Int
    -- ^ Number of microseconds to delay between calls `cacheAction`
    , cacheCons         :: item -> store -> store
    -- ^ Add an item into a store 
    , cacheNull         :: store -> Bool
    -- ^ Check if a store is empty, in which case the worker thread
    -- will shut down.
    , cacheEmpty        :: store
    -- ^ An empty store
    }

data Cache store item = Cache
    { cacheAdd  :: item -> IO ()
    , cacheRead :: IO store
    -- | Stopping the cache thread if exists.
    --   The current store is returned.
    , cacheStop :: IO store
    , cacheKill :: IO ()
    }

data CacheState store = NoCache       -- ^ No cache thread
                      | Store !store -- ^ The current cache

mkCache :: CacheSettings store item -> IO (Cache store item)
mkCache settings@CacheSettings{..} = do
    stateRef <- newIORef NoCache
    tidRef   <- newIORef Nothing
    pure Cache 
        { cacheAdd  = add settings stateRef tidRef
        , cacheRead = readRef stateRef
        , cacheStop = stop stateRef
        , cacheKill = kill tidRef
        }
    where
        readRef stateRef = do
            cs <- readIORef stateRef
            case cs of
                NoCache  -> pure cacheEmpty
                Store s -> pure s

        stop stateRef = atomicModifyIORef' stateRef $ \case 
            NoCache -> (NoCache, cacheEmpty)
            Store s -> (Store cacheEmpty, s)

        kill tidRef = do
            tid <- readIORef tidRef
            for_ tid killThread
        
add :: CacheSettings store item 
    -> IORef (CacheState store) 
    -> IORef (Maybe ThreadId) 
    -> item 
    -> IO ()
add settings@CacheSettings{..} stateRef tidRef item = 
    mask_ $ do join (atomicModifyIORef' stateRef cons)
    where
        cons NoCache    =   let s = cacheCons item cacheEmpty
                            in (Store s, spawn settings stateRef tidRef)
        cons (Store s)  =   let s' = cacheCons item s
                            in (Store s', pure ())
        
spawn   :: CacheSettings store item 
        -> IORef (CacheState store) 
        -> IORef (Maybe ThreadId)
        -> IO ()
spawn settings stateRef tidRef = do
    tid <- forkIO $ refresh settings stateRef tidRef
    writeIORef tidRef $ Just tid

refresh :: CacheSettings store item 
        -> IORef (CacheState store) 
        -> IORef (Maybe ThreadId)
        -> IO ()
refresh settings@CacheSettings{..} stateRef tidRef = do
    threadDelay cacheRefreshFreq

    store   <- atomicModifyIORef' stateRef swapWithEmpty
    !merge  <- cacheAction store
    join $ atomicModifyIORef' stateRef (check merge)
    where
        swapWithEmpty NoCache   = error "Unexpected NoCache"
        swapWithEmpty (Store s) = (Store cacheEmpty, s)

        check _ NoCache         = error "Unexpected NoCache"
        check merge (Store s)   = if cacheNull s'
            -- If there is no cache, refresh thread is terminated.
            then (NoCache, writeIORef tidRef Nothing)
            -- If there are caches, carry on.
            else (Store s', refresh settings stateRef tidRef)
            where s' = merge s

type Key        = (Domain, TYPE)
type Store      = OrdPSQ Key UTCTime [RData]
type TTLCache   = Cache Store (Key, UTCTime, [RData]) 

newTTLCache :: Int -> IO TTLCache 
newTTLCache secs = mkCache CacheSettings
    { cacheAction       = prune
    , cacheRefreshFreq  = secs * 1000000
    , cacheCons         = \(k, tim, v) psq -> PSQ.insert k tim v psq
    , cacheNull         = PSQ.null
    , cacheEmpty        = PSQ.empty
    }
    where
        prune oldpsq = do
            tim <- getCurrentTime
            let (_, pruned) = PSQ.atMostView tim oldpsq
            pure $ \newpsq -> foldl' ins pruned $ PSQ.toList newpsq
            where
                ins psq (k,p,v) = PSQ.insert k p v psq

insertTTLCache :: Key -> UTCTime -> [RData] -> TTLCache -> IO ()
insertTTLCache = undefined

lookupTTLCache :: Key -> TTLCache -> IO (Maybe (UTCTime, [RData]))
lookupTTLCache = undefined