{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module HummingBird.Cache 
    ( Cache

    , Key (..)
    , buildKey
    , keyDomain
    , keyType

    , Item (..)
    , buildItem
    , itemHeaderAD
    , itemHeaderRD
    , itemRcode
    , itemAnswer
    , itemAuthority
    , itemAdditional

    , mkCache
    , insertCache
    , lookupCache
    ) where

import Control.Concurrent.Lifted (fork, threadDelay, ThreadId, killThread)
import Control.Exception.Lifted (mask_)
import Control.Lens (makeLenses)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Foldable (foldl', for_)
import Data.IORef (IORef, atomicModifyIORef', writeIORef, readIORef, newIORef)
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import Data.Time (UTCTime, getCurrentTime)

import Katip (KatipContext, showLS, logTM, Severity (InfoS))
import Network.DNS

import HummingBird.Utils

data Key  = Key 
    { _keyDomain    :: Domain
    , _keyType      :: TYPE
    } deriving (Show, Eq, Ord)
makeLenses ''Key

buildKey :: DNSMessage -> Key
buildKey DNSMessage {question = []}     = error "empty question" 
buildKey DNSMessage {question = (qt:_)} = Key {_keyDomain = qname qt, _keyType = qtype qt}
    
data Item  = Item
    { _itemHeaderAD     :: Bool -- Authenticated Data bit
    , _itemHeaderRD     :: Bool -- Recursion Desired bit
    , _itemRcode        :: RCODE
    , _itemAnswer      :: Answers
    , _itemAuthority    :: AuthorityRecords
    , _itemAdditional   :: AdditionalRecords
    } deriving(Show, Eq)
makeLenses ''Item

buildItem :: DNSMessage -> Item
buildItem DNSMessage {..} = Item 
    { _itemHeaderAD     = ad header
    , _itemHeaderRD     = rd header
    , _itemRcode        = rc header
    , _itemAnswer       = answer
    , _itemAuthority    = authority
    , _itemAdditional   = additional
    }
    where 
        ad DNSHeader{..} = authenData flags 
        rd DNSHeader{..} = recDesired flags
        rc DNSHeader{..} = rcode      flags

type S  = OrdPSQ Key UTCTime Item

data Cache = Cache
    { _cacheState   :: IORef (Maybe S)
    , _cacheTid     :: IORef (Maybe ThreadId)
    , _cacheMaxSize :: Int
    , _cacheMaxTTL  :: TTL
    , _cacheMinTTL  :: TTL
    }

mkCache :: (MonadIO m) => Int -> Int -> Int -> m Cache
mkCache size maxTTL minTTL = do
    state <- liftIO $ newIORef Nothing
    tid   <- liftIO $ newIORef Nothing
    pure $ Cache state tid size (fromIntegral maxTTL) (fromIntegral minTTL)

insertCache :: (KatipContext m, MonadBaseControl IO m) => DNSMessage -> Cache -> m ()
insertCache msg c@Cache{..} = do
    $(logTM) InfoS ("cache insert: " <> showLS msg)
    $(logTM) InfoS ("cache item: " <> showLS item)
    now <- liftIO getCurrentTime
    let expire = addUTCTimeSeconds (fromIntegral ttl) now
    mask_ $ liftIO $ do join $ atomicModifyIORef' _cacheState (cons expire)
    where
        key     = buildKey msg
        item    = buildItem msg
        ttl     = max (min itemTTL _cacheMaxTTL) _cacheMinTTL
        itemTTL = if null ttls then _cacheMinTTL else minimum ttls
        ttls    = map rrttl (_itemAnswer item <> _itemAuthority item <> _itemAdditional item)
        
        cons expire Nothing  =  let q = PSQ.insert key expire item PSQ.empty
                                in (Just q, spawn)
        cons expire (Just s) =  let a = if PSQ.size s > _cacheMaxSize then PSQ.deleteMin s else s
                                    b = PSQ.insert key expire item a
                                in (Just b, respawn expire)
        spawn = do
            tid <- fork $ refreshCache c
            liftIO $ writeIORef _cacheTid $ Just tid

        respawn expire = do
            state <- readIORef _cacheState
            case state >>= PSQ.findMin >>= \(_, e, _) -> pure $ diffUTCTimeInSeconds expire e < 0 of
                Nothing     -> pure ()
                Just False  -> pure ()
                Just True   -> do
                    tid <- liftIO $ readIORef _cacheTid
                    for_ tid killThread
                    spawn

refreshCache :: (MonadIO m, MonadBaseControl IO m) => Cache -> m ()
refreshCache c@Cache {..} = do
    now     <- liftIO getCurrentTime
    store   <- liftIO $ atomicModifyIORef' _cacheState swapWithEmpty
    merge   <- prune now store
    item    <- liftIO $ do join $ atomicModifyIORef' _cacheState (check merge)
    case item of
        Nothing         -> pure ()
        Just (_, e, _)  -> do
            threadDelay $ diffUTCTimeInSeconds e now * 1000 * 1000
            refreshCache c
    where
        swapWithEmpty Nothing   = error "Swap with empty cache"
        swapWithEmpty (Just s)  = (Just PSQ.empty, s)

        prune now s = do
            let (_, pruned) = PSQ.atMostView now s
            pure $ \q -> foldl' f pruned $ PSQ.toList q
            where 
                f psq (k, p, v) = PSQ.insert k p v psq

        check _ Nothing         = error "merge with no cache"
        check merge (Just s)   = if PSQ.null s'
            then (Nothing, do writeIORef _cacheTid Nothing; pure Nothing)
            else (Just s', pure $ PSQ.findMin s')
            where s' = merge s

lookupCache :: (MonadIO m) => Key -> Cache -> m (Maybe (TTL, Item))
lookupCache k Cache{..} = do
    now   <- liftIO getCurrentTime
    state <- liftIO $ readIORef _cacheState
    pure $ withTTL now =<< PSQ.lookup k =<< state
    where
        withTTL now (e, v) = if ttl <= 0 then Nothing else Just (fromIntegral ttl, v)
            where ttl = diffUTCTimeInSeconds e now
        
