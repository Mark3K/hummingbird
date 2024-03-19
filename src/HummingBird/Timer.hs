module HummingBird.Timer
    ( Timer
    , newTimer
    , newTimerRange
    , startTimer
    , resetTimer
    , waitTimer
    ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import System.Random (StdGen, mkStdGen, Random (randomR))
import Numeric.Natural (Natural)
import Control.Monad (void, unless)
import Control.Concurrent (threadDelay)

data Timer = Timer 
    { timerAsync    :: TMVar (Async ())
      -- ^ The async computation of the timer
    , timerLock     :: TMVar ()
      -- ^ When the TMVar is empty, the timer is being used
    , timerGen      :: TVar StdGen
    , timerRange    :: (Natural, Natural)
    }

-- | Create a new timer with the supplied timer action and timer length
newTimer :: Natural -> IO Timer
newTimer timeout = newTimerRange 0 (timeout, timeout)

-- | Create a new timer with the supplied timer action, random seed, and range
-- from which the timer will choose a random timer length at each start or reset.
newTimerRange :: Int -> (Natural, Natural) -> IO Timer
newTimerRange seed rng = do
    (ta, tl, tg) <- atomically $ (,,) <$> newEmptyTMVar <*> newTMVar () <*> newTVar (mkStdGen seed)
    pure $ Timer ta tl tg rng

-- | Start the timer. If the timer is already running, the timer is not started.
-- Returns True if the timer was successfully started.
startTimer :: Timer -> IO Bool
startTimer timer = do
    lock <- atomically $ tryTakeTMVar $ timerLock timer
    case lock of
        Nothing -> pure False
        Just () -> resetTimer timer >> pure True

resetTimer :: Timer -> IO ()
resetTimer timer = do
    -- Chec if the tiimer is already running. If it is, asyncronously kill the thread
    mta <- atomically $ tryTakeTMVar $ timerAsync timer
    case mta of
        Nothing -> pure ()
        Just ta -> void $ async $ uninterruptibleCancel ta

    -- Fork a new async computation that waits the specified (random) amount of time.
    -- performs the timer action, and then puts the lock back signaling the timer finising
    ta <- async $ do
        threadDelay =<< randomDelay timer
        success <- atomically $ tryPutTMVar (timerLock timer) ()
        unless success $ error "[Failed Invariant]: Putting the timer lock back should succeed"
    
    -- Check that putting the new async succeeded. If it did not, there is a race condition
    -- and the newly created async should be canceled. Warning: this may not work for very short timers.
    success <- atomically $ tryPutTMVar (timerAsync timer) ta
    unless success $ void $ async (uninterruptibleCancel ta)

waitTimer :: Timer -> IO ()
waitTimer timer = atomically $ readTMVar (timerLock timer)

randomDelay :: Timer -> IO Int
randomDelay timer = atomically $ do
    g <- readTVar $ timerGen timer
    let (from', to') = timerRange timer
        (n, g') = randomR (toInteger from', toInteger to') g
    writeTVar (timerGen timer) g'
    pure (fromIntegral n)