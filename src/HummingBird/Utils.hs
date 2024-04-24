module HummingBird.Utils 
    ( diffUTCTimeInSeconds
    , addUTCTimeSeconds
    ) where

import Data.Time.Clock
    ( UTCTime
    , nominalDiffTimeToSeconds
    , secondsToNominalDiffTime
    , addUTCTime
    , diffUTCTime 
    )

diffUTCTimeInSeconds :: UTCTime -> UTCTime -> Int
diffUTCTimeInSeconds a b = floor $ nominalDiffTimeToSeconds (diffUTCTime a b)

addUTCTimeSeconds :: Int -> UTCTime -> UTCTime
addUTCTimeSeconds secs = addUTCTime (secondsToNominalDiffTime $ fromIntegral secs)