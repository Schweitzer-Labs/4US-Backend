module Now where

import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX
import Data.Time
import Data.Int

secSinceEpoch :: UTCTime -> Int64
secSinceEpoch =
    floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

nowEpoch :: IO Int64
nowEpoch = do
    t <- getCurrentTime
    return $ secSinceEpoch t


secsAgo :: Int64 -> Int64 -> Int64
secsAgo now secs = now - secs
