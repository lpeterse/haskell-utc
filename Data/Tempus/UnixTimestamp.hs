module Data.Tempus.UnixTimestamp
  ( UnixTimestamp (..)
   -- * Creation
  ) where

import Control.Monad

import Data.Tempus.GregorianCalendar
import Data.Tempus.Rfc3339Time
import Data.Tempus.Internal
import Data.Tempus.UnixTime

-- | A time representation counting the milliseconds since 1970-01-01T00:00:00Z.
newtype UnixTimestamp
      = UnixTimestamp Integer
      deriving (Eq, Ord, Show)

instance UnixTime UnixTimestamp where
  unixEpoch 
    = UnixTimestamp 0
  unixSeconds (UnixTimestamp i)
    = i `div` 1000
  fromUnixSeconds s
    = return (UnixTimestamp (s * 1000))

instance GregorianCalendar UnixTimestamp where
  commonEpoch
    = UnixTimestamp (negate msDiffUnixEpochCommonEpoch)

  getYear
    = mapAsRfc3339Time getYear
  getMonth
    = mapAsRfc3339Time getMonth
  getDay
    = mapAsRfc3339Time getDay
  getHour   (UnixTimestamp t)
    = return $ t `div` (60 * 60 * 1000) `mod` 24
  getMinute (UnixTimestamp t)
    = return $ t `div` (     60 * 1000) `mod` 60
  getSecond (UnixTimestamp t)
    = return $ t `div` (          1000) `mod` 60
  getMilliSecond (UnixTimestamp t)
    = return $ t                        `mod` 1000

  setYear x
    = modifyAsRfc3339Time (setYear x)
  setMonth x
    = modifyAsRfc3339Time (setMonth x)
  setDay x
    = modifyAsRfc3339Time (setDay x)
  setHour x
    = modifyAsRfc3339Time (setHour x)
  setMinute x
    = modifyAsRfc3339Time (setMinute x)
  setSecond x
    = modifyAsRfc3339Time (setSecond x)
  setMilliSecond x
    = modifyAsRfc3339Time (setMilliSecond x)

  fromMilliSecondsSinceCommonEpoch i
    = return $ UnixTimestamp (i + msDiffUnixEpochCommonEpoch)
  toMilliSecondsSinceCommonEpoch (UnixTimestamp t)
    = return $ t - msDiffUnixEpochCommonEpoch

modifyAsRfc3339Time :: MonadPlus m => (Rfc3339Time -> m Rfc3339Time) -> UnixTimestamp -> m UnixTimestamp
modifyAsRfc3339Time f t
  = toMilliSecondsSinceCommonEpoch t
    >>= fromMilliSecondsSinceCommonEpoch
    >>= f
    >>= toMilliSecondsSinceCommonEpoch
    >>= fromMilliSecondsSinceCommonEpoch

mapAsRfc3339Time :: MonadPlus m => (Rfc3339Time -> m a) -> UnixTimestamp -> m a
mapAsRfc3339Time f t
  = toMilliSecondsSinceCommonEpoch t
    >>= fromMilliSecondsSinceCommonEpoch
    >>= f
