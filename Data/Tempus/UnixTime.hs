module Data.Tempus.UnixTime
  ( UnixTime (..)
   -- * Creation
  ) where

import Control.Monad

import Data.Int

import Data.Tempus.Epoch
import Data.Tempus.GregorianCalendar
import Data.Tempus.Rfc3339Time
import Data.Tempus.Internal

-- | A time representation counting the milliseconds since 1970-01-01T00:00:00Z.
newtype UnixTime
      = UnixTime Integer
      deriving (Eq, Ord, Show)

instance UnixEpoch UnixTime where
  unixEpoch 
    = UnixTime 0

instance GregorianCalendar UnixTime where
  commonEpoch
    = UnixTime (negate msDiffUnixEpochCommonEpoch)

  getYear
    = mapAsRfc3339Time getYear
  getMonth
    = mapAsRfc3339Time getMonth
  getDay
    = mapAsRfc3339Time getDay
  getHour   (UnixTime t)
    = return $ t `div` (60 * 60 * 1000) `mod` 24
  getMinute (UnixTime t)
    = return $ t `div` (     60 * 1000) `mod` 60
  getSecond (UnixTime t)
    = return $ t `div` (          1000) `mod` 60
  getMilliSecond (UnixTime t)
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
    = return $ UnixTime (i + msDiffUnixEpochCommonEpoch)
  toMilliSecondsSinceCommonEpoch (UnixTime t)
    = return $ t - msDiffUnixEpochCommonEpoch

modifyAsRfc3339Time :: MonadPlus m => (Rfc3339Time -> m Rfc3339Time) -> UnixTime -> m UnixTime
modifyAsRfc3339Time f t
  = toMilliSecondsSinceCommonEpoch t
    >>= fromMilliSecondsSinceCommonEpoch
    >>= f
    >>= toMilliSecondsSinceCommonEpoch
    >>= fromMilliSecondsSinceCommonEpoch

mapAsRfc3339Time :: MonadPlus m => (Rfc3339Time -> m a) -> UnixTime -> m a
mapAsRfc3339Time f t
  = toMilliSecondsSinceCommonEpoch t
    >>= fromMilliSecondsSinceCommonEpoch
    >>= f
