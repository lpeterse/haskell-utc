module Data.Tempus.UnixTimestamp
  ( UnixTimestamp (..)
   -- * Creation
  ) where

import Control.Monad

import Data.Ratio

import Data.Tempus.GregorianTime
import Data.Tempus.Rfc3339Timestamp
import Data.Tempus.Internal
import Data.Tempus.UnixTime

-- | A time representation counting the milliseconds since 1970-01-01T00:00:00Z.
newtype UnixTimestamp
      = UnixTimestamp Rational
      deriving (Eq, Ord, Show)

instance UnixTime UnixTimestamp where
  unixEpoch 
    = UnixTimestamp 0
  unixSeconds (UnixTimestamp i)
    = i
  fromUnixSeconds s
    = return (UnixTimestamp s)

instance GregorianTime UnixTimestamp where
  commonEpoch
    = UnixTimestamp (negate deltaUnixEpochCommonEpoch)

  year
    = undefined
  month
    = undefined
  day
    = undefined
  hour   (UnixTimestamp t)
    = truncate t `div` (60 * 60) `mod` 24
  minute (UnixTimestamp t)
    = truncate t `div`       60  `mod` 60
  second (UnixTimestamp t)
    = truncate t                 `mod` 60
  secondFraction (UnixTimestamp t)
    = t - (truncate t % 1)

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
  setSecondFraction x
    = modifyAsRfc3339Time (setSecondFraction x)

  fromGregorianSeconds i
    = return $ UnixTimestamp (i + deltaUnixEpochCommonEpoch)
  gregorianSeconds (UnixTimestamp t)
    = t - deltaUnixEpochCommonEpoch

modifyAsRfc3339Time :: MonadPlus m => (Rfc3339Timestamp -> m Rfc3339Timestamp) -> UnixTimestamp -> m UnixTimestamp
modifyAsRfc3339Time f t
  = fromGregorianSeconds (gregorianSeconds t)
    >>= f
    >>= fromGregorianSeconds . gregorianSeconds

