module Data.Tempus.UnixTimestamp
  ( UnixTimestamp (..)
   -- * Creation
  ) where

import Data.Ratio

import Data.Tempus.Internal
import Data.Tempus.UnixTime
import Data.Tempus.GregorianTime

-- | A time representation counting the seconds since 1970-01-01T00:00:00Z.
newtype UnixTimestamp
      = UnixTimestamp Rational
      deriving (Eq, Ord, Show)

instance UnixTime UnixTimestamp where
  unixEpoch 
    = UnixTimestamp 0
  toSecondsSinceUnixEpoch (UnixTimestamp i)
    = i
  fromSecondsSinceUnixEpoch s
    = return (UnixTimestamp s)

instance GregorianTime UnixTimestamp where
  commonEpoch
    = UnixTimestamp (negate deltaUnixEpochCommonEpoch)

  year (UnixTimestamp t)
    = let (y,_,_) = daysToYearMonthDay (truncate t `div` secsPerDay) in y
  month (UnixTimestamp t)
    = let (_,m,_) = daysToYearMonthDay (truncate t `div` secsPerDay) in m
  day (UnixTimestamp t)
    = let (_,_,d) = daysToYearMonthDay (truncate t `div` secsPerDay) in d
  hour (UnixTimestamp t)
    = truncate t `div` secsPerHour   `mod` hoursPerDay
  minute (UnixTimestamp t)
    = truncate t `div` secsPerMinute `mod` minsPerHour
  second (UnixTimestamp t)
    = truncate t                     `mod` secsPerMinute
  secondFraction (UnixTimestamp t)
    = t - (truncate t % 1)

  fromSecondsSinceCommonEpoch i
    = return $ UnixTimestamp (i + deltaUnixEpochCommonEpoch)
  toSecondsSinceCommonEpoch (UnixTimestamp t)
    = t - deltaUnixEpochCommonEpoch