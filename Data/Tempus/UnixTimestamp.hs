module Data.Tempus.UnixTimestamp
  ( UnixTimestamp (..)
   -- * Creation
  ) where

import Data.Ratio
import Data.String
import Data.Maybe

import Data.Tempus.Internal
import Data.Tempus.UnixTime
import Data.Tempus.GregorianTime
import Data.Tempus.Rfc3339

-- | A time representation counting the seconds since 1970-01-01T00:00:00-00:00
--   based on the UTC system.
--
--   * The type uses multiprecision integers internally and is able to represent
--     any UTC date in the past and in the future with arbitrary precision
--     (apart from the time span within a leap second).
--   * The instances for 'Data.String.IsString' and 'Prelude.Show' are only
--     meant for debugging purposes and default to 'unixEpoch' in case of
--     failure. Don't rely on their behaviour!
newtype UnixTimestamp
      = UnixTimestamp Rational
      deriving (Eq, Ord)

instance Show UnixTimestamp where
  -- The assumption is that every GregorianTimestamp is valid and renderable as Rfc3339 string
  -- and rendering failure is impossible.
  show = fromMaybe "1970-01-01T00:00:00-00:00" . renderRfc3339String

instance IsString UnixTimestamp where
  fromString = fromMaybe unixEpoch . parseRfc3339String

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
    = let (y,_,_) = daysToYearMonthDay (truncate (t + deltaUnixEpochCommonEpoch) `div` secsPerDay) in y
  month (UnixTimestamp t)
    = let (_,m,_) = daysToYearMonthDay (truncate (t + deltaUnixEpochCommonEpoch) `div` secsPerDay) in m
  day (UnixTimestamp t)
    = let (_,_,d) = daysToYearMonthDay (truncate (t + deltaUnixEpochCommonEpoch) `div` secsPerDay) in d
  hour (UnixTimestamp t)
    = truncate t `div` secsPerHour   `mod` hoursPerDay
  minute (UnixTimestamp t)
    = truncate t `div` secsPerMinute `mod` minsPerHour
  second (UnixTimestamp t)
    = truncate t                     `mod` secsPerMinute
  secondFraction (UnixTimestamp t)
    = t - (truncate t % 1)

  setYear y t
    = return $ UnixTimestamp
             $ (days * secsPerDay % 1)
             + (truncate (toSecondsSinceUnixEpoch t) `mod` secsPerDay % 1)
             + (secondFraction t)
             - deltaUnixEpochCommonEpoch
    where
      days = yearMonthDayToDays (y, month t, day t)

  setMonth m t
    = return $ UnixTimestamp
             $ (days * secsPerDay % 1)
             + (truncate (toSecondsSinceUnixEpoch t) `mod` secsPerDay % 1)
             + (secondFraction t)
             - deltaUnixEpochCommonEpoch
    where
      days = yearMonthDayToDays (year t, m, day t)

  setDay d t
    = return $ UnixTimestamp
             $ (days * secsPerDay % 1)
             + (truncate (toSecondsSinceUnixEpoch t) `mod` secsPerDay % 1)
             + (secondFraction t)
             - deltaUnixEpochCommonEpoch
    where
      days = yearMonthDayToDays (year t, month t, d)

  setHour h t
    = return $ UnixTimestamp
             $ (toSecondsSinceUnixEpoch t)
             - (hour t  * secsPerHour % 1)
             + (h * secsPerHour % 1)

  setMinute m t
    = return $ UnixTimestamp
             $ (toSecondsSinceUnixEpoch t)
             - (minute t  * secsPerMinute % 1)
             + (m * secsPerMinute % 1)

  setSecond s t
    = return $ UnixTimestamp
             $ (toSecondsSinceUnixEpoch t)
             - (second t % 1)
             + (s  % 1)

  setSecondFraction s (UnixTimestamp t)
    = return $ UnixTimestamp
             $ (truncate t % 1)
             + s

instance LocalOffset UnixTimestamp where
  localOffset _
    = Nothing
  setLocalOffset _ x
    = return x