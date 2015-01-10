module Data.Tempus.UnixTimestamp
  ( UnixTimestamp (..)
   -- * Creation
  ) where

import Control.Monad

import Data.Ratio
import Data.String
import Data.Maybe

import Data.Tempus.Epoch
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
--     meant for debugging purposes and default to 'epoch' in case of
--     failure. Don't rely on their behaviour!
newtype UnixTimestamp
      = UnixTimestamp Rational
      deriving (Eq, Ord)

instance Epoch UnixTimestamp where
  epoch = UnixTimestamp 0

instance Show UnixTimestamp where
  show = fromMaybe "1970-01-01T00:00:00-00:00" . renderRfc3339String

instance IsString UnixTimestamp where
  fromString = fromMaybe epoch . parseRfc3339String

instance UnixTime UnixTimestamp where
  toUnixSeconds (UnixTimestamp i)
    = i
  fromUnixSeconds s
    = return (UnixTimestamp s)

instance Date UnixTimestamp where
  year (UnixTimestamp t)
    = let (y,_,_) = daysToYearMonthDay (truncate (t + deltaUnixEpochCommonEpoch) `div` secsPerDay) in y
  month (UnixTimestamp t)
    = let (_,m,_) = daysToYearMonthDay (truncate (t + deltaUnixEpochCommonEpoch) `div` secsPerDay) in m
  day (UnixTimestamp t)
    = let (_,_,d) = daysToYearMonthDay (truncate (t + deltaUnixEpochCommonEpoch) `div` secsPerDay) in d
  setYear y t
    = return $ UnixTimestamp
             $ (days * secsPerDay % 1)
             + (daySecs t)
             - deltaUnixEpochCommonEpoch
    where
      days = yearMonthDayToDays (y, month t, day t)
  setMonth m t
    = return $ UnixTimestamp
             $ (days * secsPerDay % 1)
             + (daySecs t)
             - deltaUnixEpochCommonEpoch
    where
      days = yearMonthDayToDays (year t, m, day t)
  setDay d t
    = return $ UnixTimestamp
             $ (days * secsPerDay % 1)
             + (daySecs t)
             - deltaUnixEpochCommonEpoch
    where
      days = yearMonthDayToDays (year t, month t, d)

instance Time UnixTimestamp where
  hour (UnixTimestamp t)
    = truncate t `div` secsPerHour   `mod` hoursPerDay
  minute (UnixTimestamp t)
    = truncate t `div` secsPerMinute `mod` minsPerHour
  second (UnixTimestamp t)
    = truncate t                     `mod` secsPerMinute
  secondFraction (UnixTimestamp t)
    = t - (truncate t % 1)
  setHour h t
    | h < 0     = mzero
    | h > 23    = mzero
    | otherwise = return $ UnixTimestamp
                         $ (toUnixSeconds t)
                         - (hour t  * secsPerHour % 1)
                         + (h * secsPerHour % 1)
  setMinute m t
    | m < 0     = mzero
    | m > 59    = mzero
    | otherwise = return $ UnixTimestamp
                         $ (toUnixSeconds t)
                         - (minute t  * secsPerMinute % 1)
                         + (m * secsPerMinute % 1)
  setSecond s t
    | s < 0     = mzero
    | s > 59    = mzero
    | otherwise = return $ UnixTimestamp
                         $ (toUnixSeconds t)
                         - (second t % 1)
                         + (s  % 1)
  setSecondFraction s (UnixTimestamp t)
    | s <  0.0  = mzero
    | s >= 1.0  = mzero
    | otherwise = return $ UnixTimestamp
                         $ (truncate t % 1)
                         + s

instance LocalOffset UnixTimestamp where
  localOffset _
    = Nothing
  setLocalOffset _ x
    = return x

-- helpers

-- | The seconds since midnight.
daySecs :: UnixTimestamp -> Rational
daySecs u@(UnixTimestamp t)
  = (truncate t `mod` secsPerDay % 1)
  + (secondFraction u)