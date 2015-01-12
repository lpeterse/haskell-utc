module Data.Tempus.UnixTimestamp
  ( UnixTimestamp (..)
   -- * Creation
  ) where

import Data.Ratio
import Data.String
import Data.Maybe

import Data.Tempus.Epoch
import Data.Tempus.Local
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
  show = fromMaybe "1970-01-01T00:00:00-00:00" . renderRfc3339String . unknown

instance IsString UnixTimestamp where
  fromString = utc . fromMaybe epoch . parseRfc3339String

instance UnixTime UnixTimestamp where
  toUnixSeconds (UnixTimestamp i)
    = i
  fromUnixSeconds s
    = return (UnixTimestamp s)

instance Dated UnixTimestamp where
  year (UnixTimestamp t)
    = let (y,_,_) = daysToYearMonthDay (truncate (t + deltaUnixEpochCommonEpoch) `div` secsPerDay) in y
  month (UnixTimestamp t)
    = let (_,m,_) = daysToYearMonthDay (truncate (t + deltaUnixEpochCommonEpoch) `div` secsPerDay) in m
  day (UnixTimestamp t)
    = let (_,_,d) = daysToYearMonthDay (truncate (t + deltaUnixEpochCommonEpoch) `div` secsPerDay) in d
  setYear y t
    | not (isValidDate (y,m,d)) = fail   $ "Date.setYear " ++ show y
    | otherwise                 = return $ UnixTimestamp
                                         $ (yearMonthDayToDays (y,m,d) * secsPerDay % 1)
                                         + (daySecs t)
                                         - deltaUnixEpochCommonEpoch
    where
      m = month t
      d = day t
  setMonth m t
    | not (isValidDate (y,m,d)) = fail   $ "Date.setMonth " ++ show m
    | otherwise                 = return $ UnixTimestamp
                                         $ (yearMonthDayToDays (y,m,d) * secsPerDay % 1)
                                         + (daySecs t)
                                         - deltaUnixEpochCommonEpoch
    where
      y = year t
      d = day t
  setDay d t
    | not (isValidDate (y,m,d)) = fail   $ "Date.setDay " ++ show d
    | otherwise                 = return $ UnixTimestamp
                                         $ (yearMonthDayToDays (y,m,d) * secsPerDay % 1)
                                         + (daySecs t)
                                         - deltaUnixEpochCommonEpoch
    where
      y = year t
      m = month t

instance Timed UnixTimestamp where
  hour (UnixTimestamp t)
    = truncate t `div` secsPerHour   `mod` hoursPerDay
  minute (UnixTimestamp t)
    = truncate t `div` secsPerMinute `mod` minsPerHour
  second (UnixTimestamp t)
    = truncate t                     `mod` secsPerMinute
  secondFraction (UnixTimestamp t)
    = t - (truncate t % 1)
  setHour h t
    | h < 0     = fail ""
    | h > 23    = fail ""
    | otherwise = return $ UnixTimestamp
                         $ (toUnixSeconds t)
                         - (hour t  * secsPerHour % 1)
                         + (h * secsPerHour % 1)
  setMinute m t
    | m < 0     = fail ""
    | m > 59    = fail ""
    | otherwise = return $ UnixTimestamp
                         $ (toUnixSeconds t)
                         - (minute t  * secsPerMinute % 1)
                         + (m * secsPerMinute % 1)
  setSecond s t
    | s < 0     = fail ""
    | s > 59    = fail ""
    | otherwise = return $ UnixTimestamp
                         $ (toUnixSeconds t)
                         - (second t % 1)
                         + (s  % 1)
  setSecondFraction s (UnixTimestamp t)
    | s <  0.0  = fail ""
    | s >= 1.0  = fail ""
    | otherwise = return $ UnixTimestamp
                         $ (truncate t % 1)
                         + s
  midnight = epoch

-- helpers

-- | The seconds since midnight.
daySecs :: UnixTimestamp -> Rational
daySecs u@(UnixTimestamp t)
  = (truncate t `mod` secsPerDay % 1)
  + (secondFraction u)