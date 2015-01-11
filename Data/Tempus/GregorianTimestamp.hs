module Data.Tempus.GregorianTimestamp
  ( -- * Type
    GregorianTimestamp()
  -- * Creation
  ) where

import Control.Monad

import Data.Ratio
import Data.String
import Data.Maybe

import Data.Tempus.Epoch
import Data.Tempus.UnixTime
import Data.Tempus.GregorianTime
import Data.Tempus.Rfc3339
import Data.Tempus.Internal

-- | A time representation based on years, months, days, hours, minutes and seconds with
--   local offset based on the UTC system. 
--   This representation is very close to RFC3339 (a stricter profile of ISO8601) strings. 
--
--   * The type uses multiprecision integers internally and is able to represent
--     any UTC date in the past and in the future with arbitrary precision
--     (apart from the time span within a leap second).
--   * 'Prelude.Eq' and 'Prelude.Ord' are operating on the output of
--     'toSecondsSinceCommonEpoch' and are independant of local offsets.
--   * The instances for 'Data.String.IsString' and 'Prelude.Show' are only
--     meant for debugging purposes and default to 'epoch' in case of
--     failure. Don't rely on their behaviour!
data GregorianTimestamp
   = GregorianTimestamp
     { gdtYear           :: Integer
     , gdtMonth          :: Integer
     , gdtDay            :: Integer
     , gdtHour           :: Integer
     , gdtMinute         :: Integer
     , gdtSecond         :: Integer
     , gdtSecondFraction :: Rational
     , gdtOffset         :: (Maybe Rational)
     }

instance Eq GregorianTimestamp where
  (==) a b
    = (==)
       (toUnixSeconds a)
       (toUnixSeconds b)

instance Ord GregorianTimestamp where
  compare a b
    = compare
       (toUnixSeconds a)
       (toUnixSeconds b)

instance Show GregorianTimestamp where
  show = fromMaybe "1970-01-01T00:00:00-00:00" . renderRfc3339String

instance IsString GregorianTimestamp where
  fromString = fromMaybe epoch . parseRfc3339String

instance Epoch GregorianTimestamp where
  epoch
    = GregorianTimestamp
      { gdtYear           = 1970
      , gdtMonth          = 1
      , gdtDay            = 1
      , gdtHour           = 0
      , gdtMinute         = 0
      , gdtSecond         = 0
      , gdtSecondFraction = 0
      , gdtOffset         = Nothing
      }

instance UnixTime GregorianTimestamp where
  toUnixSeconds t
    = (days       * secsPerDay    % 1)
    + (hour t     * secsPerHour   % 1)
    + (minute t   * secsPerMinute % 1)
    + (second t                   % 1)
    + (secondFraction t)
    - (fromMaybe 0 $ localOffset t)
    - deltaUnixEpochCommonEpoch
    where
      days = yearMonthDayToDays (year t, month t, day t)
  fromUnixSeconds u
    = return
    $ GregorianTimestamp
      { gdtYear           = y
      , gdtMonth          = m
      , gdtDay            = d
      , gdtHour           = truncate s `div` secsPerHour   `mod` hoursPerDay
      , gdtMinute         = truncate s `div` secsPerMinute `mod` minsPerHour
      , gdtSecond         = truncate s                     `mod` secsPerMinute
      , gdtSecondFraction = s - (truncate s % 1)
      , gdtOffset         = Nothing
      }
    where
      s         = u + deltaUnixEpochCommonEpoch
      (y, m, d) = daysToYearMonthDay (truncate s `div` secsPerDay)

instance Date GregorianTimestamp where
  year
    = gdtYear
  month
    = gdtMonth
  day
    = gdtDay
  setYear x t
    = if isValidDate (x, month t, day t)
      then return t { gdtYear  = x }
      else fail   $ "Date.setYear "  ++ show x
  setMonth x t
    = if isValidDate (year t, x, day t)
      then return t { gdtMonth = x }
      else fail   $ "Date.setMonth " ++ show x
  setDay x t
    = if isValidDate (year t, month t, x)
      then return $ t { gdtDay   = x }
      else fail   $ "Date.setDay "   ++ show x

instance Time GregorianTimestamp where
  hour
    = gdtHour
  minute
    = gdtMinute
  second
    = gdtSecond
  secondFraction
    = gdtSecondFraction

  setHour x t
    | x < 0 || 23 < x = fail   $ "Time.setHour "           ++ show x
    | otherwise       = return $ t { gdtHour           = x }
  setMinute x t
    | x < 0 || 59 < x = fail   $ "Time.setMinute "         ++ show x
    | otherwise       = return $ t { gdtMinute         = x }
  setSecond x t
    | x < 0 || 59 < x = fail   $ "Time.setSecond "         ++ show x
    | otherwise       = return $ t { gdtSecond         = x }
  setSecondFraction x t
    | x < 0 || 1 <= x = fail   $ "Time.setSecondFraction " ++ show x
    | otherwise       = return $ t { gdtSecondFraction = x }

instance LocalOffset GregorianTimestamp where
  localOffset
    = gdtOffset
  setLocalOffset mm gt
    = return $ gt { gdtOffset = mm }
