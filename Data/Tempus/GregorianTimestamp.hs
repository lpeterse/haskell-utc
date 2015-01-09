module Data.Tempus.GregorianTimestamp
  ( -- * Type
    GregorianTimestamp()
  -- * Creation
  ) where

import Data.Ratio
import Data.String
import Data.Maybe

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
--     meant for debugging purposes and default to 'commonEpoch' in case of
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
       (toSecondsSinceCommonEpoch a)
       (toSecondsSinceCommonEpoch b)

instance Ord GregorianTimestamp where
  compare a b
    = compare
       (toSecondsSinceCommonEpoch a)
       (toSecondsSinceCommonEpoch b)

instance Show GregorianTimestamp where
  -- The assumption is that every GregorianTimestamp is valid and renderable as Rfc3339 string
  -- and rendering failure is impossible.
  show = fromMaybe "0000-01-01T00:00:00-00:00" . renderRfc3339String

instance IsString GregorianTimestamp where
  fromString = fromMaybe commonEpoch . parseRfc3339String

instance UnixTime GregorianTimestamp where
  unixEpoch
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
  toSecondsSinceUnixEpoch t
    = toSecondsSinceCommonEpoch t + deltaUnixEpochCommonEpoch
  fromSecondsSinceUnixEpoch s
    = fromSecondsSinceCommonEpoch (s - deltaUnixEpochCommonEpoch)

instance GregorianTime GregorianTimestamp where
  year
    = gdtYear
  month
    = gdtMonth
  day
    = gdtDay
  hour
    = gdtHour
  minute
    = gdtMinute
  second
    = gdtSecond
  secondFraction
    = gdtSecondFraction

  setYear x gt
    = validate $ gt { gdtYear           = x }
  setMonth x gt
    = validate $ gt { gdtMonth          = x }
  setDay x gt
    = validate $ gt { gdtDay            = x }
  setHour x gt
    = validate $ gt { gdtHour           = x }
  setMinute x gt
    = validate $ gt { gdtMinute         = x }
  setSecond x gt
    = validate $ gt { gdtSecond         = x }
  setSecondFraction x gt
    = validate $ gt { gdtSecondFraction = x }

  commonEpoch
    = GregorianTimestamp
      { gdtYear           = 0
      , gdtMonth          = 1
      , gdtDay            = 1
      , gdtHour           = 0
      , gdtMinute         = 0
      , gdtSecond         = 0
      , gdtSecondFraction = 0
      , gdtOffset         = Nothing
      }

  toSecondsSinceCommonEpoch t
    = (days       * secsPerDay    % 1)
    + (hour t     * secsPerHour   % 1)
    + (minute t   * secsPerMinute % 1)
    + (second t                   % 1)
    + (secondFraction t)
    - (fromMaybe 0 $ localOffset t)
    where
      days = yearMonthDayToDays (year t, month t, day t)

  fromSecondsSinceCommonEpoch s
    = validate
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
      (y, m, d) = daysToYearMonthDay (truncate s `div` secsPerDay)

instance LocalOffset GregorianTimestamp where
  localOffset
    = gdtOffset
  setLocalOffset mm gt
    = validate $ gt { gdtOffset = mm }
