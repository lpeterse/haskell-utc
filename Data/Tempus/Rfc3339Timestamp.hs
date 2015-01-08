module Data.Tempus.Rfc3339Timestamp
  ( -- * Type
    Rfc3339Timestamp()
  -- * Creation
  ) where

import Data.Ratio
import Data.String
import Data.Maybe

import Data.Tempus.GregorianTime
import Data.Tempus.Rfc3339
import Data.Tempus.Internal

-- | A time and date representation based on years, months, days, hours, minutes and seconds.
-- This representation is closest to RFC3339 (a stricter profile of ISO8601) strings. 
--
-- Use it if
--
--   * you are parsing and rendering RFC3339 strings and only use
--     Gregorian operations in between.
--   * you need to be able to represent leap seconds.
--   * you need to be able to represent a local offset (timezone).
--   * you don't care about a value's memory footprint.
data Rfc3339Timestamp
   = Rfc3339Timestamp
     { gdtYear           :: Integer
     , gdtMonth          :: Integer
     , gdtDay            :: Integer
     , gdtHour           :: Integer
     , gdtMinute         :: Integer
     , gdtSecond         :: Integer
     , gdtSecondFraction :: Rational
     , gdtOffset         :: (Maybe Rational)
     }
   deriving (Eq, Ord)

instance Show Rfc3339Timestamp where
  -- The assumption is that every Rfc3339Timestamp is valid and renderable as Rfc3339 string
  -- and rendering failure is impossible.
  show = fromMaybe undefined . renderRfc3339String

instance IsString Rfc3339Timestamp where
  fromString = fromMaybe undefined . parseRfc3339String

instance GregorianTime Rfc3339Timestamp where
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
    = Rfc3339Timestamp
      { gdtYear           = 0
      , gdtMonth          = 1
      , gdtDay            = 1
      , gdtHour           = 0
      , gdtMinute         = 0
      , gdtSecond         = 0
      , gdtSecondFraction = 0
      , gdtOffset       = Just 0
      }

  toSecondsSinceCommonEpoch t
    = (days  * 24 * 60 * 60 % 1)
    + (hour t     * 60 * 60 % 1)
    + (minute t        * 60 % 1)
    + (second t             % 1)
    + (secondFraction t)
    where
      days = yearMonthDayToDays (year t, month t, day t)

  fromSecondsSinceCommonEpoch s
    = do let (y, m, d) = daysToYearMonthDay (truncate s `div` (24 * 60 * 60))
         let hh        = truncate s `div` (60 * 60) `mod` 24
         let mm        = truncate s `div`       60  `mod` 60
         let ss        = truncate s                 `mod` 60
         let sf        = s - (truncate s % 1)
         return commonEpoch 
           >>= setYear           y
           >>= setMonth          m
           >>= setDay            d
           >>= setHour           hh
           >>= setMinute         mm
           >>= setSecond         ss
           >>= setSecondFraction sf

instance LocalOffset Rfc3339Timestamp where
  localOffset
    = gdtOffset
  setLocalOffset mm gt
    = validate $ gt { gdtOffset = mm }
