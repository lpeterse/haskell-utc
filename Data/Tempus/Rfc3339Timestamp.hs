module Data.Tempus.Rfc3339Timestamp
  ( -- * Type
    Rfc3339Timestamp()
  -- * Creation
  ) where

import Control.Monad

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
     , gdtOffset         :: (Maybe Integer)
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
    = do let (year, month, day) =  daysToYearMonthDay (truncate s `div` (24 * 60 * 60))
         let hour               = truncate s `div` (60 * 60) `mod` 24
         let minute             = truncate s `div`       60  `mod` 60
         let second             = truncate s                 `mod` 60
         let secfrac            = s - (truncate s % 1)
         return commonEpoch 
           >>= setYear           year
           >>= setMonth          month
           >>= setDay            day
           >>= setHour           hour
           >>= setMinute         minute
           >>= setSecond         second
           >>= setSecondFraction secfrac


instance LocalOffset Rfc3339Timestamp where
  localOffset
    = gdtOffset
  setLocalOffset mm gt
    = validate $ gt { gdtOffset = mm }


validate :: MonadPlus m => Rfc3339Timestamp -> m Rfc3339Timestamp
validate gdt
  = do validateYear
       validateMonthAndDay
       validateHour
       validateMinute
       validateSecond
       validateSecondFraction
       validateOffset
       return gdt
  where
    validateYear
      = if 0 <= gdtYear gdt && gdtYear gdt <= 9999
          then return ()
          else mzero
    validateMonthAndDay
      = if 1 <= gdtMonth gdt && gdtMonth gdt <= 12
          then case gdtMonth gdt of
                 1  -> validateDays31
                 2  -> validateDays28or29
                 3  -> validateDays31
                 4  -> validateDays30
                 5  -> validateDays31
                 6  -> validateDays30
                 7  -> validateDays31
                 8  -> validateDays31
                 9  -> validateDays30
                 10 -> validateDays31
                 11 -> validateDays30
                 12 -> validateDays31
                 _  -> mzero
          else mzero
    validateDays31
      | 1 <= gdtDay gdt && gdtDay gdt <= 31           = return ()
      | otherwise                                     = mzero
    validateDays30
      | 1 <= gdtDay gdt && gdtDay gdt <= 30           = return ()
      | otherwise                                     = mzero
    validateDays28or29
      | 1 <= gdtDay gdt && gdtDay gdt <= 28           = return ()
      | gdtDay gdt == 29 && isLeapYear                = return ()
      | otherwise                                     = mzero
    validateHour
      | 0 <= gdtHour gdt && gdtHour gdt < 24          = return ()
      | otherwise                                     = mzero
    validateMinute
      | 0 <= gdtMinute gdt && gdtMinute gdt < 60      = return ()
      | otherwise                                     = mzero
    validateSecond
      | 0 <= gdtSecond gdt && gdtSecond gdt < 60      = return ()
      | otherwise                                     = mzero
    validateSecondFraction
      | truncate (gdtSecondFraction gdt) == (0 :: Integer) = return ()
      | otherwise                                          = mzero
    validateOffset
      = case gdtOffset gdt of
          Nothing  -> return ()
          Just   o -> if negate (24*60) < o && o < (24*60)
                        then return ()
                        else mzero
    isLeapYear
      = let y = gdtYear gdt
        in  (y `mod` 4 == 0) && ((y `mod` 400 == 0) || (y `mod` 100 /= 0))
