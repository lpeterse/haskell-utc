{-# LANGUAGE Safe #-}
module Data.Tempus.GregorianTime.Type
  ( GregorianTime(..)
  , Offset(..)
  , validate
  ) where

import Control.Monad

import Data.Tempus.Epoch

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
data GregorianTime
   = GregorianTime
     { gdtYear         :: Integer
     , gdtMonth        :: Integer
     , gdtDay          :: Integer
     , gdtMinutes      :: Integer
     , gdtMilliSeconds :: Integer
     , gdtOffset       :: Offset
     }
   deriving (Eq, Ord)

data Offset
   = OffsetMinutes Integer
   | OffsetUnknown
   deriving (Eq, Ord)

instance UnixEpoch GregorianTime where
  unixEpoch
    = GregorianTime
      { gdtYear         = 1970
      , gdtMonth        = 1
      , gdtDay          = 1
      , gdtMinutes      = 0
      , gdtMilliSeconds = 0
      , gdtOffset       = OffsetMinutes 0
      }

validate :: MonadPlus m => GregorianTime -> m GregorianTime
validate gdt
  = do validateYear
       validateMonthAndDay
       validateMinutes
       validateMilliSeconds
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
    validateMinutes
      | 0 <= gdtMinutes gdt && gdtMinutes gdt < 24*60 = return ()
      | otherwise                                     = mzero
    validateMilliSeconds
      | 0 <= gdtMinutes gdt && gdtMinutes gdt < 61000 = return ()
      | otherwise                                     = mzero
    validateOffset
      = case gdtOffset gdt of
          OffsetUnknown   -> return ()
          OffsetMinutes o -> if negate (24*60) < o && o < (24*60)
                               then return ()
                               else mzero
    isLeapYear
      = let y = gdtYear gdt
        in  (y `mod` 4 == 0) && ((y `mod` 400 == 0) || (y `mod` 100 /= 0))
