module Data.Tempus.GregorianTime.Type
  ( GregorianTime(..)
  , Offset(..)
  ) where

-- | A time and date representation based on years, months and days.
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
     { gdtYear         :: Int
     , gdtMonth        :: Int
     , gdtDay          :: Int
     , gdtMinutes      :: Int
     , gdtMilliSeconds :: Int
     , gdtOffset       :: Offset
     }
   deriving (Eq, Ord)

data Offset
   = OffsetMinutes Int
   | OffsetUnknown
   deriving (Eq, Ord)
