module Data.Tempus.GregorianDateTime.Internal
  ( GregorianDateTime(..)
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
data GregorianDateTime
   = GregorianDateTime
     { gdtYear    :: Int
     , gdtMonth   :: Int
     , gdtMDay    :: Int
     , gdtHour    :: Int
     , gdtMinute  :: Int
     , gdtmSecond :: Int
     , gdtOffset  :: Offset
     }
   | InvalidTime

data Offset
   = OffsetMinutes Int
   | OffsetUnknown

instance Eq GregorianDateTime where
  a == b
    =  gdtYear   a  == gdtYear    b
    && gdtMonth  a  == gdtMonth   b
    && gdtMDay   a  == gdtMDay    b
    && gdtHour   a  == gdtHour    b
    && gdtMinute a  == gdtMinute  b
    && gdtmSecond a == gdtmSecond b

instance Ord GregorianDateTime where
  compare a b
    = case compare (gdtYear a) (gdtYear b) of
        LT -> LT
        GT -> GT
        EQ -> case compare (gdtMonth a) (gdtMonth b) of
                LT -> LT
                GT -> GT
                EQ -> case compare (gdtMDay a) (gdtMDay b) of
                        LT -> LT
                        GT -> GT
                        EQ -> case compare (gdtHour a) (gdtHour b) of
                                LT -> LT
                                GT -> GT
                                EQ -> case compare (gdtMinute a) (gdtMinute b) of
                                        LT -> LT
                                        GT -> GT
                                        EQ -> compare (gdtmSecond a) (gdtmSecond b)