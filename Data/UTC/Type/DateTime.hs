{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.UTC.Type.DateTime
  ( -- * Type
    DateTime (..)
  ) where

import Data.Maybe

import Data.UTC.Class.Epoch
import Data.UTC.Class.IsDate
import Data.UTC.Class.IsTime
import Data.UTC.Class.IsUnixTime
import Data.UTC.Type.Date
import Data.UTC.Type.Time
import Data.UTC.Type.Local
import Data.UTC.Internal

-- | A time representation based on a 'Data.UTC.Date' and the 'Data.UTC.Time' of the day.
--
--   * The type uses multiprecision integers internally and is able to represent
--     any UTC date in the past and in the future with arbitrary precision
--     (apart from the time span within a leap second).
--   * The instance of 'Prelude.Show' is only
--     meant for debugging purposes. Don't rely on its behaviour!
--
-- > > show (epoch :: DateTime)
-- > 1970-01-01T00:00:00
data DateTime
   = DateTime
     { date  :: Date
     , time  :: Time
     } deriving (Eq, Ord)

instance Show DateTime where
  show (DateTime d t)
    = show d ++ "T" ++ show t

instance Epoch DateTime where
  epoch = DateTime epoch epoch

instance IsUnixTime DateTime where
  unixSeconds (DateTime d t)
    = unixSeconds d
    + unixSeconds t
  fromUnixSeconds u
    = do d <- fromUnixSeconds u
         t <- fromUnixSeconds u
         return (DateTime d t)

instance IsDate DateTime where
  year
    = year . date
  month
    = month . date
  day
    = day . date
  setYear y t
    = do dt <- setYear y (date t)
         return $ t { date = dt }
  setMonth m t
    = do dt <- setMonth m (date t)
         return $ t { date = dt }
  setDay d t
    = do dt <- setDay d (date t)
         return $ t { date = dt }

instance IsTime DateTime where
  hour
    = hour . time
  minute
    = minute . time
  second
    = second . time
  secondFraction
    = secondFraction . time
  setHour y t
    = do tm <- setHour y (time t)
         return $ t { time = tm }
  setMinute y t
    = do tm <- setMinute y (time t)
         return $ t { time = tm }
  setSecond y t
    = do tm <- setSecond y (time t)
         return $ t { time = tm }
  setSecondFraction y t
    = do tm <- setSecondFraction y (time t)
         return $ t { time = tm }

  -- The default implementation of addHours fails whena day flows over.
  -- For DateTimes we can let it ripple into the days.
  addHours h t
    = setHour hors t >>= addDays days
    where
      h'   = h + (hour t)
      hors = h' `mod` hoursPerDay
      days = h' `div` hoursPerDay

-- assumption: addSecondFractions for DateTime is always successful
instance IsDate (Local DateTime) where
  year (Local Nothing t)
    = year t
  year (Local (Just 0) t)
    = year t
  year (Local (Just o) t)
    = year
    $ fromMaybe undefined $ addSecondFractions o t
  month (Local Nothing t)
    = month t
  month (Local (Just 0) t)
    = month t
  month (Local (Just o) t)
    = month
    $ fromMaybe undefined $ addSecondFractions o t
  day (Local Nothing t)
    = day t
  day (Local (Just 0) t)
    = day t
  day  (Local (Just o) t)
    = day
    $ fromMaybe undefined $ addSecondFractions o t
  setYear h (Local o@Nothing t)
    = setYear h t >>= return . Local o
  setYear h (Local o@(Just 0) t)
    = setYear h t >>= return . Local o
  setYear h (Local o@(Just i) t)
    = addSecondFractions i t >>= setYear h >>= addSecondFractions (negate i) >>= return . Local o
  setMonth h (Local o@Nothing t)
    = setMonth h t >>= return . Local o
  setMonth h (Local o@(Just 0) t)
    = setMonth h t >>= return . Local o
  setMonth h (Local o@(Just i) t)
    = addSecondFractions i t >>= setMonth h >>= addSecondFractions (negate i) >>= return . Local o
  setDay h (Local o@Nothing t)
    = setDay h t >>= return . Local o
  setDay h (Local o@(Just 0) t)
    = setDay h t >>= return . Local o
  setDay h (Local o@(Just i) t)
    = addSecondFractions i t >>= setDay h >>= addSecondFractions (negate i) >>= return . Local o

-- assumption: addSecondFractions for DateTime is always successful
instance IsTime (Local DateTime) where
  hour (Local Nothing t)
    = hour t
  hour (Local (Just 0) t)
    = hour t
  hour (Local (Just o) t)
    = hour
    $ fromMaybe undefined $ addSecondFractions o t
  minute (Local Nothing t)
    = minute t
  minute (Local (Just 0) t)
    = minute t
  minute (Local (Just o) t)
    = minute
    $ fromMaybe undefined $ addSecondFractions o t
  second (Local Nothing t)
    = second t
  second (Local (Just 0) t)
    = second t
  second  (Local (Just o) t)
    = second
    $ fromMaybe undefined $ addSecondFractions o t
  secondFraction (Local Nothing t)
    = secondFraction t
  secondFraction (Local (Just 0) t)
    = secondFraction t
  secondFraction (Local (Just o) t)
    = secondFraction
    $ fromMaybe undefined $ addSecondFractions o t
  setHour h (Local o@Nothing t)
    = setHour h t >>= return . Local o
  setHour h (Local o@(Just 0) t)
    = setHour h t >>= return . Local o
  setHour h (Local o@(Just i) t)
    = addSecondFractions i t >>= setHour h >>= addSecondFractions (negate i) >>= return . Local o
  setMinute h (Local o@Nothing t)
    = setMinute h t >>= return . Local o
  setMinute h (Local o@(Just 0) t)
    = setMinute h t >>= return . Local o
  setMinute h (Local o@(Just i) t)
    = addSecondFractions i t >>= setMinute h >>= addSecondFractions (negate i) >>= return . Local o
  setSecond h (Local o@Nothing t)
    = setSecond h t >>= return . Local o
  setSecond h (Local o@(Just 0) t)
    = setSecond h t >>= return . Local o
  setSecond h (Local o@(Just i) t)
    = addSecondFractions i t >>= setSecond h >>= addSecondFractions (negate i) >>= return . Local o
  setSecondFraction h (Local o@Nothing t)
    = setSecondFraction h t >>= return . Local o
  setSecondFraction h (Local o@(Just 0) t)
    = setSecondFraction h t >>= return . Local o
  setSecondFraction h (Local o@(Just i) t)
    = addSecondFractions i t >>= setSecondFraction h >>= addSecondFractions (negate i) >>= return . Local o

  -- This one is necessary to override, because the overflow should
  -- ripple into the date part.
  addHours h t
    = setHour hors t >>= addDays days
    where
      h'   = h + (hour t)
      days = h' `div` hoursPerDay
      hors = h' `mod` hoursPerDay
