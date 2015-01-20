{-# LANGUAGE FlexibleInstances #-}
module Data.UTC.Type.DateTime
  ( -- * Type
    DateTime (..)
  ) where

import Data.String
import Data.Maybe

import Data.UTC.Class.Epoch
import Data.UTC.Class.IsDate
import Data.UTC.Class.IsTime
import Data.UTC.Class.IsUnixTime
import Data.UTC.Type.Date
import Data.UTC.Type.Time
import Data.UTC.Type.Local
import Data.UTC.Format.Rfc3339
import Data.UTC.Internal

-- | A time representation based on years, months, days, hours, minutes, seconds and second fractions. 
--
--   * The type uses multiprecision integers internally and is able to represent
--     any UTC date in the past and in the future with arbitrary precision
--     (apart from the time span within a leap second).
--   * The instances for 'Data.String.IsString' and 'Prelude.Show' are only
--     meant for debugging purposes and default to 'epoch' in case of
--     failure. Don't rely on their behaviour!
data DateTime
   = DateTime
     { date  :: Date
     , time  :: Time
     } deriving (Eq, Ord)

instance Show DateTime where
  show = fromMaybe "1970-01-01T00:00:00-00:00" . renderRfc3339 . unknown

instance Show (Local DateTime) where
  show = fromMaybe "1970-01-01T00:00:00-00:00" . renderRfc3339

instance IsString DateTime where
  fromString = utc . fromMaybe epoch . parseRfc3339

instance Epoch DateTime where
  epoch = DateTime epoch midnight

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
  year (Local t Nothing)
    = year t
  year (Local t (Just 0))
    = year t
  year (Local t (Just o))
    = year
    $ fromMaybe undefined $ addSecondFractions o t
  month (Local t Nothing)
    = month t
  month (Local t (Just 0))
    = month t
  month (Local t (Just o))
    = month
    $ fromMaybe undefined $ addSecondFractions o t
  day (Local t Nothing)
    = day t
  day (Local t (Just 0))
    = day t
  day  (Local t (Just o))
    = day
    $ fromMaybe undefined $ addSecondFractions o t
  setYear h (Local t o@Nothing)
    = do t' <- setYear h t
         return (Local t' o)
  setYear h (Local t o@(Just 0))
    = do t' <- setYear h t
         return (Local t' o)
  setYear h (Local t o@(Just i))
    = do t' <- addSecondFractions i t >>= setYear h >>= addSecondFractions (negate i)
         return (Local t' o)
  setMonth h (Local t o@Nothing)
    = do t' <- setMonth h t
         return (Local t' o)
  setMonth h (Local t o@(Just 0))
    = do t' <- setMonth h t
         return (Local t' o)
  setMonth h (Local t o@(Just i))
    = do t' <- addSecondFractions i t >>= setMonth h >>= addSecondFractions (negate i)
         return (Local t' o)
  setDay h (Local t o@Nothing)
    = do t' <- setDay h t
         return (Local t' o)
  setDay h (Local t o@(Just 0))
    = do t' <- setDay h t
         return (Local t' o)
  setDay h (Local t o@(Just i))
    = do t' <- addSecondFractions i t >>= setDay h >>= addSecondFractions (negate i)
         return (Local t' o)

-- assumption: addSecondFractions for DateTime is always successful
instance IsTime (Local DateTime) where
  hour (Local t Nothing)
    = hour t
  hour (Local t (Just 0))
    = hour t
  hour (Local t (Just o))
    = hour
    $ fromMaybe undefined $ addSecondFractions o t
  minute (Local t Nothing)
    = minute t
  minute (Local t (Just 0))
    = minute t
  minute (Local t (Just o))
    = minute
    $ fromMaybe undefined $ addSecondFractions o t
  second (Local t Nothing)
    = second t
  second (Local t (Just 0))
    = second t
  second  (Local t (Just o))
    = second
    $ fromMaybe undefined $ addSecondFractions o t
  secondFraction (Local t Nothing)
    = secondFraction t
  secondFraction (Local t (Just 0))
    = secondFraction t
  secondFraction (Local t (Just o))
    = secondFraction
    $ fromMaybe undefined $ addSecondFractions o t
  setHour h (Local t o@Nothing)
    = do t' <- setHour h t
         return (Local t' o)
  setHour h (Local t o@(Just 0))
    = do t' <- setHour h t
         return (Local t' o)
  setHour h (Local t o@(Just i))
    = do t' <- addSecondFractions i t >>= setHour h >>= addSecondFractions (negate i)
         return (Local t' o)
  setMinute h (Local t o@Nothing)
    = do t' <- setMinute h t
         return (Local t' o)
  setMinute h (Local t o@(Just 0))
    = do t' <- setMinute h t
         return (Local t' o)
  setMinute h (Local t o@(Just i))
    = do t' <- addSecondFractions i t >>= setMinute h >>= addSecondFractions (negate i)
         return (Local t' o)
  setSecond h (Local t o@Nothing)
    = do t' <- setSecond h t
         return (Local t' o)
  setSecond h (Local t o@(Just 0))
    = do t' <- setSecond h t
         return (Local t' o)
  setSecond h (Local t o@(Just i))
    = do t' <- addSecondFractions i t >>= setSecond h >>= addSecondFractions (negate i)
         return (Local t' o)
  setSecondFraction h (Local t o@Nothing)
    = do t' <- setSecondFraction h t
         return (Local t' o)
  setSecondFraction h (Local t o@(Just 0))
    = do t' <- setSecondFraction h t
         return (Local t' o)
  setSecondFraction h (Local t o@(Just i))
    = do t' <- addSecondFractions i t >>= setSecondFraction h >>= addSecondFractions (negate i)
         return (Local t' o)

  -- This one is necessary to override, because the overflow should
  -- ripple into the date part.
  addHours h t
    = setHour hors t >>= addDays days
    where
      h'   = h + (hour t)
      days = h' `div` hoursPerDay
      hors = h' `mod` hoursPerDay