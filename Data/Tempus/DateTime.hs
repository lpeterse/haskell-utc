module Data.Tempus.DateTime
  ( -- * Type
    DateTime (..)
  ) where

import Data.String
import Data.Maybe

import Data.Tempus.Class.Epoch
import Data.Tempus.Class.IsDate
import Data.Tempus.Class.IsTime
import Data.Tempus.Class.IsUnixTime
import Data.Tempus.Local
import Data.Tempus.Date
import Data.Tempus.Time
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
data DateTime
   = DateTime
     { date  :: Date
     , time  :: Time
     } deriving (Eq, Ord)

instance Show DateTime where
  show = fromMaybe "1970-01-01T00:00:00-00:00" . renderRfc3339String . unknown

instance IsString DateTime where
  fromString = utc . fromMaybe epoch . parseRfc3339String

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

  midnight
    = epoch

