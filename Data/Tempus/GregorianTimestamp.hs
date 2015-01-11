module Data.Tempus.GregorianTimestamp
  ( -- * Type
    DateTime (..)
  -- * Creation
  ) where

import Data.Ratio
import Data.String
import Data.Maybe

import Data.Tempus.Epoch
import Data.Tempus.Date
import Data.Tempus.Time
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
data DateTime
   = DateTime
     { gdtDate           :: Date
     , gdtTime           :: Time
     , gdtOffset         :: (Maybe Rational)
     }

instance Eq DateTime where
  (==) a b
    = (==)
       (toUnixSeconds a)
       (toUnixSeconds b)

instance Ord DateTime where
  compare a b
    = compare
       (toUnixSeconds a)
       (toUnixSeconds b)

instance Show DateTime where
  show = fromMaybe "1970-01-01T00:00:00-00:00" . renderRfc3339String

instance IsString DateTime where
  fromString = fromMaybe epoch . parseRfc3339String

instance Epoch DateTime where
  epoch
    = DateTime
      { gdtDate           = epoch
      , gdtTime           = midnight
      , gdtOffset         = Nothing
      }

instance UnixTime DateTime where
  toUnixSeconds t
    = (days       * secsPerDay    % 1)
    + (hour t     * secsPerHour   % 1)
    + (minute t   * secsPerMinute % 1)
    + (second t                   % 1)
    + (secondFraction t)
    - (fromMaybe 0 $ localOffset t)
    - deltaUnixEpochCommonEpoch
    where
      days = yearMonthDayToDays (year (gdtDate t), month (gdtDate t), day (gdtDate t))
  fromUnixSeconds u
    = do dt <- fromUnixSeconds u
         tm <- fromUnixSeconds u
         return $ DateTime
                  { gdtDate           = dt
                  , gdtTime           = tm
                  , gdtOffset         = Nothing
                  }

instance Dated DateTime where
  year
    = year . gdtDate
  month
    = month . gdtDate
  day
    = day . gdtDate
  setYear y t
    = do dt <- setYear y (gdtDate t)
         return $ t { gdtDate = dt }
  setMonth m t
    = do dt <- setMonth m (gdtDate t)
         return $ t { gdtDate = dt }
  setDay d t
    = do dt <- setDay d (gdtDate t)
         return $ t { gdtDate = dt }

instance Timed DateTime where
  hour
    = hour . gdtTime
  minute
    = minute . gdtTime
  second
    = second . gdtTime
  secondFraction
    = secondFraction . gdtTime
  setHour y t
    = do tm <- setHour y (gdtTime t)
         return $ t { gdtTime = tm }
  setMinute y t
    = do tm <- setMinute y (gdtTime t)
         return $ t { gdtTime = tm }
  setSecond y t
    = do tm <- setSecond y (gdtTime t)
         return $ t { gdtTime = tm }
  setSecondFraction y t
    = do tm <- setSecondFraction y (gdtTime t)
         return $ t { gdtTime = tm }

  midnight
    = epoch

instance LocalOffset DateTime where
  localOffset
    = gdtOffset
  setLocalOffset mm gt
    = return $ gt { gdtOffset = mm }
