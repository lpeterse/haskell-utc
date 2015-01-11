module Data.Tempus.GregorianTimestamp
  ( -- * Type
    DateTime()
  -- * Creation
  ) where

import Data.Ratio
import Data.String
import Data.Maybe

import Data.Tempus.Epoch
import Data.Tempus.Date
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
     , gdtHour           :: Integer
     , gdtMinute         :: Integer
     , gdtSecond         :: Integer
     , gdtSecondFraction :: Rational
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
      , gdtHour           = 0
      , gdtMinute         = 0
      , gdtSecond         = 0
      , gdtSecondFraction = 0
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
         return $ DateTime
                  { gdtDate           = dt
                  , gdtHour           = truncate s `div` secsPerHour   `mod` hoursPerDay
                  , gdtMinute         = truncate s `div` secsPerMinute `mod` minsPerHour
                  , gdtSecond         = truncate s                     `mod` secsPerMinute
                  , gdtSecondFraction = s - (truncate s % 1)
                  , gdtOffset         = Nothing
                  }
    where
      s = u + deltaUnixEpochCommonEpoch

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
    = gdtHour
  minute
    = gdtMinute
  second
    = gdtSecond
  secondFraction
    = gdtSecondFraction

  setHour x t
    | x < 0 || 23 < x = fail   $ "Time.setHour "           ++ show x
    | otherwise       = return $ t { gdtHour           = x }
  setMinute x t
    | x < 0 || 59 < x = fail   $ "Time.setMinute "         ++ show x
    | otherwise       = return $ t { gdtMinute         = x }
  setSecond x t
    | x < 0 || 59 < x = fail   $ "Time.setSecond "         ++ show x
    | otherwise       = return $ t { gdtSecond         = x }
  setSecondFraction x t
    | x < 0 || 1 <= x = fail   $ "Time.setSecondFraction " ++ show x
    | otherwise       = return $ t { gdtSecondFraction = x }

  midnight
    = epoch

instance LocalOffset DateTime where
  localOffset
    = gdtOffset
  setLocalOffset mm gt
    = return $ gt { gdtOffset = mm }
