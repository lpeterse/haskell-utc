{-# LANGUAGE Safe #-}
module Data.UTC.Class.IsDate
  ( IsDate (..)
  ) where

import Data.UTC.Internal
import Data.UTC.Class.Epoch

-- | This class captures the behaviour of the __Proleptic Gregorian Calendar__.
--
-- Without any exception the following holds:
--
--   * A regular year has 365 days and the corresponding February has 28 days.
--   * A leap year has 366 days and the corresponding February has 29 days.
--   * A year that is a multiple of 400 is a leap year.
--   * A year that is a multiple of 100 but not of 400 is not a leap year.
--   * A year that is a multiple of 4 but not of 100 is a leap year.
class Epoch t => IsDate t where
  -- | > year  "2014-⁠12-⁠24" == 2014
  -- For negative years the function assumes astronomical year
  -- numbering (year 1 ~ 1 AD, year 0 ~ 1 BC, year -1 ~ 2 BC etc).
  -- Note that 1 BC and 5 BC are therefore leap years.
  year                  :: t -> Integer
  -- | > month "2014-⁠12-⁠24" == 12
  -- The function only returns values ranging from 1 to 12.
  month                 :: t -> Integer
  -- | > day   "2014-⁠12-⁠24" == 24
  -- The function only returns values ranging from 1 to 31.
  day                   :: t -> Integer

  -- | Sets the year and fails if the result would be invalid.
  --
  -- > setYear 2005 "2004-02-28" :: Maybe Date
  -- > > Just 2005-02-28
  -- > setYear 2005 "2004-02-29" :: Maybe Date
  -- > > Nothing
  setYear               :: (Monad m) => Integer  -> t -> m t
  -- | Sets the month of year and fails if the result would be invalid.
  --
  -- The function only accepts input ranging from 1 to 12.
  setMonth              :: (Monad m) => Integer  -> t -> m t
  -- | Sets the day of month and fails if the result would be invalid.
  --
  -- The function only accepts input ranging from 1 to 31 (or less depending on month and year).
  setDay                :: (Monad m) => Integer  -> t -> m t

  -- | A /year/ is a relative amount of time.
  -- The function's semantic is a follows:
  --
  --   * The years (positive or negative) are added.
  --   * If the target date is invalid then days are subtracted until the date gets valid.
  --   * If the resulting date is out of the instance type's range, the function fails
  --     (cannot happen for 'Data.UTC.Date' and 'Data.UTC.DateTime' as they use 
  --     multiprecision integers).
  --
  -- > addYears 4 "2000-02-29" :: Maybe Date
  -- > > Just 2004-02-29
  -- > addYears 1 "2000-02-29" :: Maybe Date
  -- > > Just 2001-02-28
  addYears              :: (Monad m) => Integer  -> t -> m t
  addYears ys t
    = if isValidDate (year t + ys, month t, day t)
        then setYear (year t + ys) t
        else setYear (year t + ys) =<< setDay (day t - 1) t

  -- | A /month/ is a relative amount of time.
  -- The function's semantic is equivalent to that of 'addYears'.
  --
  -- The function fails if the resulting date is out of the instance type's range
  -- (cannot happen for 'Data.UTC.Date' and 'Data.UTC.DateTime' as they use 
  --  multiprecision integers).
  --
  -- > addMonths (-13) "1970-01-01" :: Maybe Date
  -- > > Just 1968-12-01
  addMonths             :: (Monad m) => Integer  -> t -> m t
  addMonths ms t
    = setDay 1 t >>= setYear y >>= setMonth m >>= setDay d
    where
      ym = (year t * monthsPerYear)
         + (month t - 1)
         + ms
      y  = ym `div` monthsPerYear
      m  = (ym `mod` monthsPerYear) + 1
      d' = day t
      d  | isValidDate (y, m, d')     = d'
         | isValidDate (y, m, d' - 1) = d' - 1
         | isValidDate (y, m, d' - 2) = d' - 2
         | otherwise                  = d' - 3 -- was 31, now 28

  -- | A /day/ is an absolute amount of time. There is no surprise to expect.
  --
  -- The function fails if the resulting date is out of the instance type's range
  -- (cannot happen for 'Data.UTC.Date' and 'Data.UTC.DateTime' as they use 
  --  multiprecision integers).
  --
  -- > addDays 365 "1970-01-01" :: Maybe Date
  -- > > Just 1971-01-01
  -- > addDays 365 "2000-01-01" :: Maybe Date
  -- > > Just 2000-12-31
  addDays               :: (Monad m) => Integer  -> t -> m t
  addDays ds t
    -- setDay 1 to avoid intermediate generation of invalid dates!
    = setDay 1 t >>= setYear y >>= setMonth m >>= setDay d
    where
      ds'       = yearMonthDayToDays (year t, month t, day t)
      (y, m, d) = daysToYearMonthDay (ds' + ds)