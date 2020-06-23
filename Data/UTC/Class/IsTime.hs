{-# LANGUAGE Safe #-}
module Data.UTC.Class.IsTime
  ( IsTime (..)
  ) where

import Control.Monad.Catch

import Data.Ratio

import Data.UTC.Internal

-- | This class captures the concept of a 24-hour clock time
-- during a day.
class IsTime t where
  -- | Returns values in the range 0 to 23.
  hour                  :: t -> Integer
  -- | Returns values in the range 0 to 59.
  minute                :: t -> Integer
  -- | Returns values in the range 0 to 59.
  second                :: t -> Integer
  -- | Returns values in the range 0.0 <= x < 1.0.
  secondFraction        :: t -> Rational
  -- | Accepts values in the range 0 to 23.
  --
  -- The function fails if the result cannot be represented by the type (cannot happen for 'Data.UTC.Time' and 'Data.UTC.DateTime').
  setHour               :: (MonadThrow m) => Integer  -> t -> m t
  -- | Accepts values in the range 0 to 59.
  --
  -- The function fails if the result cannot be represented by the type (cannot happen for 'Data.UTC.Time' and 'Data.UTC.DateTime').
  setMinute             :: (MonadThrow m) => Integer  -> t -> m t
  -- | Accepts values in the range 0 to 59.
  --
  -- The function fails if the result cannot be represented by the type (cannot happen for 'Data.UTC.Time' and 'Data.UTC.DateTime').
  setSecond             :: (MonadThrow m) => Integer  -> t -> m t
  -- | Accepts values in the range 0.0 <= x < 1.0.
  --
  -- The function fails if the result cannot be represented by the type (cannot happen for 'Data.UTC.Time' and 'Data.UTC.DateTime').
  setSecondFraction     :: (MonadThrow m) => Rational -> t -> m t

  -- | Adds an arbitrary count of hours (positive or negative).
  --
  --   * Full days flow over to 'Data.UTC.addDays' if the type is also an instance of 'Data.UTC.Class.IsDate' (this is the case for 'Data.UTC.DateTime').
  --   * Types not implementing the 'Data.UTC.Class.IsDate' class should just ignore the days part on overflow (like 'Data.UTC.Time' does).
  --   * Fails if the result cannot be represented by the type (cannot happen for 'Data.UTC.Time' and 'Data.UTC.DateTime').
  addHours              :: (MonadThrow m) => Integer  -> t -> m t
  addHours h t
    = setHour hors t
    where
      h'   = h + (hour t)
      hors = h' `mod` hoursPerDay

  -- | Adds an arbitrary count of minutes (positive or negative).
  --
  --   * Full hours flow over to 'addHours'.
  --   * Fails if the result cannot be represented by the type (cannot happen for 'Data.UTC.Time' and 'Data.UTC.DateTime').
  addMinutes            :: (MonadThrow m) => Integer  -> t -> m t
  addMinutes m t
    = setMinute mins t >>= addHours hors
    where
      m'   = m + (minute t)
      mins = m' `mod` minsPerHour
      hors = m' `div` minsPerHour

  -- | Adds an arbitrary count of seconds (positive or negative).
  --
  --   * Full minutes flow over to 'addMinutes'.
  --   * Fails if the result cannot be represented by the type (cannot happen for 'Data.UTC.Time' and 'Data.UTC.DateTime').
  addSeconds            :: (MonadThrow m) => Integer  -> t -> m t
  addSeconds s t
    = setSecond secs t >>= addMinutes mins
    where
      s'   = s + (second t)
      secs = s' `mod` secsPerMinute
      mins = s' `div` secsPerMinute

  -- | Adds an arbitrary second fraction (positive or negative).
  --
  --   * Full seconds flow over to 'addSeconds'.
  --   * Instances of this class are not required to preserve full precision (although 'Data.UTC.Time' and 'Data.UTC.DateTime' do so).
  --   * Fails if the result cannot be represented by the type (cannot happen for 'Data.UTC.Time' and 'Data.UTC.DateTime').
  addSecondFractions    :: (MonadThrow m) => Rational -> t -> m t
  addSecondFractions f t
    | f == 0    = return t
    | f >= 0    = setSecondFraction frcs t         >>= addSeconds secs
    | frcs == 0 = setSecondFraction 0 t            >>= addSeconds secs
    | otherwise = setSecondFraction (frcs + 1.0) t >>= addSeconds (secs - 1)
    where
      f'   = f + (secondFraction t)
      frcs = f' - (truncate f' % 1)
      secs = truncate f'
