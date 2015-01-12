{-# LANGUAGE Safe #-}
module Data.Tempus.Class.IsTime
  ( IsTime (..)
  ) where

import Data.Ratio

import Data.Tempus.Internal

class IsTime t where
  -- | > hour           "2014-⁠12-⁠24T18:11:47.042Z" ==          18
  hour                  :: t -> Integer
  -- | > minute         "2014-⁠12-⁠24T18:11:47.042Z" ==          11
  minute                :: t -> Integer
  -- | > second         "2014-⁠12-⁠24T18:11:47.042Z" ==          47
  second                :: t -> Integer
  -- | > secondFraction "2014-⁠12-⁠24T18:11:47.042Z" ==          42
  secondFraction        :: t -> Rational
  setHour               :: (Monad m) => Integer  -> t -> m t
  setMinute             :: (Monad m) => Integer  -> t -> m t
  setSecond             :: (Monad m) => Integer  -> t -> m t
  setSecondFraction     :: (Monad m) => Rational -> t -> m t

  addHours              :: (Monad m) => Integer  -> t -> m t
  addHours h t
    = if days /= 0
        then fail "IsTime.addHours: out of range"
        else setHour hors t
    where
      h'   = h + (hour t)
      hors = h' `mod` hoursPerDay
      days = h' `div` hoursPerDay

  addMinutes            :: (Monad m) => Integer  -> t -> m t
  addMinutes m t
    = setMinute mins t >>= addHours hors
    where
      m'   = m + (minute t)
      mins = m' `mod` minsPerHour
      hors = m' `div` minsPerHour

  addSeconds            :: (Monad m) => Integer  -> t -> m t
  addSeconds s t
    = setSecond secs t >>= addMinutes mins
    where
      s'   = s + (second t)
      secs = s' `mod` secsPerMinute
      mins = s' `div` secsPerMinute

  addSecondFractions    :: (Monad m) => Rational -> t -> m t
  addSecondFractions f t
    = setSecondFraction frcs t >>= addSeconds secs
    where
      f'   = f + (secondFraction t)
      frcs = f' - (truncate f' % 1)
      secs = truncate f'

  -- | The beginning of a day: 00:00:00
  midnight :: t