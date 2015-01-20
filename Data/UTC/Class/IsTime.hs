{-# LANGUAGE Safe #-}
module Data.UTC.Class.IsTime
  ( IsTime (..)
  ) where

import Data.Ratio

import Data.UTC.Internal

class IsTime t where
  hour                  :: t -> Integer
  minute                :: t -> Integer
  second                :: t -> Integer
  secondFraction        :: t -> Rational
  setHour               :: (Monad m) => Integer  -> t -> m t
  setMinute             :: (Monad m) => Integer  -> t -> m t
  setSecond             :: (Monad m) => Integer  -> t -> m t
  setSecondFraction     :: (Monad m) => Rational -> t -> m t

  addHours              :: (Monad m) => Integer  -> t -> m t
  addHours h t
    = setHour hors t
    where
      h'   = h + (hour t)
      hors = h' `mod` hoursPerDay

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
    | f == 0    = return t
    | f >= 0    = setSecondFraction frcs t         >>= addSeconds secs
    | otherwise = setSecondFraction (frcs + 1.0) t >>= addSeconds (secs - 1)
    where
      f'   = f + (secondFraction t)
      frcs = f' - (truncate f' % 1)
      secs = truncate f'
