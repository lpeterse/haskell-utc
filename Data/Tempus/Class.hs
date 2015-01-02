module Data.Tempus.Class where

import Control.Monad

class Tempus a where
  -- | > getYears   "2014-⁠12-⁠24T18:11:47Z" == 2014
  getYears        :: (MonadPlus m) => a -> m Int
  -- | > getMonths  "2014-⁠12-⁠24T18:11:47Z" == 12
  getMonths       :: (MonadPlus m) => a -> m Int
  -- | > getDays    "2014-⁠12-⁠24T18:11:47Z" == 24
  getDays         :: (MonadPlus m) => a -> m Int
  -- | > getHours   "2014-⁠12-⁠24T18:11:47Z" == 18
  getHours        :: (MonadPlus m) => a -> m Int
  -- | > getMinutes "2014-⁠12-⁠24T18:11:47Z" == 11
  getMinutes      :: (MonadPlus m) => a -> m Int
  -- | > getSeconds "2014-⁠12-⁠24T18:11:47Z" == 47
  getSeconds      :: (MonadPlus m) => a -> m Int
  -- | > getMillis  "2014-⁠12-⁠24T18:11:47Z" == 0
  getMilliSeconds :: (MonadPlus m) => a -> m Int
  setYears        :: Int -> a -> Maybe a
  setMonths       :: Int -> a -> Maybe a
  setDays         :: Int -> a -> Maybe a
  setHours        :: Int -> a -> Maybe a
  setMinutes      :: Int -> a -> Maybe a
  setSeconds      :: Int -> a -> Maybe a
  setMilliSeconds :: Int -> a -> Maybe a
  addYears        :: Int -> a -> Maybe a
  addMonths       :: Int -> a -> Maybe a
  addDays         :: Int -> a -> Maybe a
  addHours        :: Int -> a -> Maybe a
  addMinutes      :: Int -> a -> Maybe a
  addSeconds      :: Int -> a -> Maybe a
  addMilliSeconds :: Int -> a -> Maybe a
