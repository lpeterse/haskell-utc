module Data.Tempus.Class where

import Control.Monad

class Tempus a where
  now             :: IO a

  -- | > getYears   "2014-⁠12-⁠24T18:11:47Z" == 2014
  getYear         :: (MonadPlus m) => a -> m Int
  -- | > getMonths  "2014-⁠12-⁠24T18:11:47Z" == 12
  getMonth        :: (MonadPlus m) => a -> m Int
  -- | > getDays    "2014-⁠12-⁠24T18:11:47Z" == 24
  getDay          :: (MonadPlus m) => a -> m Int
  -- | > getHours   "2014-⁠12-⁠24T18:11:47Z" == 18
  getHour         :: (MonadPlus m) => a -> m Int
  -- | > getMinutes "2014-⁠12-⁠24T18:11:47Z" == 11
  getMinute       :: (MonadPlus m) => a -> m Int
  -- | > getSeconds "2014-⁠12-⁠24T18:11:47Z" == 47
  getSecond       :: (MonadPlus m) => a -> m Int
  -- | > getMillis  "2014-⁠12-⁠24T18:11:47Z" == 0
  getMilliSecond  :: (MonadPlus m) => a -> m Int
  setYear         :: Int -> a -> Maybe a
  setMonth        :: Int -> a -> Maybe a
  setDay          :: Int -> a -> Maybe a
  setHour         :: Int -> a -> Maybe a
  setMinute       :: Int -> a -> Maybe a
  setSecond       :: Int -> a -> Maybe a
  setMilliSecond  :: Int -> a -> Maybe a
  addYears        :: Int -> a -> Maybe a
  addMonths       :: Int -> a -> Maybe a
  addDays         :: Int -> a -> Maybe a
  addHours        :: Int -> a -> Maybe a
  addMinutes      :: Int -> a -> Maybe a
  addSeconds      :: Int -> a -> Maybe a
  addMilliSeconds :: Int -> a -> Maybe a
