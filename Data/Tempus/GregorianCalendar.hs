{-# LANGUAGE Safe #-}
module Data.Tempus.GregorianCalendar
  ( GregorianCalendar(..)
  , LocalOffset(..)
  ) where

import Control.Monad

import Data.Int

class GregorianCalendar a where
  -- | > fromRfc3339String "0000-00-00T00:00:00Z"  == Just commonEpoch
  commonEpoch        :: a

  -- | > getYear        "2014-⁠12-⁠24T18:11:47.042Z" == Just 2014
  getYear            :: (MonadPlus m) => a -> m Int
  -- | > getMonth       "2014-⁠12-⁠24T18:11:47.042Z" == Just   12
  getMonth           :: (MonadPlus m) => a -> m Int
  -- | > getDay         "2014-⁠12-⁠24T18:11:47.042Z" == Just   24
  getDay             :: (MonadPlus m) => a -> m Int
  -- | > getHour        "2014-⁠12-⁠24T18:11:47.042Z" == Just   18
  getHour            :: (MonadPlus m) => a -> m Int
  -- | > getMinute      "2014-⁠12-⁠24T18:11:47.042Z" == Just   11
  getMinute          :: (MonadPlus m) => a -> m Int
  -- | > getSecond      "2014-⁠12-⁠24T18:11:47.042Z" == Just   47
  getSecond          :: (MonadPlus m) => a -> m Int
  -- | > getMilliSecond "2014-⁠12-⁠24T18:11:47.042Z" == Just   42
  getMilliSecond        :: (MonadPlus m) => a -> m Int

  setYear               :: (MonadPlus m) => Int -> a -> m a
  setMonth              :: (MonadPlus m) => Int -> a -> m a
  setDay                :: (MonadPlus m) => Int -> a -> m a
  setHour               :: (MonadPlus m) => Int -> a -> m a
  setMinute             :: (MonadPlus m) => Int -> a -> m a
  setSecond             :: (MonadPlus m) => Int -> a -> m a
  setMilliSecond        :: (MonadPlus m) => Int -> a -> m a

  getMilliSecondsCommonEpoch :: (MonadPlus m) => a -> m Int64
  setMilliSecondsCommonEpoch :: (MonadPlus m) => Int64 -> m a

  addYears              :: Int -> a -> Maybe a
  addMonths             :: Int -> a -> Maybe a
  addDays               :: Int -> a -> Maybe a
  addHours              :: Int -> a -> Maybe a
  addMinutes            :: Int -> a -> Maybe a
  addSeconds            :: Int -> a -> Maybe a
  addMilliSeconds       :: Int -> a -> Maybe a

class LocalOffset a where
  getLocalOffset        :: (MonadPlus m) => a -> m (Maybe Int)
  setLocalOffset        :: (MonadPlus m) => Maybe Int -> a -> m a