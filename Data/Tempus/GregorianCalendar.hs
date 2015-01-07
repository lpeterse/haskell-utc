{-# LANGUAGE Safe #-}
module Data.Tempus.GregorianCalendar
  ( GregorianCalendar(..)
  , LocalOffset(..)
  ) where

import Control.Monad

import Data.Tempus.Internal

class GregorianCalendar a where
  -- | > fromRfc3339String "0000-00-00T00:00:00Z"  == Just commonEpoch
  commonEpoch        :: a

  -- | > getYear        "2014-⁠12-⁠24T18:11:47.042Z" == Just 2014
  getYear            :: (MonadPlus m) => a -> m Integer
  -- | > getMonth       "2014-⁠12-⁠24T18:11:47.042Z" == Just   12
  getMonth           :: (MonadPlus m) => a -> m Integer
  -- | > getDay         "2014-⁠12-⁠24T18:11:47.042Z" == Just   24
  getDay             :: (MonadPlus m) => a -> m Integer
  -- | > getHour        "2014-⁠12-⁠24T18:11:47.042Z" == Just   18
  getHour            :: (MonadPlus m) => a -> m Integer
  -- | > getMinute      "2014-⁠12-⁠24T18:11:47.042Z" == Just   11
  getMinute          :: (MonadPlus m) => a -> m Integer
  -- | > getSecond      "2014-⁠12-⁠24T18:11:47.042Z" == Just   47
  getSecond          :: (MonadPlus m) => a -> m Integer
  -- | > getMilliSecond "2014-⁠12-⁠24T18:11:47.042Z" == Just   42
  getMilliSecond        :: (MonadPlus m) => a -> m Integer

  setYear               :: (MonadPlus m) => Integer -> a -> m a
  setMonth              :: (MonadPlus m) => Integer -> a -> m a
  setDay                :: (MonadPlus m) => Integer -> a -> m a
  setHour               :: (MonadPlus m) => Integer -> a -> m a
  setMinute             :: (MonadPlus m) => Integer -> a -> m a
  setSecond             :: (MonadPlus m) => Integer -> a -> m a
  setMilliSecond        :: (MonadPlus m) => Integer -> a -> m a

  toMilliSecondsSinceCommonEpoch :: (MonadPlus m) => a -> m Integer
  toMilliSecondsSinceCommonEpoch t
    = do year    <- getYear        t
         month   <- getMonth       t
         day     <- getDay         t
         hour    <- getHour        t
         minute  <- getMinute      t
         second  <- getSecond      t
         msecond <- getMilliSecond t
         days    <- yearMonthDayToDays (year, month, day)
         return $ (days  * 24 * 60 * 60 * 1000)
                + (hour       * 60 * 60 * 1000)
                + (minute          * 60 * 1000)
                + (second               * 1000)
                + (msecond                    )

  fromMilliSecondsSinceCommonEpoch :: (MonadPlus m) => Integer -> m a
  fromMilliSecondsSinceCommonEpoch ms
    = do (year, month, day)       <- daysToYearMonthDay (ms `div` (24 * 60 * 60 * 1000))
         let hour                  = ms `div` (60 * 60 * 1000) `mod` 24
         let minute                = ms `div` (     60 * 1000) `mod` 60
         let second                = ms `div` (          1000) `mod` 60
         let msecond               = ms `div` (             1) `mod` 1000
         return commonEpoch 
           >>= setYear        year
           >>= setMonth       month
           >>= setDay         day
           >>= setHour        hour
           >>= setMinute      minute
           >>= setSecond      second
           >>= setMilliSecond msecond

class LocalOffset a where
  getLocalOffset        :: (MonadPlus m) => a -> m (Maybe Integer)
  setLocalOffset        :: (MonadPlus m) => Maybe Integer -> a -> m a