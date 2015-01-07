{-# LANGUAGE Safe #-}
module Data.Tempus.GregorianCalendar
  ( GregorianCalendar(..)
  , LocalOffset(..)
  ) where

import Control.Monad

import Data.Ratio

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
  getSecondFraction  :: (MonadPlus m) => a -> m Rational

  setYear               :: (MonadPlus m) => Integer -> a -> m a
  setMonth              :: (MonadPlus m) => Integer -> a -> m a
  setDay                :: (MonadPlus m) => Integer -> a -> m a
  setHour               :: (MonadPlus m) => Integer -> a -> m a
  setMinute             :: (MonadPlus m) => Integer -> a -> m a
  setSecond             :: (MonadPlus m) => Integer -> a -> m a
  setSecondFraction     :: (MonadPlus m) => Rational -> a -> m a

  toSecondsSinceCommonEpoch :: (MonadPlus m) => a -> m Rational
  toSecondsSinceCommonEpoch t
    = do year    <- getYear        t
         month   <- getMonth       t
         day     <- getDay         t
         hour    <- getHour        t
         minute  <- getMinute      t
         second  <- getSecond      t
         secfrac <- getSecondFraction t
         days    <- yearMonthDayToDays (year, month, day)
         return $ (days * 24 * 60 * 60 % 1)
                + (hour      * 60 * 60 % 1)
                + (minute         * 60 % 1)
                + (second              % 1)
                + secfrac

  fromSecondsSinceCommonEpoch :: (MonadPlus m) => Rational -> m a
  fromSecondsSinceCommonEpoch s
    = do (year, month, day) <- daysToYearMonthDay (truncate s `div` (24 * 60 * 60))
         let hour            = truncate s `div` (60 * 60) `mod` 24
         let minute          = truncate s `div`       60  `mod` 60
         let second          = truncate s                 `mod` 60
         let secfrac         = s - (truncate s % 1)
         return commonEpoch 
           >>= setYear           year
           >>= setMonth          month
           >>= setDay            day
           >>= setHour           hour
           >>= setMinute         minute
           >>= setSecond         second
           >>= setSecondFraction secfrac

class LocalOffset a where
  getLocalOffset        :: (MonadPlus m) => a -> m (Maybe Integer)
  setLocalOffset        :: (MonadPlus m) => Maybe Integer -> a -> m a