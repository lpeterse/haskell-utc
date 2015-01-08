{-# LANGUAGE Safe #-}
module Data.Tempus.GregorianTime
  ( GregorianTime(..)
  , LocalOffset(..)
  ) where

import Control.Monad

import Data.Ratio

import Data.Tempus.Internal

class GregorianTime a where
  -- | > fromRfc3339String "0000-00-00T00:00:00Z"  == Just commonEpoch
  commonEpoch        :: a

  -- | > getYear        "2014-⁠12-⁠24T18:11:47.042Z" == Just 2014
  year            :: a -> Integer
  -- | > getMonth       "2014-⁠12-⁠24T18:11:47.042Z" == Just   12
  month           :: a -> Integer
  -- | > getDay         "2014-⁠12-⁠24T18:11:47.042Z" == Just   24
  day             :: a -> Integer
  -- | > getHour        "2014-⁠12-⁠24T18:11:47.042Z" == Just   18
  hour            :: a -> Integer
  -- | > getMinute      "2014-⁠12-⁠24T18:11:47.042Z" == Just   11
  minute          :: a -> Integer
  -- | > getSecond      "2014-⁠12-⁠24T18:11:47.042Z" == Just   47
  second          :: a -> Integer
  -- | > getMilliSecond "2014-⁠12-⁠24T18:11:47.042Z" == Just   42
  secondFraction  :: a -> Rational

  setYear               :: (MonadPlus m) => Integer  -> a -> m a
  setMonth              :: (MonadPlus m) => Integer  -> a -> m a
  setDay                :: (MonadPlus m) => Integer  -> a -> m a
  setHour               :: (MonadPlus m) => Integer  -> a -> m a
  setMinute             :: (MonadPlus m) => Integer  -> a -> m a
  setSecond             :: (MonadPlus m) => Integer  -> a -> m a
  setSecondFraction     :: (MonadPlus m) => Rational -> a -> m a

  gregorianSeconds :: a -> Rational
  gregorianSeconds t
    = (days  * 24 * 60 * 60 % 1)
    + (hour t     * 60 * 60 % 1)
    + (minute t        * 60 % 1)
    + (second t             % 1)
    + (secondFraction t)
    where
      days = yearMonthDayToDays (year t, month t, day t)

  fromGregorianSeconds :: (MonadPlus m) => Rational -> m a
  fromGregorianSeconds s
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
  localOffset        :: a -> Maybe Integer
  setLocalOffset     :: (MonadPlus m) => Maybe Integer -> a -> m a