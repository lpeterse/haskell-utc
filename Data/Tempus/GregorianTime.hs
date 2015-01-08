{-# LANGUAGE Safe #-}
module Data.Tempus.GregorianTime
  ( GregorianTime(..)
  , LocalOffset(..)
  ) where

import Control.Monad

import Data.Ratio

import Data.Tempus.Internal

class GregorianTime t where
  -- | > getYear        "2014-⁠12-⁠24T18:11:47.042Z" == Just 2014
  year            :: t -> Integer
  year t
    = let (y,_,_) = daysToYearMonthDay (truncate (toSecondsSinceCommonEpoch t) `div` (24 * 60 * 60))
      in  y
  -- | > getMonth       "2014-⁠12-⁠24T18:11:47.042Z" == Just   12
  month           :: t -> Integer
  month t
    = let (_,m,_) = daysToYearMonthDay (truncate (toSecondsSinceCommonEpoch t) `div` (24 * 60 * 60))
      in  m
  -- | > getDay         "2014-⁠12-⁠24T18:11:47.042Z" == Just   24
  day             :: t -> Integer
  day t
    = let (_,_,d) = daysToYearMonthDay (truncate (toSecondsSinceCommonEpoch t) `div` (24 * 60 * 60))
      in  d
  -- | > getHour        "2014-⁠12-⁠24T18:11:47.042Z" == Just   18
  hour            :: t -> Integer
  hour t
    = truncate (toSecondsSinceCommonEpoch t) `div` (60 * 60) `mod` 24
  -- | > getMinute      "2014-⁠12-⁠24T18:11:47.042Z" == Just   11
  minute          :: t -> Integer
  minute t
    = truncate (toSecondsSinceCommonEpoch t) `div`       60  `mod` 60
  -- | > getSecond      "2014-⁠12-⁠24T18:11:47.042Z" == Just   47
  second          :: t -> Integer
  second t
    = truncate (toSecondsSinceCommonEpoch t)                 `mod` 60
  -- | > getMilliSecond "2014-⁠12-⁠24T18:11:47.042Z" == Just   42
  secondFraction  :: t -> Rational
  secondFraction t
    = let s = toSecondsSinceCommonEpoch t
      in  s - (truncate s % 1)

  setYear               :: (MonadPlus m) => Integer  -> t -> m t
  setMonth              :: (MonadPlus m) => Integer  -> t -> m t
  setDay                :: (MonadPlus m) => Integer  -> t -> m t
  setHour               :: (MonadPlus m) => Integer  -> t -> m t
  setMinute             :: (MonadPlus m) => Integer  -> t -> m t
  setSecond             :: (MonadPlus m) => Integer  -> t -> m t
  setSecondFraction     :: (MonadPlus m) => Rational -> t -> m t

  -- | > fromRfc3339String "0000-00-00T00:00:00Z"  == Just commonEpoch
  getCommonEpoch              :: (MonadPlus m) => m t
  getCommonEpoch
    = fromSecondsSinceCommonEpoch 0

  toSecondsSinceCommonEpoch   :: t -> Rational
  fromSecondsSinceCommonEpoch :: (MonadPlus m) => Rational -> m t

class LocalOffset a where
  localOffset        :: a -> Maybe Integer
  setLocalOffset     :: (MonadPlus m) => Maybe Integer -> a -> m a