{-# LANGUAGE Safe #-}
module Data.Tempus.GregorianTime
  ( Dated(..)
  , Timed(..)
  ) where

import Data.Tempus.Epoch

class Epoch t => Dated t where
  -- | > year  "2014-⁠12-⁠24" ==  2014
  year                  :: t -> Integer
  -- | > month "2014-⁠12-⁠24" ==    12
  month                 :: t -> Integer
  -- | > day   "2014-⁠12-⁠24" ==    24
  day                   :: t -> Integer
  setYear               :: (Monad m) => Integer  -> t -> m t
  setMonth              :: (Monad m) => Integer  -> t -> m t
  setDay                :: (Monad m) => Integer  -> t -> m t

class Timed t where
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
  -- | The beginning of a day: 00:00:00
  midnight :: t