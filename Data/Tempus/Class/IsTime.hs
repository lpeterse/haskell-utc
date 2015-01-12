{-# LANGUAGE Safe #-}
module Data.Tempus.Class.IsTime
  ( IsTime (..)
  ) where

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
  -- | The beginning of a day: 00:00:00
  midnight :: t