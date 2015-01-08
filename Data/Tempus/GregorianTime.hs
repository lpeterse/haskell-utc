{-# LANGUAGE Safe #-}
module Data.Tempus.GregorianTime
  ( GregorianTime(..)
  , LocalOffset(..)
  ) where

import Control.Monad

class GregorianTime t where
  -- | >                "0000-00-00T00:00:00Z"     == commonEpoch
  commonEpoch     :: t

  -- | > year           "2014-⁠12-⁠24T18:11:47.042Z" ==        2014
  year            :: t -> Integer
  -- | > month          "2014-⁠12-⁠24T18:11:47.042Z" ==          12
  month           :: t -> Integer
  -- | > day            "2014-⁠12-⁠24T18:11:47.042Z" ==          24
  day             :: t -> Integer
  -- | > hour           "2014-⁠12-⁠24T18:11:47.042Z" ==          18
  hour            :: t -> Integer
  -- | > minute         "2014-⁠12-⁠24T18:11:47.042Z" ==          11
  minute          :: t -> Integer
  -- | > second         "2014-⁠12-⁠24T18:11:47.042Z" ==          47
  second          :: t -> Integer
  -- | > secondFraction "2014-⁠12-⁠24T18:11:47.042Z" ==          42
  secondFraction  :: t -> Rational
  setYear               :: (MonadPlus m) => Integer  -> t -> m t

  setMonth              :: (MonadPlus m) => Integer  -> t -> m t
  setDay                :: (MonadPlus m) => Integer  -> t -> m t
  setHour               :: (MonadPlus m) => Integer  -> t -> m t
  setMinute             :: (MonadPlus m) => Integer  -> t -> m t
  setSecond             :: (MonadPlus m) => Integer  -> t -> m t
  setSecondFraction     :: (MonadPlus m) => Rational -> t -> m t


  toSecondsSinceCommonEpoch   :: t -> Rational
  fromSecondsSinceCommonEpoch :: (MonadPlus m) => Rational -> m t

class LocalOffset a where
  localOffset        :: a -> Maybe Integer
  setLocalOffset     :: (MonadPlus m) => Maybe Integer -> a -> m a