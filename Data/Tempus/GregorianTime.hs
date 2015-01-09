{-# LANGUAGE Safe #-}
module Data.Tempus.GregorianTime
  ( GregorianTime(..)
  , LocalOffset(..)
  , validate
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

class LocalOffset a where
  localOffset        :: a -> Maybe Rational
  setLocalOffset     :: (MonadPlus m) => Maybe Rational -> a -> m a

validate :: (MonadPlus m, GregorianTime t) => t -> m t
validate t
  = do validateDate
       validateHour
       validateMinute
       validateSecond
       validateSecondFraction
       return t
  where
    validateDate
      = if 1 <= month t && month t <= 12
          then case month t of
                 1  -> validateDays31
                 2  -> validateDays28or29
                 3  -> validateDays31
                 4  -> validateDays30
                 5  -> validateDays31
                 6  -> validateDays30
                 7  -> validateDays31
                 8  -> validateDays31
                 9  -> validateDays30
                 10 -> validateDays31
                 11 -> validateDays30
                 12 -> validateDays31
                 _  -> mzero
          else mzero
    validateDays31
      | 1 <= day t && day t <= 31           = return ()
      | otherwise                           = mzero
    validateDays30
      | 1 <= day t && day t <= 30           = return ()
      | otherwise                           = mzero
    validateDays28or29
      | 1 <= day t && day t <= 28           = return ()
      | day t == 29 && isLeapYear           = return ()
      | otherwise                           = mzero
    validateHour
      | 0 <= hour t && hour t < 24          = return ()
      | otherwise                           = mzero
    validateMinute
      | 0 <= minute t && minute t < 60      = return ()
      | otherwise                           = mzero
    validateSecond
      | 0 <= second t && second t < 60      = return ()
      | otherwise                           = mzero
    validateSecondFraction
      | truncate (secondFraction t) == (0 :: Integer) = return ()
      | otherwise                                     = mzero
    isLeapYear
      = let y = year t
        in  (y `mod` 4 == 0) && ((y `mod` 400 == 0) || (y `mod` 100 /= 0))