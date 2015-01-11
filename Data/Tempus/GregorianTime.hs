{-# LANGUAGE Safe #-}
module Data.Tempus.GregorianTime
  ( Date(..)
  , Time(..)
  , LocalOffset(..)
  , validate
  ) where

import Control.Monad

import Data.Tempus.Epoch

class Epoch t => Date t where
  -- | > year  "2014-⁠12-⁠24" ==  2014
  year                  :: t -> Integer
  -- | > month "2014-⁠12-⁠24" ==    12
  month                 :: t -> Integer
  -- | > day   "2014-⁠12-⁠24" ==    24
  day                   :: t -> Integer
  setYear               :: (Monad m) => Integer  -> t -> m t
  setMonth              :: (Monad m) => Integer  -> t -> m t
  setDay                :: (Monad m) => Integer  -> t -> m t

class Epoch t => Time t where
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

class LocalOffset a where
  localOffset           :: a -> Maybe Rational
  setLocalOffset        :: (Monad m) => Maybe Rational -> a -> m a

validate :: (Monad m, Date t, Time t) => t -> m t
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
                 _  -> fail ""
          else fail ""
    validateDays31
      | 1 <= day t && day t <= 31           = return ()
      | otherwise                           = fail ""
    validateDays30
      | 1 <= day t && day t <= 30           = return ()
      | otherwise                           = fail ""
    validateDays28or29
      | 1 <= day t && day t <= 28           = return ()
      | day t == 29 && isLeapYear           = return ()
      | otherwise                           = fail ""
    validateHour
      | 0 <= hour t && hour t < 24          = return ()
      | otherwise                           = fail ""
    validateMinute
      | 0 <= minute t && minute t < 60      = return ()
      | otherwise                           = fail ""
    validateSecond
      | 0 <= second t && second t < 60      = return ()
      | otherwise                           = fail ""
    validateSecondFraction
      | truncate (secondFraction t) == (0 :: Integer) = return ()
      | otherwise                                     = fail ""
    isLeapYear
      = let y = year t
        in  (y `mod` 4 == 0) && ((y `mod` 400 == 0) || (y `mod` 100 /= 0))