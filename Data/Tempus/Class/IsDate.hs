{-# LANGUAGE Safe #-}
module Data.Tempus.Class.IsDate
  ( IsDate (..)
  ) where

import Data.Tempus.Internal
import Data.Tempus.Class.Epoch

class Epoch t => IsDate t where
  -- | > year  "2014-⁠12-⁠24" ==  2014
  year                  :: t -> Integer
  -- | > month "2014-⁠12-⁠24" ==    12
  month                 :: t -> Integer
  -- | > day   "2014-⁠12-⁠24" ==    24
  day                   :: t -> Integer

  setYear               :: (Monad m) => Integer  -> t -> m t
  setMonth              :: (Monad m) => Integer  -> t -> m t
  setDay                :: (Monad m) => Integer  -> t -> m t

  addYears              :: (Monad m) => Integer  -> t -> m t
  addYears ys t
    = undefined

  addMonths             :: (Monad m) => Integer  -> t -> m t
  addMonths ms t
    = undefined

  addDays               :: (Monad m) => Integer  -> t -> m t
  addDays ds t
    -- setDay 1 to avoid intermediate generation of invalid dates!
    = setDay 1 t >>= setYear y >>= setMonth m >>= setDay d
    where
      ds'       = yearMonthDayToDays (year t, month t, day t)
      (y, m, d) = daysToYearMonthDay (ds' + ds)