{-# LANGUAGE Safe #-}
module Data.Tempus.Class.IsDate
  ( IsDate (..)
  ) where

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