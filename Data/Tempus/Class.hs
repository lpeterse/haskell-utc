module Data.Tempus.Class where

import Data.Tempus.GregorianTime.Internal

class (Eq a, Ord a) => Tempus a where
  toGregorianTime   :: a -> GregorianTime
  fromGregorianTime :: GregorianTime -> a
  isInvalid :: a -> Bool
