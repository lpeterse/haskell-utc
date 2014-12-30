module Data.Tempus.Class where

class (Eq a, Ord a) => Tempus a where
  -- | The instant in time designated by __1970-01-01T00:00:00Z__.
  epoch :: a