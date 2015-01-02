module Data.Tempus.Class where

class (Eq a, Ord a) => Tempus a where
  invalid :: a