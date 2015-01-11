module Data.Tempus.Epoch
  ( Epoch(..)
  , Midnight(..)
  ) where

-- | The instant in time also known as __the epoch__: 1970-01-01T00:00:00Z.
class Epoch t where
  epoch :: t

-- | This class is for types that represent time instants within a day.
class Midnight t where
  -- | The beginning of a day: 00:00:00
  --
  -- midnight' shall just be an alias for 'epoch' for types that are also instance of 'Epoch'.
  midnight :: t
