{-# LANGUAGE Safe #-}
module Data.Tempus.Class.Epoch
  ( Epoch(..)
  ) where

-- | The instant in time also known as __the epoch__: 1970-01-01T00:00:00Z
class Epoch t where
  epoch :: t

