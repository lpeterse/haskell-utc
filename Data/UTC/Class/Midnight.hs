{-# LANGUAGE Safe #-}
module Data.UTC.Class.Midnight
  ( Midnight(..)
  ) where

-- | The beginning of a day: 00:00:00
class Midnight t where
  midnight :: t

