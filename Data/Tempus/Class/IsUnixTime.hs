{-# LANGUAGE Safe #-}
module Data.Tempus.Class.IsUnixTime
  ( IsUnixTime(..)
  ) where

class IsUnixTime t where
  unixSeconds      :: t -> Rational
  fromUnixSeconds  :: Monad m => Rational -> m t