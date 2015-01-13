{-# LANGUAGE Safe #-}
module Data.UTC.Class.IsUnixTime
  ( IsUnixTime(..)
  ) where

class IsUnixTime t where
  unixSeconds      :: t -> Rational
  fromUnixSeconds  :: Monad m => Rational -> m t