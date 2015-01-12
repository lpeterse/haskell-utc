{-# LANGUAGE Safe #-}
module Data.Tempus.Class.HasUnixTime
  ( HasUnixTime(..)
  ) where

class HasUnixTime t where
  unixSeconds      :: t -> Rational
  fromUnixSeconds  :: Monad m => Rational -> m t