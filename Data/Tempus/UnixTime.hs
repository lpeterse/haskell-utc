module Data.Tempus.UnixTime
  ( UnixTime(..)
  ) where

class UnixTime t where
  toUnixSeconds    :: t -> Rational
  fromUnixSeconds  :: Monad m => Rational -> m t