module Data.Tempus.UnixTime
  ( UnixTime(..)
  ) where

import Control.Monad

class UnixTime t where
  toUnixSeconds    :: t -> Rational
  fromUnixSeconds  :: Monad m => Rational -> m t