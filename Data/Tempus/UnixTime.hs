module Data.Tempus.UnixTime
  ( UnixTime(..)
  ) where

import Control.Monad

class UnixTime t where
  toUnixSeconds    :: t -> Rational
  fromUnixSeconds  :: MonadPlus m => Rational -> m t