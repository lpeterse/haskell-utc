module Data.Tempus.UnixTime
  ( UnixTime(..)
  ) where

import Control.Monad

class UnixTime t where
  unixEpoch                  :: t
  toSecondsSinceUnixEpoch    :: t -> Rational
  fromSecondsSinceUnixEpoch  :: MonadPlus m => Rational -> m t