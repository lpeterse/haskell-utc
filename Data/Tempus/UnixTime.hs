module Data.Tempus.UnixTime
  ( UnixTime(..)
  ) where

import Control.Monad

class UnixTime t where
  -- | > unixEpoch == "1970-01-01T00:00:00-00:00"
  unixEpoch                  :: t
  toSecondsSinceUnixEpoch    :: t -> Rational
  fromSecondsSinceUnixEpoch  :: MonadPlus m => Rational -> m t