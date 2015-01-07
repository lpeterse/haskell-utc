module Data.Tempus.UnixTime
  ( UnixTime(..)
  ) where

import Control.Monad

class UnixTime t where
  unixSeconds      :: t -> Integer
  fromUnixSeconds  :: MonadPlus m => Integer -> m t