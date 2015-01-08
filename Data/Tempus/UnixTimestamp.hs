module Data.Tempus.UnixTimestamp
  ( UnixTimestamp (..)
   -- * Creation
  ) where

import Control.Monad

import Data.Ratio

import Data.Tempus.GregorianTime
import Data.Tempus.Rfc3339Timestamp
import Data.Tempus.Internal
import Data.Tempus.UnixTime

-- | A time representation counting the milliseconds since 1970-01-01T00:00:00Z.
newtype UnixTimestamp
      = UnixTimestamp Rational
      deriving (Eq, Ord, Show)

instance UnixTime UnixTimestamp where
  unixEpoch 
    = UnixTimestamp 0
  unixSeconds (UnixTimestamp i)
    = i
  fromUnixSeconds s
    = return (UnixTimestamp s)

instance GregorianTime UnixTimestamp where
  fromSecondsSinceCommonEpoch i
    = return $ UnixTimestamp (i + deltaUnixEpochCommonEpoch)
  toSecondsSinceCommonEpoch (UnixTimestamp t)
    = t - deltaUnixEpochCommonEpoch
