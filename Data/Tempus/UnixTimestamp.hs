module Data.Tempus.UnixTimestamp
  ( UnixTimestamp (..)
   -- * Creation
  ) where

import Data.Tempus.GregorianTime
import Data.Tempus.Internal
import Data.Tempus.UnixTime

-- | A time representation counting the seconds since 1970-01-01T00:00:00Z.
newtype UnixTimestamp
      = UnixTimestamp Rational
      deriving (Eq, Ord, Show)

instance UnixTime UnixTimestamp where
  unixEpoch 
    = UnixTimestamp 0
  toSecondsSinceUnixEpoch (UnixTimestamp i)
    = i
  fromSecondsSinceUnixEpoch s
    = return (UnixTimestamp s)

instance GregorianTime UnixTimestamp where
  commonEpoch
    = UnixTimestamp (negate deltaUnixEpochCommonEpoch)

  fromSecondsSinceCommonEpoch i
    = return $ UnixTimestamp (i + deltaUnixEpochCommonEpoch)
  toSecondsSinceCommonEpoch (UnixTimestamp t)
    = t - deltaUnixEpochCommonEpoch