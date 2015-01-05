module Data.Tempus.UnixTime.Type where

import Data.Int

import Data.Tempus.Epoch

-- | A time representation counting the milliseconds since 1970-01-01T00:00:00Z.
newtype UnixTime
      = UnixTime Int64
      deriving (Eq, Ord)

instance UnixEpoch UnixTime where
  unixEpoch 
    = UnixTime 0

instance CommonEpoch UnixTime where
  commonEpoch
    = UnixTime (-62167219200000)