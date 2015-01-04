module Data.Tempus.UnixTime.Type where

import Data.Int

-- | A time representation counting the milliseconds since 1970-01-01T00:00:00Z.
newtype UnixTime
      = UnixTime Int64
      deriving (Eq, Ord)