module Data.Tempus.UnixTime.Type where

import Data.Int

-- | A time representation counting the seconds since 1970-01-01T00:00:00Z.
--
-- Use it if you were tempted to use 'Data.Tempus.UnixOffset.UnixOffset', but
--
--   * a value's memory footprint is extremely critical (and you're on 32bit).
--   * you don't need to represent dates after 2106-02-07T06:28:15Z.
newtype UnixTime
      = UnixTime Int64
      deriving (Eq, Ord)