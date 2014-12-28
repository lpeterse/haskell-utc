module Data.Tempus.UnixTime
  ( UnixTime (..)
  , epoch
  , fromUnixOffset, toUnixOffset
  , fromUnixTime, toUnixTime
  ) where

import Data.Word

-- | A time representation counting the seconds since 1970-01-01T00:00:00Z.
--
-- Use it if you were tempted to use 'Data.Tempus.UnixOffset.UnixOffset', but
--
--   * a value's memory footprint is extremely critical (and you're on 32bit).
--   * you don't need to represent dates after 2106-02-07T06:28:15Z.
newtype UnixTime
      = UnixTime Word32

-- | > epoch == "1970-01-01T00:00:00Z"
epoch :: UnixTime
epoch
  = UnixTime 0

fromUnixOffset :: Word32 -> UnixTime
fromUnixOffset i64
  = UnixTime i64

toUnixOffset :: UnixTime -> Word32
toUnixOffset (UnixTime t)
  = t

fromUnixTime :: Word32 -> UnixTime
fromUnixTime i64
  = fromUnixOffset (i64 * 1000)

toUnixTime :: UnixTime -> Word32
toUnixTime t
  = (toUnixOffset t) `quot` 1000

instance Show UnixTime where
  show _ = "FIXME"