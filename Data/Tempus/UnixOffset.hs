module Data.Tempus.UnixOffset
  ( UnixOffset (..)
  ) where

import Data.Int

import Data.Tempus.Class

-- | A time representation counting the milliseconds since 1970-01-01T00:00:00Z.
--
-- Use it if
--
--   * a value's memory footprint is important to you.
--   * you don't care about leap seconds.
--   * you don't care about a local offset or timezones.
--   * you don't need to represent dates before 1970-01-01T00:00:00Z.
newtype UnixOffset
      = UnixOffset Int
      deriving (Eq, Ord)

instance Tempus UnixOffset where
  invalid = UnixOffset minBound