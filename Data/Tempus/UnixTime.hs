module Data.Tempus.UnixTime
  ( UnixTime (..)
  ) where


import Control.Monad
import Data.Int

import Data.Tempus.Class

-- | A time representation counting the seconds since 1970-01-01T00:00:00Z.
--
-- Use it if you were tempted to use 'Data.Tempus.UnixOffset.UnixOffset', but
--
--   * a value's memory footprint is extremely critical (and you're on 32bit).
--   * you don't need to represent dates after 2106-02-07T06:28:15Z.
newtype UnixTime
      = UnixTime Int64
      deriving (Eq, Ord)




instance Show UnixTime where
  show _ = "FIXME"

instance Tempus UnixTime where
  getYear  (UnixTime t)
    = mzero
  getMonth (UnixTime t)
    = mzero
  getDay    (UnixTime t)
    = mzero
  getHour   (UnixTime t)
    = return $ fromIntegral $ t `quot` 60*60*1000 `rem` 24
  getMinute (UnixTime t)
    = return $ fromIntegral $ t `quot` 60*1000 `rem` 60
  getSecond (UnixTime t)
    = return $ fromIntegral $ t `quot` 1000 `rem` 60
  getMilliSecond (UnixTime t)
    = return $ fromIntegral $ t `rem` 1000
