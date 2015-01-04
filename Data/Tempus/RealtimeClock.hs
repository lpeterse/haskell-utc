module Data.Tempus.RealtimeClock where

import Data.Int
import Data.Time.Clock.POSIX

now :: IO Int64
now
  = do p <- getPOSIXTime
       return (truncate (p * 1000))