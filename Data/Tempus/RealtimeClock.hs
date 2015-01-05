module Data.Tempus.RealtimeClock where

import Data.Int
import Data.Time.Clock.POSIX

class RealtimeClock m where
  -- | Get the current clock time.
  --
  -- This function does __not__ guarantee that subsequent calls are monotonically
  -- increasing. The machine's clock might stop or even go backwards when
  -- synchronised manually or via NTP or when adapting to a leap second.
  now :: m Int64

instance RealtimeClock IO where
  now
    = do p <- getPOSIXTime
         return (truncate (p * 1000))