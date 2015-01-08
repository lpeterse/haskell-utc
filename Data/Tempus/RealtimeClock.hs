{-# LANGUAGE Safe #-}
module Data.Tempus.RealtimeClock where

import Data.Ratio
import System.Clock as C

import Data.Tempus.UnixTime

-- | This class defines an interface for contexts that can be asked for a timestamp.
--
-- Most users are likely to just need the 'Prelude.IO' instance, but you might
-- think of other instances:
--
--    - A wrapper around the system clock with internal state that ensures
--      strict monotonically increasing values.
--    - A custom monadic computation that needs time, but should not be given
--     'Prelude.IO' access.
--    - Testing contexts where you might want to inject and test specific timestamps.
class RealtimeClock m where
  -- | Get the current clock time.
  --
  -- Beware: The 'Prelude.IO' instance does __not__ guarantee that
  -- subsequent calls are monotonically increasing. The system's clock might
  -- stop or even go backwards when synchronised manually or via NTP or when
  -- adapting to a leap second.
  --
  -- Example:
  --
  -- > import Data.Tempus
  -- > 
  -- > printCurrentYear :: IO ()
  -- > printCurrentYear
  -- >   = do now <- getTime :: IO UnixTimestamp
  -- >        print (year now)
  getTime :: (UnixTime a) => m a

instance RealtimeClock IO where
  getTime
    = do TimeSpec sec nsec <- C.getTime Realtime
         case fromUnixSeconds ((fromIntegral sec) % 1 + (fromIntegral nsec) % 1000000000) of
           Just t  -> return t
           Nothing -> fail "Data.Tempus.RealtimeClock.getTime: cannot convert to UnixTime"

