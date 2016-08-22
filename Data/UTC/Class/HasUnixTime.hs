{-# LANGUAGE Safe #-}
{-# LANGUAGE ConstrainedClassMethods #-}


module Data.UTC.Class.HasUnixTime where

import Control.Monad.Catch

import Data.Ratio
import System.Clock as C

import Data.UTC.Class.IsUnixTime
import Data.UTC.Type.Exception

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
class HasUnixTime m where
  -- | Get a timestamp from the surrounding context. The 'Prelude.IO' instance gives
  --  access to the __system clock__ and is what most users are probably looking for.
  --
  -- /Beware:/ The 'Prelude.IO' instance does __not__ guarantee that
  -- subsequent calls are monotonically increasing. The system's clock might
  -- stop or even go backwards when synchronised manually or via NTP or when
  -- adapting to a leap second.
  --
  -- /Example:/
  --
  -- > import Data.UTC
  -- > 
  -- > printCurrentYear :: IO ()
  -- > printCurrentYear
  -- >   = do now <- getUnixTime :: IO DateTime
  -- >        print (year now)
  getUnixTime :: (Monad m, IsUnixTime a) => m a

instance HasUnixTime IO where
  getUnixTime
    = do TimeSpec s ns <- C.getTime Realtime
         case fromUnixSeconds ((fromIntegral s) % 1 + (fromIntegral ns) % 1000000000) of
           Just t  -> return t
           Nothing -> throwM $ UtcException "HasUnixTime IO: getUnixTime"

