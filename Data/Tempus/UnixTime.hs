module Data.Tempus.UnixTime
  ( UnixTime (..)
   -- * Creation
  , fromGregorianTime
  ) where


import Control.Monad

import Data.Tempus.Class
import Data.Tempus.Rfc3339
import Data.Tempus.GregorianTime
import Data.Tempus.UnixTime.Type
import Data.Tempus.UnixTime.FromGregorianTime
import Data.Tempus.RealtimeClock as RT

instance Show UnixTime where
  show (UnixTime t) = show t

instance Tempus UnixTime where
  now
    = RT.now >>= return . UnixTime
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

instance Rfc3339 UnixTime where
  parseRfc3339ByteString  s = parseRfc3339ByteString s >>= fromGregorianTime
  renderRfc3339ByteString t = fromUnixTime           t >>= renderRfc3339ByteString