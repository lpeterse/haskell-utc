{-# LANGUAGE Safe #-}
module Data.Tempus.UnixTime
  ( UnixTime (..)
   -- * Creation
  , fromGregorianTime
  ) where


import Control.Monad

import Data.Tempus.GregorianCalendar
import Data.Tempus.UnixTime.Type
import Data.Tempus.UnixTime.FromGregorianTime

instance GregorianCalendar UnixTime where
  commonEpoch
    = UnixTime (-62167219200000)
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

