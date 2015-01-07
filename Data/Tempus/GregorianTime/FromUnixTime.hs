{-# LANGUAGE Safe #-}
module Data.Tempus.GregorianTime.FromUnixTime where

import Control.Monad

import Data.Tempus.GregorianTime.Type
import Data.Tempus.UnixTime.Type
import Data.Tempus.Internal

fromUnixTime :: MonadPlus m => UnixTime -> m GregorianTime
fromUnixTime (UnixTime i)
  | i < (-62167219200000) = mzero -- 0000-01-01T00:00:00Z
  | i > (253402300799999) = mzero -- 9999-12-31T23:59:59.999Z
  | otherwise
  = do -- difference between unix epoch and common epoch:
       -- > yearMonthDayToDays (1970,1,1) == Just 719528
       let days                      = i `div` (24*60*60*1000) + 719528
       (year,month,day)             <- daysToYearMonthDay (fromIntegral days)
       return $ GregorianTime
               { gdtYear         = year
               , gdtMonth        = month
               , gdtDay          = day
               , gdtMinutes      = fromIntegral $ i `div` 60000 `mod` (24*60)
               , gdtMilliSeconds = fromIntegral $ i `mod` 60000
               , gdtOffset       = OffsetMinutes 0
               }
