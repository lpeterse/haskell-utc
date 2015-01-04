module Data.Tempus.GregorianTime.ToUnixTime where

import Control.Monad

import Data.Int

import Data.Tempus.GregorianTime.Internal
import Data.Tempus.UnixTime

toUnixTime :: MonadPlus m => GregorianTime -> m UnixTime
toUnixTime gdt
  = do -- count of days of the "finalised" years
       let daysY = yearToDays $ fromIntegral (gdtYear gdt - 1)
       -- count of days of the "finalised" months
       let daysM = case gdtMonth gdt - 1 of
                     1  -> 31
                     2  -> 31 + 28 + leapDay
                     3  -> 31 + 28 + 31 + leapDay
                     4  -> 31 + 28 + 31 + 30 + leapDay
                     5  -> 31 + 28 + 31 + 30 + 31 + leapDay
                     6  -> 31 + 28 + 31 + 30 + 31 + 30 + leapDay
                     7  -> 31 + 28 + 31 + 30 + 31 + 30 + 31 + leapDay
                     8  -> 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + leapDay
                     9  -> 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + leapDay
                     10 -> 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + leapDay
                     11 -> 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + leapDay
                     _  -> 0
       -- count of the "finalised" days
       let daysD = fromIntegral (gdtDay gdt - 1)
       let days  = daysY + daysM + daysD
       return $ UnixTime $              (days * 24 * 60 * 60 * 1000)
                         + fromIntegral (gdtMinutes gdt * 60 * 1000)
                         + fromIntegral (gdtMilliSeconds gdt)
                         - 62167219200000 -- ms between 0000-01-01 and 1970-01-01
  where

    yearToDays :: Int64 -> Int64
    yearToDays year 
      | year >= 0 = ((year + 1) * 365) + (year `div` 4) - (year `div` 100) + (year `div` 400) + 1
      | otherwise = 0

    leapDay :: Int64
    leapDay
      | (gdtYear gdt `mod` 4 == 0) && ((gdtYear gdt `mod` 400 == 0) || (gdtYear gdt `mod` 100 /= 0)) = 1
      | otherwise                                                                                    = 0

