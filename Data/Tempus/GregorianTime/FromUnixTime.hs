{-# LANGUAGE Safe #-}
module Data.Tempus.GregorianTime.FromUnixTime where

import Control.Monad

import Data.Int

import Data.Tempus.GregorianTime.Type
import Data.Tempus.UnixTime.Type

-- | Influenced by an ingenious solution from @caf found here:
--   https://stackoverflow.com/questions/1274964/how-to-decompose-unix-time-in-c
fromUnixTime :: MonadPlus m => UnixTime -> m GregorianTime
fromUnixTime (UnixTime i)
  | i < (-62167219200000) = mzero
  | i > (253402300799999) = mzero
  | otherwise
  = do -- adjust the epoch to the year -400 and the start of the year to March 1
       let days                      = i `div` (24*60*60*1000) + 719499 + (yearToDays 400)
       -- calculate the "year" whereas a year ranges from March 1 to February 28|29
       -- having the leap days at the end of the year allows for some tricks
       yearMarFeb                   <- shrinkYearMarFeb days 399 10400
       let remainingDays             = days - (yearToDays yearMarFeb)
       let monthMarFeb               = selectMonthMarFeb remainingDays
       let (yearJanDec, monthJanDec) = if monthMarFeb > 10
                                         then (yearMarFeb + 1, monthMarFeb - 10)
                                         else (yearMarFeb,     monthMarFeb + 2)
       return $ GregorianTime
               { gdtYear         = fromIntegral $ yearJanDec - 400
               , gdtMonth        = fromIntegral monthJanDec
               , gdtDay          = fromIntegral $ remainingDays - (367 * monthMarFeb `div` 12)
               , gdtMinutes      = fromIntegral $ i `div` 60000 `mod` (24*60)
               , gdtMilliSeconds = fromIntegral $ i `mod` 60000
               , gdtOffset       = OffsetMinutes 0
               }
  where

    shrinkYearMarFeb :: MonadPlus m => Int64 -> Int64 -> Int64 -> m Int64
    shrinkYearMarFeb days lower upper
      -- we found the year satifying the condition
      | lower == upper                      = return lower
      -- just a fail-safe recursion breaker
      | lower > upper                       = mzero
      -- the tested year has more or equally many days than what we are looking for
      -- induction guarantee: unless 'lower == upper' (catched above) it always holds 'mid < upper'
      | days <= yearToDays (mid   + 1) + 30 = shrinkYearMarFeb days lower mid
      -- the tested year has less days than what we are looking for
      -- induction guarantee: it always holds 'mid + 1 > lower'
      | days >  yearToDays (mid   + 1) + 30 = shrinkYearMarFeb days (mid + 1) upper
      -- should not happen
      | otherwise                           = mzero
      where
        mid = (lower + upper) `div` 2

    selectMonthMarFeb :: Int64 -> Int64
    selectMonthMarFeb d
          | d <= 367 *  2 `div` 12 = 1
          | d <= 367 *  3 `div` 12 = 2
          | d <= 367 *  4 `div` 12 = 3
          | d <= 367 *  5 `div` 12 = 4
          | d <= 367 *  6 `div` 12 = 5
          | d <= 367 *  7 `div` 12 = 6
          | d <= 367 *  8 `div` 12 = 7
          | d <= 367 *  9 `div` 12 = 8
          | d <= 367 * 10 `div` 12 = 9
          | d <= 367 * 11 `div` 12 = 10
          | d <= 367               = 11
          | otherwise              = 12

    yearToDays :: Int64 -> Int64
    yearToDays year
      = (year * 365) + (year `div` 4) - (year `div` 100) + (year `div` 400)
