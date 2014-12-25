module Data.Tempus
  ( UnixTime
  )
  where

import           Data.Int
import           Data.Word
import Data.List

class Tempus a where
  toUnixTime   :: a -> UnixTime
  fromUnixTime :: UnixTime -> a

newtype UnixTime
      = UnixTime Int64

instance Tempus UnixTime where
  toUnixTime
    = id
  fromUnixTime
    = id

data CalendarTime
   = CalendarTime
     { years     :: Word16
     , months    :: Word8
     , days      :: Word16
     , hours     :: Word8
     , minutes   :: Word8
     , seconds   :: Word8
     , millis    :: Word16
     , tzMinutes :: Int16
     , tzEnabled :: Bool
     }

instance Tempus CalendarTime where
  toUnixTime ct
    = UnixTime (x1 + x2 + x3 + x4 + x5 + x6)
    where
      x1 | years ct < 1970 = 0 - umsFromYears [years ct .. 1969]
         | years ct > 1970 = 0 + umsFromYears [1970 .. years ct - 1]
         | otherwise       = 0
      x2 = umsFromMonths   (months ct) (years ct)
      x3 = (i64 . hours)   ct * umsPerHour
      x4 = (i64 . minutes) ct * umsPerMinute
      x5 = (i64 . seconds) ct * umsPerSecond
      x6 = (i64 . millis)  ct
  fromUnixTime 
    = undefined

-- helper functions

isLeapYear :: Word16 -> Bool
isLeapYear year
  = year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0)

daysFromYear :: Integral a => Word16 -> a
daysFromYear year
  | isLeapYear year = 366
  | otherwise       = 365

daysFromMonth :: Integral a => Word8 -> Word16 -> a
daysFromMonth
  1  _ = 31
daysFromMonth
  2  y | isLeapYear y = 29
       | otherwise    = 28
daysFromMonth
  3  _ = 31
daysFromMonth
  4  _ = 30
daysFromMonth
  5  _ = 31
daysFromMonth
  6  _ = 30
daysFromMonth
  7  _ = 31
daysFromMonth
  8  _ = 31
daysFromMonth
  9  _ = 30
daysFromMonth
  10 _ = 31
daysFromMonth
  11 _ = 30
daysFromMonth
  12 _ = 31
daysFromMonth
  _  _ = 0

i64 :: Integral a => a -> Int64
i64
  = fromIntegral

umsFromMonths :: Word8 -> Word16 -> Int64
umsFromMonths m y
  = foldl' (+) 0 $ map (\n-> daysFromMonth n y * umsPerDay) [1 .. m]

umsFromYears :: [Word16] -> Int64
umsFromYears years
  = foldl' (+) 0 $ map (\y-> daysFromYear y * umsPerDay) years

umsFromHours :: Word8 -> Int64
umsFromHours hours
  = i64 hours * umsPerHour

umsPerDay    :: Int64
umsPerDay
  = 24 * umsPerHour

umsPerHour   :: Int64
umsPerHour
  = 60 * umsPerMinute

umsPerMinute :: Int64
umsPerMinute
  = 60 * umsPerSecond

umsPerSecond :: Int64
umsPerSecond
  = 1000


