module Data.UTC.Internal
  ( daysToYearMonthDay
  , yearMonthDayToDays

  , yearToDays
  , daysToYear

  , deltaUnixEpochCommonEpoch

  , isValidDate

  , secsPerDay, secsPerHour, secsPerMinute, minsPerHour, hoursPerDay
  , monthsPerYear

  , fixedDecimal
  , decimalFraction
  ) where

import Data.Ratio

deltaUnixEpochCommonEpoch :: Rational
deltaUnixEpochCommonEpoch
  = 62167219200

secsPerDay :: Integer
secsPerDay
  = hoursPerDay * secsPerHour

secsPerHour :: Integer
secsPerHour
  = minsPerHour * secsPerMinute

secsPerMinute :: Integer
secsPerMinute
  = 60

minsPerHour :: Integer
minsPerHour
  = 60

hoursPerDay :: Integer
hoursPerDay
  = 24

monthsPerYear :: Integer
monthsPerYear
  = 12

-- | Convert Year-Month-Day to since 0000-01-01 in the Gregorian Calendar
--
--    * year   0         is     a leap year
--    * year 400         is     a leap year
--    * year 100,200,300 is not a leap year
--    * year / 4         is     a leap year
--    * year (-4) (5 BC) is     a leap year

--    * AD/BC vs. astronomical year numbering (as used by ISO8601)
--    * year   0         ia 1 BC
--    * year (-1)        is 2 BC
--    * year (-2)        is 3 BC etc.

yearMonthDayToDays :: (Integer, Integer, Integer) -> Integer
yearMonthDayToDays (year,month,day)
  = -- count of days of the "finalised" years
    let daysY = yearToDays year
    -- count of days of the "finalised" months
        daysM = case month - 1 of
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
        daysD = day - 1
    in  daysY + daysM + daysD
  where
    leapDay :: Integer
    leapDay
      | (year `mod` 4 == 0) && ((year `mod` 400 == 0) || (year `mod` 100 /= 0)) = 1
      | otherwise                                                               = 0

yearToDays :: Integer -> Integer
yearToDays y
  | y == 0    = 0
  | y >= 0    = 366
              + ((y-1) * 365)       -- .. and a normal year has 365 days ..
              + ((y-1) `quot` 4)    -- .. and every 4 years a leap day occurs..
              - ((y-1) `quot` 100)  -- .. but not in centuries ..
              + ((y-1) `quot` 400)  -- .. except every 400 years.
  | otherwise = (y * 365)
              + (y `quot` 4)
              - (y `quot` 100)
              + (y `quot` 400)

daysToYear :: Integer -> Integer
daysToYear ds
  = if ds <= d
      then y - 1
      else y
  where
    y = ds `div` 365
    d = yearToDays y

daysToYearMonthDay :: Integer -> (Integer, Integer, Integer)
daysToYearMonthDay d'
  = (year + y400 * 400, month, day)
  where
    d400            = yearToDays 400
    y400            = d' `div` d400
    d               = d' `mod` d400
    year            = daysToYear (d + 1)
    ld              = if (year `mod` 4   == 0) &&
                        ((year `mod` 400 == 0)
                      || (year `mod` 100 /= 0)) then (1+) else id
    doy             = d - yearToDays year
    (month,day) 
     | doy <     31 = ( 1, doy          + 1)
     | doy < ld  59 = ( 2, doy -     31 + 1)
     | doy < ld  90 = ( 3, doy - ld  59 + 1)
     | doy < ld 120 = ( 4, doy - ld  90 + 1)
     | doy < ld 151 = ( 5, doy - ld 120 + 1)
     | doy < ld 181 = ( 6, doy - ld 151 + 1)
     | doy < ld 212 = ( 7, doy - ld 181 + 1)
     | doy < ld 243 = ( 8, doy - ld 212 + 1)
     | doy < ld 273 = ( 9, doy - ld 243 + 1)
     | doy < ld 304 = (10, doy - ld 273 + 1)
     | doy < ld 334 = (11, doy - ld 304 + 1)
     | otherwise    = (12, doy - ld 334 + 1)

isValidDate :: (Integer, Integer, Integer) -> Bool
isValidDate (y,m,d)
  | m < 1  = False
  | m > 12 = False
  | otherwise = case m of
                   1  -> validateDays31
                   2  -> validateDays28or29
                   3  -> validateDays31
                   4  -> validateDays30
                   5  -> validateDays31
                   6  -> validateDays30
                   7  -> validateDays31
                   8  -> validateDays31
                   9  -> validateDays30
                   10 -> validateDays31
                   11 -> validateDays30
                   12 -> validateDays31
                   _  -> False
  where
    validateDays31
      | 1 <= d && d <= 31           = True
      | otherwise                   = False
    validateDays30
      | 1 <= d && d <= 30           = True
      | otherwise                   = False
    validateDays28or29
      | 1 <= d && d <= 28           = True
      | d == 29 && isLeapYear       = True
      | otherwise                   = False
    isLeapYear
      = (y `mod` 4 == 0) && ((y `mod` 400 == 0) || (y `mod` 100 /= 0))

fixedDecimal :: Integer -> Integer -> String
fixedDecimal digits j
  = f digits j []
  where
    f n i accum | n <= 0    = accum
                | otherwise = f (n - 1) (i `div` 10)
                                ((toEnum $ fromIntegral $ 48 + i `mod` 10):accum)

decimalFraction :: Integer -> Rational -> String
decimalFraction _     0
  = ""
decimalFraction limit r
  = '.':(df limit r)
  where
    df _ 0
      = ""
    df 0 _
      = "..."
    df limit' r'
      = ch:(df (limit' - 1) s)
      where
        r10 = r' * 10
        ch  = toEnum $ fromIntegral $ 48 + (truncate $ r10) `mod` (10 :: Integer)
        s   = r10 - (truncate r10 % 1)