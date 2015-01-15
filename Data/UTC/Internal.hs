module Data.UTC.Internal
  ( daysToYearMonthDay
  , yearMonthDayToDays
  , yearToDays
  , deltaUnixEpochCommonEpoch

  , isValidDate

  , secsPerDay, secsPerHour, secsPerMinute, minsPerHour, hoursPerDay
  ) where

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
  = -- count of days of the "finalised" years (therefor -1)
    let daysY = yearToDays (year - 1)
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
  = (if y >= 0 then 366 else 0) -- The year 0 is a leap year..
  + (y * 365)                   -- .. and a normal year has 365 days ..
  + (y `quot` 4)                -- .. and every 4 years a leap day occurs..
  - (y `quot` 100)              -- .. but not in centuries ..
  + (y `quot` 400)              -- .. except every 400 years.

-- | Influenced by an ingenious solution from @caf found here:
--   https://stackoverflow.com/questions/1274964/how-to-decompose-unix-time-in-c
daysToYearMonthDay :: Integer -> (Integer, Integer, Integer)
daysToYearMonthDay d
  = let days                      = d + 146068 -- 400 years and 2 months
        yearMarFeb                = shrinkYearMarFeb days 399 10400
        remainingDays             = days - (yearToDays yearMarFeb)
        monthMarFeb               = selectMonthMarFeb remainingDays
        (yearJanDec, monthJanDec) = if monthMarFeb > 10
                                       then (yearMarFeb + 1, monthMarFeb - 10)
                                       else (yearMarFeb,     monthMarFeb + 2)
    in  (yearJanDec - 400, monthJanDec, remainingDays - (367 * monthMarFeb `div` 12))
  where

    shrinkYearMarFeb :: Integer -> Integer -> Integer -> Integer
    shrinkYearMarFeb days lower upper
      -- we found the year satifying the condition
      | lower == upper                      = lower
      -- just a fail-safe recursion breaker
      | lower > upper                       = 0
      -- the tested year has more or equally many days than what we are looking for
      -- induction guarantee: unless 'lower == upper' (catched above) it always holds 'mid < upper'
      | days <= yearToDays (mid   + 1) + 30 = shrinkYearMarFeb days lower mid
      -- the tested year has less days than what we are looking for
      -- induction guarantee: it always holds 'mid + 1 > lower'
      | days >  yearToDays (mid   + 1) + 30 = shrinkYearMarFeb days (mid + 1) upper
      -- should not happen
      | otherwise                           = 0
      where
        mid = (lower + upper) `div` 2

    selectMonthMarFeb :: Integer -> Integer
    selectMonthMarFeb days
          | days <= 367 *  2 `div` 12 = 1
          | days <= 367 *  3 `div` 12 = 2
          | days <= 367 *  4 `div` 12 = 3
          | days <= 367 *  5 `div` 12 = 4
          | days <= 367 *  6 `div` 12 = 5
          | days <= 367 *  7 `div` 12 = 6
          | days <= 367 *  8 `div` 12 = 7
          | days <= 367 *  9 `div` 12 = 8
          | days <= 367 * 10 `div` 12 = 9
          | days <= 367 * 11 `div` 12 = 10
          | days <= 367               = 11
          | otherwise              = 12

    yearToDays :: Integer -> Integer
    yearToDays year
      = (year * 365) + (year `div` 4) - (year `div` 100) + (year `div` 400)

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