module Data.UTC.Internal.Test where

import Test.QuickCheck
import Test.Framework (testGroup)
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.UTC.Internal

test :: Test
test
  = testGroup "Data.UTC.Internal"
  $ [ testYearMonthDayToDays
    , testYearToDays
    , testDaysToYearMonthDay
    , testYearMonthDayToDaysAndDaysToYearMonthDay

    , testProperty "yearMonthDayToDays for every day in year 0-400"
    $ conjoin
    $ map (\(ds, ymd)-> ds == yearMonthDayToDays ymd
          ) (dayDateTuples)

    , testProperty "daysToYearMonthDay for every day in year 0-400"
    $ conjoin
    $ map (\(ds, ymd)-> daysToYearMonthDay ds == ymd
          ) (dayDateTuples)
    ]

dayDateTuples :: [(Integer, (Integer, Integer, Integer))]
dayDateTuples
  = zip [0..]
        [ (y,m,d) | y <- [0..400]
                  , m <- [1..12]
                  , d <- [1..31]
                  , isValidDate (y,m,d)
        ]

testYearMonthDayToDays :: Test
testYearMonthDayToDays
  = testGroup "yearMonthDayToDays"
  $ [ testProperty ("yearMonthDayToDays (0,1,1) === 0")
    $ yearMonthDayToDays (0,1,1) === 0

    , testProperty ("yearMonthDayToDays (1,1,1) === 366 + 365")
    $ yearMonthDayToDays (1,1,1) === 366

    , testProperty ("yearMonthDayToDays (2,1,1) === 366 + 365")
    $ yearMonthDayToDays (2,1,1) === 366 + 365

    , testProperty ("yearMonthDayToDays (3,1,1) === 366 + 365 + 365")
    $ yearMonthDayToDays (3,1,1) === 366 + 365 + 365

    , testProperty ("yearMonthDayToDays (4,1,1) === 366 + 365 + 365 + 365")
    $ yearMonthDayToDays (4,1,1) === 366 + 365 + 365 + 365

    , testProperty ("yearMonthDayToDays (4,3,3) === 366 + 365 + 365 + 365 + 31 + 29 + 2")
    $ yearMonthDayToDays (4,3,3) === 366 + 365 + 365 + 365 + 31 + 29 + 2
    ]

testYearToDays :: Test
testYearToDays
  = testGroup "yearToDays"
  $ [ testProperty "400 years are 146463 days"
    $ yearToDays 401    === (fromIntegral $ length dayDateTuples)
    , testProperty ("yearToDays 800    === 365 * 800 + length " ++ show (js 800))
    $ yearToDays 800    === 365 * 800 + (fromIntegral $ length (js 800))
    , testProperty ("yearToDays 700    === 365 * 700 + length " ++ show (js 700))
    $ yearToDays 700    === 365 * 700 + (fromIntegral $ length (js 700))
    , testProperty ("yearToDays 600    === 365 * 600 + length " ++ show (js 600))
    $ yearToDays 600    === 365 * 600 + (fromIntegral $ length (js 600))
    , testProperty ("yearToDays 500    === 365 * 500 + length " ++ show (js 500))
    $ yearToDays 500    === 365 * 500 + (fromIntegral $ length (js 500))
    , testProperty ("yearToDays 401    === 365 * 401 + length " ++ show (js 401))
    $ yearToDays 401    === 365 * 401 + (fromIntegral $ length (js 401))
    , testProperty ("yearToDays 400    === 365 * 400 + length " ++ show (js 400))
    $ yearToDays 400    === 365 * 400 + (fromIntegral $ length (js 400))
    , testProperty ("yearToDays 399    === 365 * 399 + length " ++ show (js 399))
    $ yearToDays 399    === 365 * 399 + (fromIntegral $ length (js 399))
    , testProperty ("yearToDays 300    === 365 * 300 + length " ++ show (js 300))
    $ yearToDays 300    === 365 * 300 + (fromIntegral $ length (js 300))
    , testProperty ("yearToDays 200    === 365 * 200 + length " ++ show (js 200))
    $ yearToDays 200    === 365 * 200 + (fromIntegral $ length (js 200))
    , testProperty ("yearToDays 101    === 365 * 101 + length " ++ show (js 101))
    $ yearToDays 101    === 365 * 101 + (fromIntegral $ length (js 101))
    , testProperty ("yearToDays 100    === 365 * 100 + length " ++ show (js 100))
    $ yearToDays 100    === 365 * 100 + (fromIntegral $ length (js 100))
    , testProperty ("yearToDays  99    === 365 *  99 + length " ++ show (js 99))
    $ yearToDays  99    === 365 * 99 + (fromIntegral $ length (js 99))
    , testProperty ("yearToDays   4    === 365 * 4 + length " ++ show (js 4))
    $ yearToDays   4    === 365 * 4 + (fromIntegral $ length (js 4))
    , testProperty ("yearToDays   3    === 365 * 3 + length " ++ show (js 3))
    $ yearToDays   3    === 365 * 3 + (fromIntegral $ length (js 3))
    , testProperty ("yearToDays   2    === 365 * 2 + length " ++ show (js 2))
    $ yearToDays   2    === 365 * 2 + (fromIntegral $ length (js 2))
    , testProperty ("yearToDays   1    === 365 * 1 + length " ++ show (js 1))
    $ yearToDays   1    === 365 * 1 + (fromIntegral $ length (js 1))
    , testProperty  "yearToDays   0    === 0"
    $ yearToDays   0    === 0
    , testProperty  "yearToDays (-1)   === 0 - 365"
    $ yearToDays (-1)   === 0 - 365
    , testProperty  "yearToDays (-2)   === 0 - 365 - 365"
    $ yearToDays (-2)   === 0 - 365 - 365
    , testProperty  "yearToDays (-3)   === 0 - 365 - 365 - 365"
    $ yearToDays (-3)   === 0 - 365 - 365 - 365
    , testProperty  "yearToDays (-4)   === 0 - 365 - 365 - 365 - 366"
    $ yearToDays (-4)   === 0 - 365 - 365 - 365 - 366
    , testProperty ("yearToDays (-100) === (-365) * 100 - length " ++ show (ls 100))
    $ yearToDays (-100) === (-365) * 100 - (fromIntegral $ length (ls 100))
    , testProperty ("yearToDays (-200) === (-365) * 200 - length " ++ show (ls 200))
    $ yearToDays (-200) === (-365) * 200 - (fromIntegral $ length (ls 200))
    , testProperty ("yearToDays (-300) === (-365) * 300 - length " ++ show (ls 300))
    $ yearToDays (-300) === (-365) * 300 - (fromIntegral $ length (ls 300))
    , testProperty ("yearToDays (-400) === (-365) * 400 - length " ++ show (ls 400))
    $ yearToDays (-400) === (-365) * 400 - (fromIntegral $ length (ls 400))
    ]
  where
    ls  :: Int -> [Int]
    ls z = [x | x <- [negate z..(-1)], x `mod` 400 == 0 || (x `mod` 4 == 0 && x `mod` 100 /= 0 )]
    js  :: Int -> [Int]
    js z = [x | x <- [0..(z-1)], x `mod` 400 == 0 || (x `mod` 4 == 0 && x `mod` 100 /= 0 )]

testDaysToYearMonthDay :: Test
testDaysToYearMonthDay
  = testGroup "daysToYearMonthDay"
  $ [
      testProperty "daysToYearMonthDay 366 === (1,1,1)"
    $ daysToYearMonthDay 366 === (1,1,1)

    , testProperty "daysToYearMonthDay 365 === (0,12,31)"
    $ daysToYearMonthDay 365 === (0,12,31)

    , testProperty "daysToYearMonthDay 335 === (0,12,1)"
    $ daysToYearMonthDay 335 === (0,12,1)

    , testProperty "daysToYearMonthDay 334 === (0,11,30)"
    $ daysToYearMonthDay 334 === (0,11,30)

    , testProperty "daysToYearMonthDay 305 === (0,11,1)"
    $ daysToYearMonthDay 305 === (0,11,1)

    , testProperty "daysToYearMonthDay 304 === (0,10,31)"
    $ daysToYearMonthDay 304 === (0,10,31)

    , testProperty "daysToYearMonthDay 274 === (0,10,1)"
    $ daysToYearMonthDay 274 === (0,10,1)

    , testProperty "daysToYearMonthDay 273 === (0,9,30)"
    $ daysToYearMonthDay 273 === (0,9,30)

    , testProperty "daysToYearMonthDay 244 === (0,9,1)"
    $ daysToYearMonthDay 244 === (0,9,1)

    , testProperty "daysToYearMonthDay 243 === (0,8,31)"
    $ daysToYearMonthDay 243 === (0,8,31)

    , testProperty "daysToYearMonthDay 213 === (0,8,1)"
    $ daysToYearMonthDay 213 === (0,8,1)

    , testProperty "daysToYearMonthDay 212 === (0,7,31)"
    $ daysToYearMonthDay 212 === (0,7,31)

    , testProperty "daysToYearMonthDay 182 === (0,7,1)"
    $ daysToYearMonthDay 182 === (0,7,1)

    , testProperty "daysToYearMonthDay 181 === (0,6,30)"
    $ daysToYearMonthDay 181 === (0,6,30)

    , testProperty "daysToYearMonthDay 152 === (0,6,1)"
    $ daysToYearMonthDay 152 === (0,6,1)

    , testProperty "daysToYearMonthDay 151 === (0,5,31)"
    $ daysToYearMonthDay 151 === (0,5,31)

    , testProperty "daysToYearMonthDay 121 === (0,5,1)"
    $ daysToYearMonthDay 121 === (0,5,1)

    , testProperty "daysToYearMonthDay 120 === (0,4,30)"
    $ daysToYearMonthDay 120 === (0,4,30)

    , testProperty "daysToYearMonthDay 91 === (0,4,1)"
    $ daysToYearMonthDay 91 === (0,4,1)

    , testProperty "daysToYearMonthDay 90 === (0,3,31)"
    $ daysToYearMonthDay 90 === (0,3,31)

    , testProperty "daysToYearMonthDay 60 === (0,3,1)"
    $ daysToYearMonthDay 60 === (0,3,1)

    , testProperty "daysToYearMonthDay 59 === (0,2,29)"
    $ daysToYearMonthDay 59 === (0,2,29)

    , testProperty "daysToYearMonthDay 58 === (0,2,28)"
    $ daysToYearMonthDay 58 === (0,2,28)

    , testProperty "daysToYearMonthDay 31 === (0,2,1)"
    $ daysToYearMonthDay 31 === (0,2,1)

    , testProperty "daysToYearMonthDay 30 === (0,1,31)"
    $ daysToYearMonthDay 30 === (0,1,31)

    , testProperty "daysToYearMonthDay 0 === (0,1,1)"
    $ daysToYearMonthDay 0 === (0,1,1)

    , testProperty "daysToYearMonthDay (-1) === (-1,12,31)"
    $ daysToYearMonthDay (-1) === (-1,12,31)

    , testProperty "daysToYearMonthDay (-31) === (-1,12,1)"
    $ daysToYearMonthDay (-31) === (-1,12,1)

    , testProperty "daysToYearMonthDay (-32) === (-1,11,30)"
    $ daysToYearMonthDay (-32) === (-1,11,30)

    , testProperty "daysToYearMonthDay (-61) === (-1,11,1)"
    $ daysToYearMonthDay (-61) === (-1,11,1)

    , testProperty "daysToYearMonthDay (-62) === (-1,10,31)"
    $ daysToYearMonthDay (-62) === (-1,10,31)

    , testProperty "daysToYearMonthDay (-92) === (-1,10,1)"
    $ daysToYearMonthDay (-92) === (-1,10,1)

    , testProperty "daysToYearMonthDay (-93) === (-1,9,30)"
    $ daysToYearMonthDay (-93) === (-1,9,30)

    , testProperty "daysToYearMonthDay (-103) === (-1,9,20)"
    $ daysToYearMonthDay (-103) === (-1,9,20)

    , testProperty "daysToYearMonthDay (-108) === (-1,9,15)"
    $ daysToYearMonthDay (-108) === (-1,9,15)

    , testProperty "daysToYearMonthDay (-111) === (-1,9,12)"
    $ daysToYearMonthDay (-111) === (-1,9,12)

    , testProperty "daysToYearMonthDay (-112) === (-1,9,11)"
    $ daysToYearMonthDay (-112) === (-1,9,11)

    , testProperty "daysToYearMonthDay (-113) === (-1,9,10)"
    $ daysToYearMonthDay (-113) === (-1,9,10)

    , testProperty "daysToYearMonthDay (-114) === (-1,9,9)"
    $ daysToYearMonthDay (-114) === (-1,9,9)

    , testProperty "daysToYearMonthDay (-122) === (-1,9,1)"
    $ daysToYearMonthDay (-122) === (-1,9,1)

    , testProperty "daysToYearMonthDay (-123) === (-1,8,31)"
    $ daysToYearMonthDay (-123) === (-1,8,31)

    , testProperty "daysToYearMonthDay (-153) === (-1,8,1)"
    $ daysToYearMonthDay (-153) === (-1,8,1)

    , testProperty "daysToYearMonthDay (-154) === (-1,7,31)"
    $ daysToYearMonthDay (-154) === (-1,7,31)

    , testProperty "daysToYearMonthDay (-184) === (-1,7,1)"
    $ daysToYearMonthDay (-184) === (-1,7,1)

    , testProperty "daysToYearMonthDay (-185) === (-1,6,30)"
    $ daysToYearMonthDay (-185) === (-1,6,30)

    , testProperty "daysToYearMonthDay (-214) === (-1,6,1)"
    $ daysToYearMonthDay (-214) === (-1,6,1)

    , testProperty "daysToYearMonthDay (-215) === (-1,5,31)"
    $ daysToYearMonthDay (-215) === (-1,5,31)

    , testProperty "daysToYearMonthDay (-245) === (-1,5,1)"
    $ daysToYearMonthDay (-245) === (-1,5,1)

    , testProperty "daysToYearMonthDay (-246) === (-1,4,30)"
    $ daysToYearMonthDay (-246) === (-1,4,30)

    , testProperty "daysToYearMonthDay (-275) === (-1,4,1)"
    $ daysToYearMonthDay (-275) === (-1,4,1)

    , testProperty "daysToYearMonthDay (-276) === (-1,3,31)"
    $ daysToYearMonthDay (-276) === (-1,3,31)

    , testProperty "daysToYearMonthDay (-306) === (-1,3,1)"
    $ daysToYearMonthDay (-306) === (-1,3,1)

    , testProperty "daysToYearMonthDay (-307) === (-1,2,28)"
    $ daysToYearMonthDay (-307) === (-1,2,28)

    , testProperty "daysToYearMonthDay (-334) === (-1,2,1)"
    $ daysToYearMonthDay (-334) === (-1,2,1)

    , testProperty "daysToYearMonthDay (-335) === (-1,1,31)"
    $ daysToYearMonthDay (-335) === (-1,1,31)

    , testProperty "daysToYearMonthDay (-365) === (-1,1,1)"
    $ daysToYearMonthDay (-365) === (-1,1,1)

    , testProperty "daysToYearMonthDay (-366) === (-2,12,31)"
    $ daysToYearMonthDay (-366) === (-2,12,31)
    ]

testYearMonthDayToDaysAndDaysToYearMonthDay :: Test
testYearMonthDayToDaysAndDaysToYearMonthDay
  = testGroup "yearMonthDayToDays <-> daysToYearMonthDay"
  $ [ testProperty ("daysToYearMonthDay (yearMonthDayToDays (0,1,1)) === (0,1,1)")
    $ daysToYearMonthDay (yearMonthDayToDays (0,1,1)) === (0,1,1)

    , testProperty ("yearMonthDayToDays (daysToYearMonthDay x) - x === 0 for 0 < x < 1000")
    $ forAll (choose (0, 1000))
    $ \x-> yearMonthDayToDays (daysToYearMonthDay x) - x === 0

    , testProperty ("yearMonthDayToDays (daysToYearMonthDay x) - x === 0 for 0 < x < 2000")
    $ forAll (choose (0, 2000))
    $ \x-> yearMonthDayToDays (daysToYearMonthDay x) - x === 0

    , testProperty ("yearMonthDayToDays (daysToYearMonthDay x) - x === 0 for 0 < x < 3652424")
    $ forAll (choose (0, 3652424)) -- 0000-01-01 to 9999-12-31
    $ \x-> yearMonthDayToDays (daysToYearMonthDay x) - x === 0

    , testProperty ("yearMonthDayToDays (daysToYearMonthDay x) - x === 0 for -100 < x < 100")
    $ forAll (choose ((-100), 100)) -- 0000-01-01 to 9999-12-31
    $ \x-> yearMonthDayToDays (daysToYearMonthDay x) - x === 0

    , testProperty ("yearMonthDayToDays (daysToYearMonthDay x) - x === 0 for -1000000 < x < 5000000")
    $ forAll (choose (-1000000, 5000000)) -- 0000-01-01 to 9999-12-31
    $ \x-> yearMonthDayToDays (daysToYearMonthDay x) - x === 0
    ]
