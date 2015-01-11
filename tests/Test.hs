{-# LANGUAGE OverloadedStrings #-}
module Test ( tests ) where

import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck

import Test.QuickCheck

import Data.String

import Data.Tempus
import Data.Tempus.Internal

tests :: IO [Test]
tests
  = return $ testUnixTimeInstance "UnixTimestamp"      (undefined :: UnixTimestamp)
          ++ testUnixTimeInstance "GregorianTimestamp" (undefined :: GregorianTimestamp)
          ++ testDateInstance     "UnixTimestamp"      (undefined :: UnixTimestamp)
          ++ testDateInstance     "GregorianTimestamp" (undefined :: GregorianTimestamp)
          ++ testTimeInstance     "UnixTimestamp"      (undefined :: UnixTimestamp)
          ++ testTimeInstance     "GregorianTimestamp" (undefined :: GregorianTimestamp)
          ++ [ testProperty ("yearMonthDayToDays (daysToYearMonthDay x) == x")
              $ forAll (choose (0, 3652424)) -- 0000-01-01 to 9999-12-31
              $ \x-> yearMonthDayToDays (daysToYearMonthDay x) == x
            ]

testTimeInstance :: (Time t, IsString t,Eq t) => String -> t -> [Test]
testTimeInstance tn t
  = [ testProperty ("instance Time " ++ tn ++ ": test 1.t1")
    $ (t1 >>= return . hour) == Just 12
    , testProperty ("instance Time " ++ tn ++ ": test 1.t2")
    $ (t2 >>= return . hour) == Just 12
    , testProperty ("instance Time " ++ tn ++ ": test 2.t1")
    $ (t2 >>= return . minute) == Just 13
    , testProperty ("instance Time " ++ tn ++ ": test 2.t2")
    $ (t2 >>= return . minute) == Just 13
    , testProperty ("instance Time " ++ tn ++ ": test 3.t1")
    $ (t2 >>= return . second) == Just 14
    , testProperty ("instance Time " ++ tn ++ ": test 3.t2")
    $ (t2 >>= return . second) == Just 14
    , testProperty ("instance Time " ++ tn ++ ": test 4.t1")
    $ (t2 >>= return . secondFraction) == Just 0.56789
    , testProperty ("instance Time " ++ tn ++ ": test 4.t2")
    $ (t2 >>= return . secondFraction) == Just 0.56789
    ]
    ++ map (\(n,d)-> testProperty ("instance Time " ++ tn ++ ": test 5." ++ n)
                   $ d == (Nothing `asTypeOf` Just t)
           ) invalidTimes
  where
    t1 = setHour 12 (midnight `asTypeOf` t) >>= setMinute 13 >>= setSecond 14 >>= setSecondFraction 0.56789
    t2 = setSecondFraction 0.56789 (midnight `asTypeOf` t) >>= setSecond 14 >>= setMinute 13 >>= setHour 12
    invalidTimes
       = [ ("01", setHour (-1) midnight)
         , ("02", setHour 24 midnight)
         , ("03", setMinute (-1) midnight)
         , ("04", setMinute 60 midnight)
         , ("05", setSecond (-1) midnight)
         , ("06", setSecond 60 midnight)
         , ("07", setSecondFraction (-1.0) midnight)
         , ("08", setSecondFraction (-0.1) midnight)
         , ("09", setSecondFraction 1.0 midnight)
         , ("10", setSecondFraction 1.1 midnight)
         ]

testUnixTimeInstance :: (UnixTime t, IsString t,Eq t) => String -> t -> [Test]
testUnixTimeInstance tn t
  = (map
       (\(i64,s)->
        testProperty ("(" ++ tn ++ ".fromUnixSeconds " ++ show i64 ++ ") == Just " ++ show s)
        $  (fromUnixSeconds i64) == Just (fromString s `asTypeOf` t)
       )
       unixEpochMsRfc3339TimeTuples
      )
      ++
      (map
       (\(i64,s)->
        testProperty ("(" ++ tn ++ ".toUnixSeconds (" ++ show s ++ ") == " ++ show i64)
        $ toUnixSeconds (fromString s `asTypeOf` t) == i64
       )
       unixEpochMsRfc3339TimeTuples
      )

testDateInstance :: (Date t, Eq t) => String -> t -> [Test]
testDateInstance tn t
  = [ testProperty ("instance Date " ++ tn ++ ": year dat1")
      $ (dat1 >>= return . year)  == Just 1972
    , testProperty ("instance Date " ++ tn ++ ": month dat1")
      $ (dat1 >>= return . month) == Just 7
    , testProperty ("instance Date " ++ tn ++ ": day dat1")
      $ (dat1 >>= return . day)   == Just 23
    ]
    ++ map (\(n,d)-> testProperty ("instance Date " ++ tn ++ ": " ++ n)
                   $ d == Nothing
           ) invalidDates
  where
    dat1 = (setYear 1972 epoch >>= setMonth 7 >>= setDay 23) `asTypeOf` (Just t)
    invalidDates
      = [-- * invalid dates
         -- ** month or day out of bound bound
          ("inv001", (setYear 1973 epoch >>= setMonth 0               ) `asTypeOf` (Just t))
        , ("inv002", (setYear 1973 epoch >>= setMonth 13              ) `asTypeOf` (Just t))
        , ("inv003", (setYear 1973 epoch                 >>= setDay 00) `asTypeOf` (Just t))
         -- ** day beyond upper bound
        , ("inv004", (setYear 1973 epoch >>= setMonth 1  >>= setDay 32) `asTypeOf` (Just t))
        , ("inv005", (setYear 1973 epoch >>= setMonth 2  >>= setDay 29) `asTypeOf` (Just t))
        , ("inv006", (setYear 1973 epoch >>= setMonth 3  >>= setDay 32) `asTypeOf` (Just t))
        , ("inv007", (setYear 1973 epoch >>= setMonth 4  >>= setDay 31) `asTypeOf` (Just t))
        , ("inv008", (setYear 1973 epoch >>= setMonth 5  >>= setDay 32) `asTypeOf` (Just t))
        , ("inv009", (setYear 1973 epoch >>= setMonth 6  >>= setDay 31) `asTypeOf` (Just t))
        , ("inv010", (setYear 1973 epoch >>= setMonth 7  >>= setDay 32) `asTypeOf` (Just t))
        , ("inv011", (setYear 1973 epoch >>= setMonth 8  >>= setDay 32) `asTypeOf` (Just t))
        , ("inv012", (setYear 1973 epoch >>= setMonth 9  >>= setDay 31) `asTypeOf` (Just t))
        , ("inv013", (setYear 1973 epoch >>= setMonth 10 >>= setDay 32) `asTypeOf` (Just t))
        , ("inv014", (setYear 1973 epoch >>= setMonth 11 >>= setDay 31) `asTypeOf` (Just t))
        , ("inv015", (setYear 1973 epoch >>= setMonth 12 >>= setDay 32) `asTypeOf` (Just t))
        ]

unixEpochMsRfc3339TimeTuples :: [(Rational,String)]
unixEpochMsRfc3339TimeTuples
  = [ ( -62167219200.000, "0000-01-01T00:00:00Z")     -- verified against moment.js (lowest possible date)
    , ( -62162208000.000, "0000-02-28T00:00:00Z")     -- -62162035200000 - (2*24*60*60*1000)
    , ( -62162121600.000, "0000-02-29T00:00:00Z")     -- -62162035200000 - (24*60*60*1000)
    , ( -62162035200.000, "0000-03-01T00:00:00Z")     -- verified against moment.js
    , ( -62135596800.000, "0001-01-01T00:00:00Z")     -- verified against moment.js
    , ( -12275625600.000, "1581-01-01T00:00:00Z")     -- verified against moment.js
    , ( -12244089600.000, "1582-01-01T00:00:00Z")     -- verified against moment.js
    , ( -12212553600.000, "1583-01-01T00:00:00Z")     -- verified against moment.js
    , (  -2208988800.000, "1900-01-01T00:00:00Z")     -- verified against moment.js
    , (  -2203891200.000, "1900-03-01T00:00:00Z")     -- verified against moment.js (1900 is not a leap year)
    , (           -1.000, "1969-12-31T23:59:59Z")     -- verified against moment.js
    , (            0.000, "1970-01-01T00:00:00Z")     -- verified against moment.js
    , (    951696000.000, "2000-02-28T00:00:00Z")     -- verified against moment.js (2000 is a leap year)
    , (   1234234234.000, "2009-02-10T02:50:34Z")     -- verified against moment.js
    , (   1330473600.000, "2012-02-29T00:00:00Z")     -- verified against moment.js (2012 is a leap year)
    , (   1330559999.999, "2012-02-29T23:59:59.999Z") -- verified against moment.js
    , (   1330560000.000, "2012-03-01T00:00:00Z")     -- verified against moment.js
    , (   1330560000.001, "2012-03-01T00:00:00.001Z") -- verified against moment.js
    , (   1334491994.000, "2012-04-15T12:13:14Z")     -- verified against moment.js
    , (   2177410394.000, "2038-12-31T12:13:14Z")     -- verified against moment.js
    , (  32503680000.000, "3000-01-01T00:00:00Z")     -- verified against moment.js
    , ( 190288396800.000, "8000-01-01T00:00:00Z")     -- verified against moment.js
    , ( 221845392000.000, "9000-01-01T00:00:00Z")     -- verified against moment.js
    , ( 240779520000.000, "9600-01-01T00:00:00Z")     -- verified against moment.js
    , ( 253370764800.000, "9999-01-01T00:00:00Z")     -- verified against moment.js
    , ( 253402214400.000, "9999-12-31T00:00:00Z")     -- verified against moment.js
    , ( 253402300799.999, "9999-12-31T23:59:59.999Z") -- verified against moment.js (highest possible date)
    ]

