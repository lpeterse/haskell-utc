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
  = return $

      (map
       (\(i64,s)->
        testProperty ("(fromSecondsSinceCommonEpoch " ++ show i64 ++ " :: Maybe Rfc3339Time) == Just " ++ show s)
        $  (fromSecondsSinceCommonEpoch i64 :: Maybe Rfc3339Timestamp) == Just (fromString s)
       )
       commonEpochMsRfc3339TimeTuples
      )
      ++
      (map
       (\(i64,s)->
        testProperty ("secondsSinceCommonEpoch (" ++ show s ++ " :: Rfc3339Time) == " ++ show i64)
        $ toSecondsSinceCommonEpoch (fromString s :: Rfc3339Timestamp) == i64
       )
       commonEpochMsRfc3339TimeTuples
      )
      ++
      [ testProperty ("yearMonthDayToDays (daysToYearMonthDay x) == x")
        $ forAll (choose (0, 3652424)) -- 0000-01-01 to 9999-12-31
        $ \x-> yearMonthDayToDays (daysToYearMonthDay x) == x
      ]
  
commonEpochMsRfc3339TimeTuples :: [(Rational,String)]
commonEpochMsRfc3339TimeTuples
  = map
      (\(i,s)-> (i + 62167219200.000, s))
      unixEpochMsRfc3339TimeTuples

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

