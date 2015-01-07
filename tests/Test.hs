{-# LANGUAGE OverloadedStrings #-}
module Test ( tests ) where

import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck

import Test.QuickCheck

import Data.Int
import Data.String
import Data.Maybe

import Data.Tempus
import Data.Tempus.GregorianTime
import Data.Tempus.UnixTime
import Data.Tempus.Internal

tests :: IO [Test]
tests 
  = return $

      (map
       (\(i64,s)->
        testProperty ("(fromMilliSecondsCommonEpoch " ++ show i64 ++ " :: Maybe GregorianTime) == Just " ++ show s)
        $  (fromMilliSecondsCommonEpoch i64 :: Maybe GregorianTime) == Just (fromString s)
       )
       commonEpochMsRfc3339TimeTuples
      )
      ++
      [ -- 1ms before lowest possible date
        testProperty ("fromMilliSecondsCommonEpoch (-1) == (Nothing :: Maybe GregorianTime)")
        $ fromMilliSecondsCommonEpoch (-62167219200001) == (Nothing :: Maybe GregorianTime)
        -- 1ms after highest possible date
      , testProperty ("fromMilliSecondsCommonEpoch 315569520000000 == (Nothing :: Maybe GregorianTime)")
        $ fromMilliSecondsCommonEpoch 315569520000000 == (Nothing :: Maybe GregorianTime)
      ]
      ++
      (map
       (\(i64,s)->
        testProperty ("toMilliSecondsCommonEpoch (" ++ show s ++ " :: GregorianTime) == Just " ++ show i64)
        $ toMilliSecondsCommonEpoch (fromString s :: GregorianTime) == Just i64
       )
       commonEpochMsRfc3339TimeTuples
      )
      ++
      [ testProperty ("yearMonthDayToDays (daysToYearMonthDay x) == x")
        $ forAll (choose (0, 3652424)) -- 0000-01-01 to 9999-12-31
        $ \x-> fromMaybe False
        $ do ymd <- daysToYearMonthDay x
             x'  <- yearMonthDayToDays ymd
             -- error (show x')
             return (x == x')
      ]
  
commonEpochMsRfc3339TimeTuples :: [(Integer,String)]
commonEpochMsRfc3339TimeTuples
  = map
      (\(i,s)-> (i + 62167219200000, s))
      unixEpochMsRfc3339TimeTuples

unixEpochMsRfc3339TimeTuples :: [(Integer,String)]
unixEpochMsRfc3339TimeTuples
  = [ ( -62167219200000, "0000-01-01T00:00:00Z")     -- verified against moment.js (lowest possible date)
    , ( -62162208000000, "0000-02-28T00:00:00Z")     -- -62162035200000 - (2*24*60*60*1000)
    , ( -62162121600000, "0000-02-29T00:00:00Z")     -- -62162035200000 - (24*60*60*1000)
    , ( -62162035200000, "0000-03-01T00:00:00Z")     -- verified against moment.js
    , ( -62135596800000, "0001-01-01T00:00:00Z")     -- verified against moment.js
    , ( -12275625600000, "1581-01-01T00:00:00Z")     -- verified against moment.js
    , ( -12244089600000, "1582-01-01T00:00:00Z")     -- verified against moment.js
    , ( -12212553600000, "1583-01-01T00:00:00Z")     -- verified against moment.js
    , (  -2208988800000, "1900-01-01T00:00:00Z")     -- verified against moment.js
    , (  -2203891200000, "1900-03-01T00:00:00Z")     -- verified against moment.js (1900 is not a leap year)
    , (           -1000, "1969-12-31T23:59:59Z")     -- verified against moment.js
    , (               0, "1970-01-01T00:00:00Z")     -- verified against moment.js
    , (    951696000000, "2000-02-28T00:00:00Z")     -- verified against moment.js (2000 is a leap year)
    , (   1234234234000, "2009-02-10T02:50:34Z")     -- verified against moment.js
    , (   1330473600000, "2012-02-29T00:00:00Z")     -- verified against moment.js (2012 is a leap year)
    , (   1330559999999, "2012-02-29T23:59:59.999Z") -- verified against moment.js
    , (   1330560000000, "2012-03-01T00:00:00Z")     -- verified against moment.js
    , (   1330560000001, "2012-03-01T00:00:00.001Z") -- verified against moment.js
    , (   1334491994000, "2012-04-15T12:13:14Z")     -- verified against moment.js
    , (   2177410394000, "2038-12-31T12:13:14Z")     -- verified against moment.js
    , (  32503680000000, "3000-01-01T00:00:00Z")     -- verified against moment.js
    , ( 190288396800000, "8000-01-01T00:00:00Z")     -- verified against moment.js
    , ( 221845392000000, "9000-01-01T00:00:00Z")     -- verified against moment.js
    , ( 240779520000000, "9600-01-01T00:00:00Z")     -- verified against moment.js
    , ( 253370764800000, "9999-01-01T00:00:00Z")     -- verified against moment.js
    , ( 253402214400000, "9999-12-31T00:00:00Z")     -- verified against moment.js
    , ( 253402300799999, "9999-12-31T23:59:59.999Z") -- verified against moment.js (highest possible date)
    ]

