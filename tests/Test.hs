{-# LANGUAGE OverloadedStrings #-}
module Test ( tests ) where

import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck

import Test.QuickCheck

import Data.Int
import Data.String
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Attoparsec.ByteString as Atto

import Data.Tempus
import Data.Tempus.GregorianTime
import Data.Tempus.GregorianTime.Internal

tests :: IO [Test]
tests 
  = return $

      (map
       (\(i64,s)->
        testProperty ("fromUnixTime " ++ show i64 ++ " == Just " ++ show s)
        $ fromUnixTime (UnixTime i64) == Just (fromString s)
       )
       unixTimeGregorianTimeTuples
      )
      ++
      [ -- 1ms before lowest possible date
        testProperty ("fromUnixTime -62167219200001 == Nothing")
        $ fromUnixTime (UnixTime (-62167219200001)) == Nothing
        -- 1ms after highest possible date
      , testProperty ("fromUnixTime 253402300800000 == Nothing")
        $ fromUnixTime (UnixTime (253402300800000)) == Nothing
      , testProperty ("fromUnixTime        minBound == Nothing")
        $ fromUnixTime (UnixTime minBound) == Nothing
      , testProperty ("fromUnixTime        maxBound == Nothing")
        $ fromUnixTime (UnixTime maxBound) == Nothing
      ]
      

unixTimeGregorianTimeTuples :: [(Int64,String)]
unixTimeGregorianTimeTuples
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

