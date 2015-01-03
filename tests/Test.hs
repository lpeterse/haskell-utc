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
  = return

      (map
       (\(i64,s)->
        testProperty ("fromUnixTime " ++ show i64 ++ " == " ++ show s)
        $ fromUnixTime (UnixTime i64) == Just (fromString s)
       )
       unixTimeGregorianTimeTuples
      )

unixTimeGregorianTimeTuples :: [(Int64,String)]
unixTimeGregorianTimeTuples
  = [ (        -1000, "1969-12-31T23:59:59Z")
    , (            0, "1970-01-01T00:00:00Z")
    , (1234234234000, "2009-02-10T02:50:34Z")
    ]

