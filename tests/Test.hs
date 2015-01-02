{-# LANGUAGE OverloadedStrings #-}
module Test ( tests ) where

import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck

import Test.QuickCheck

import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Attoparsec.ByteString as Atto

import Data.Tempus
import Data.Tempus.GregorianTime
import Data.Tempus.GregorianTime.Internal

import System.Random



interestingValidDates :: [GregorianTime]
interestingValidDates
  = [ "1969-07-21T02:56:00Z"
    , "1970-01-01T00:00:00Z"

    ]

tests :: IO [Test]
tests 
  = return [

--    testProperty "random GregorianDateTime RFC3339 parse <-> render"
--    $ forAll (choose (undefined, undefined))
--    $ \gdt-> let bs   = toRfc3339ByteString gdt
--                 gdt' = case Atto.parseOnly rfc3339Parser bs of
--                          Left e  -> error $ e ++ "(" ++ show bs ++ ")"
--                          Right s -> s
--             in  if gdt == gdt'
--                   then True
--                   else error $ show bs
  ]

