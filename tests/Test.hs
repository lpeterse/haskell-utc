module Test ( tests ) where

import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck

import Test.QuickCheck

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Attoparsec.ByteString as Atto

import Data.Tempus
import Data.Tempus.GregorianDateTime
import Data.Tempus.GregorianDateTime.Internal

import System.Random

instance Random GregorianDateTime where
  randomR (_,_) gen
    = random gen
  random gen1
    = ( GregorianDateTime
         { gdtYear     = abs year    `rem` 10000
         , gdtMonth    = abs month   `rem` 12 + 1
         , gdtMDay     = abs mday    `rem` 28 + 1
         , gdtHour     = abs hour    `rem` 24
         , gdtMinute   = abs minute  `rem` 60
         , gdtSecond   = abs second  `rem` 60
         , gdtmSecond  = abs msecond `rem` 1000
         , gdtOffset   = if even unknown
                           then OffsetUnknown
                           else OffsetMinutes (offset `rem` (24*60))
         }
      , gen10
      )
    where
      (year,    gen2)  = next gen1
      (month,   gen3)  = next gen2
      (mday,    gen4)  = next gen3
      (hour,    gen5)  = next gen4
      (minute,  gen6)  = next gen5
      (second,  gen7)  = next gen6
      (msecond, gen8)  = next gen7
      (unknown, gen9)  = next gen8
      (offset,  gen10) = next gen9

tests :: IO [Test]
tests 
  = return [

    testProperty "epoch rendering"
    $ show (epoch :: UnixTime) === "1970-01-01T00:00:00Z"

  , testProperty "random GregorianDateTime RFC3339 parse <-> render"
    $ forAll (choose (undefined, undefined))
    $ \gdt-> let bs   = BSL.toStrict (BS.toLazyByteString (rfc3339Builder gdt))
                 gdt' = case Atto.parseOnly rfc3339Parser bs of
                          Left e  -> error $ e ++ "(" ++ show bs ++ ")"
                          Right s -> s
             in  if gdt == gdt'
                   then True
                   else error $ show bs
  ]

