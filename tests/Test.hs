module Test ( tests ) where

import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck

import Test.QuickCheck

import Data.Tempus

import Data.Word

tests :: IO [Test]
tests = return 
         [ test001
         , test002
         ]

test001 :: Test
test001
  = testProperty "test001"
  $ show epoch === "1970-01-01T00:00:00Z"

test002 :: Test
test002
  = testProperty "test002"
  $ forAll 
      (choose (0, 1000000000))
      (\x-> x == toUnixTime (fromUnixTime x))