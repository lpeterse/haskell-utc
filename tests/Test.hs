module Test ( tests ) where

import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck

import Test.QuickCheck

import Data.Tempus
import Data.Tempus.Class

import Data.Word

tests :: IO [Test]
tests 
  = return [

    testProperty "epoch rendering"
    $ show (epoch :: UnixTime) === "1970-01-01T00:00:00Z"

  , testProperty "unixOffset from -2^48 to 2^48-1"
    $ forAll 
        (choose (negate (2 ^ 48), (2 ^ 48) - 1))
        (\x-> x == toUnixTime (fromUnixTime x))

  ]

