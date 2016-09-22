{-# LANGUAGE OverloadedStrings #-}

module Data.UTC.Literals.Test where

import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.QuickCheck.Property             (protect)

import           Control.Applicative                  (liftA2)
import           Data.Monoid                          ((<>))
import           Data.Ratio                           ((%))
import           Data.UTC.Literals                    ()
import           GHC.Exts                             (IsString (..))

import           Data.UTC.Class.Epoch
import           Data.UTC.Class.IsDate
import           Data.UTC.Class.IsTime
import           Data.UTC.Type.Date
import           Data.UTC.Type.DateTime
import           Data.UTC.Type.Time

test :: Test
test
  = testGroup "Data.UTC.Literals"
    [ testValidLiteral "2016-02-01" (setYear 2016 (epoch :: Date) >>= setMonth 2 >>= setDay 1)
    , testInvalidDateLiteral "2016-02-010"
    , testInvalidDateLiteral "2016-002-01"
    , testInvalidDateLiteral "2016-02-30"
    , testInvalidDateLiteral "2016-13-30"
    , testValidLiteral "12:30:59" (setHour 12 (epoch :: Time) >>= setMinute 30 >>= setSecond 59)
    , testValidLiteral "12:30:59.8234" (setHour 12 (epoch :: Time) >>= setMinute 30 >>= setSecond 59 >>= setSecondFraction (8234 % 10000))
    , testInvalidTimeLiteral "12:30:59.82u34"
    , testInvalidTimeLiteral "12:30:60"
    , testInvalidTimeLiteral "012:30:59"
    , testInvalidTimeLiteral "12:030:59"
    , testInvalidTimeLiteral "12:30:059"
    , testValidLiteral "2016-02-01 12:30:59" $ Just (DateTime "2016-02-01" "12:30:59")
    , testValidLiteral "2016-02-01 12:30:59.8234" $ liftA2 DateTime (setYear 2016 (epoch :: Date) >>= setMonth 2 >>= setDay 1) (setHour 12 (epoch :: Time) >>= setMinute 30 >>= setSecond 59  >>= setSecondFraction (8234 % 10000))
    , testInvalidDateTimeLiteral "2016-02-30 12:30:59.8234"
    , testInvalidDateTimeLiteral "2016-02-01 12:30:60.8234"
    ]

testValidLiteral :: (Eq a, Show a, IsString a) => String -> Maybe a -> Test
testValidLiteral s expected = testProperty ("'" <> s <> "' is a valid literal") $ Just (fromString s) === expected

testInvalidDateLiteral :: String -> Test
testInvalidDateLiteral s = testProperty ("'" <> s <> "' is an invalid literal Date") $ monadicIO (do a <- run x; assert (a == epoch))
  where x = protect (const epoch) (return (fromString s :: Date))

testInvalidTimeLiteral :: String -> Test
testInvalidTimeLiteral s = testProperty ("'" <> s <> "' is an invalid literal Time") $ monadicIO (do a <- run x; assert (a == epoch))
  where x = protect (const epoch) (return (fromString s :: Time))

testInvalidDateTimeLiteral :: String -> Test
testInvalidDateTimeLiteral s = testProperty ("'" <> s <> "' is an invalid literal DateTime") $ monadicIO (do a <- run x; assert (a == epoch))
  where x = protect (const epoch) (return (fromString s :: DateTime))
