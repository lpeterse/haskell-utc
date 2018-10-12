module Data.UTC.Class.IsDate.Test where

import Test.QuickCheck
import Test.Framework (testGroup)
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.UTC

test :: Test
test
  = testGroup "Data.UTC.Class.IsDate"
  $ [ testGroup "addYears"
      [ testProperty "addYears 1 '2000-02-28'"
      $ let d = setYear 2000 (epoch :: Date) >>= setMonth 2 >>= setDay 28 >>= addYears 1
        in  conjoin
             [ (year  `fmap` d) === Just 2001
             , (month `fmap` d) === Just 2
             , (day   `fmap` d) === Just 28
             ]
      , testProperty "addYears 1 '2000-02-29'"
      $ let d = setYear 2000 (epoch :: Date) >>= setMonth 2 >>= setDay 29 >>= addYears 1
        in  conjoin
             [ (year  `fmap` d) === Just 2001
             , (month `fmap` d) === Just 2
             , (day   `fmap` d) === Just 28
             ]
      , testProperty "addYears 4 '2000-02-29'"
      $ let d = setYear 2000 (epoch :: Date) >>= setMonth 2 >>= setDay 29 >>= addYears 4
        in  conjoin
             [ (year  `fmap` d) === Just 2004
             , (month `fmap` d) === Just 2
             , (day   `fmap` d) === Just 29
             ]
      , testProperty "addYears (-4) '2000-02-29'"
      $ let d = setYear 2000 (epoch :: Date) >>= setMonth 2 >>= setDay 29 >>= addYears (-4)
        in  conjoin
             [ (year  `fmap` d) === Just 1996
             , (month `fmap` d) === Just 2
             , (day   `fmap` d) === Just 29
             ]
      ]
    , testGroup "addMonths"
      [ testProperty "addMonths 1 '2000-01-31'"
      $ let d = setYear 2000 (epoch :: Date) >>= setMonth 1 >>= setDay 31 >>= addMonths 1
        in  conjoin
             [ (year  `fmap` d) === Just 2000
             , (month `fmap` d) === Just 2
             , (day   `fmap` d) === Just 29
             ]
      , testProperty "addMonths 2 '2000-01-31'"
      $ let d = setYear 2000 (epoch :: Date) >>= setMonth 1 >>= setDay 31 >>= addMonths 2
        in  conjoin
             [ (year  `fmap` d) === Just 2000
             , (month `fmap` d) === Just 3
             , (day   `fmap` d) === Just 31
             ]
      , testProperty "addMonths 3 '2000-01-31'"
      $ let d = setYear 2000 (epoch :: Date) >>= setMonth 1 >>= setDay 31 >>= addMonths 3
        in  conjoin
             [ (year  `fmap` d) === Just 2000
             , (month `fmap` d) === Just 4
             , (day   `fmap` d) === Just 30
             ]
      , testProperty "addMonths 1 '2001-01-31'"
      $ let d = setYear 2001 (epoch :: Date) >>= setMonth 1 >>= setDay 31 >>= addMonths 1
        in  conjoin
             [ (year  `fmap` d) === Just 2001
             , (month `fmap` d) === Just 2
             , (day   `fmap` d) === Just 28
             ]
      , testProperty "addMonths (-47) '2000-01-31'"
      $ let d = setYear 2000 (epoch :: Date) >>= setMonth 1 >>= setDay 31 >>= addMonths (-47)
        in  conjoin
             [ (year  `fmap` d) === Just 1996
             , (month `fmap` d) === Just 2
             , (day   `fmap` d) === Just 29
             ]
      ]
    , testGroup "addDays"
      [ testProperty "addDays 366 '2000-01-01'"
      $ let d = setYear 2000 (epoch :: Date) >>= setMonth 1 >>= setDay 1 >>= addDays 366
        in  conjoin
             [ (year  `fmap` d) === Just 2001
             , (month `fmap` d) === Just 1
             , (day   `fmap` d) === Just 1
             ]
      ]
    ]
