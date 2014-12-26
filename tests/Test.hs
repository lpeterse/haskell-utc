module Test ( tests ) where

import Distribution.TestSuite

import Data.Tempus

tests :: IO [Test]
tests = return 
         [ Test test001
         , Test test002
         ]

test001 :: TestInstance
test001 = TestInstance
        { name = "show epoch"
        , tags = []
        , options = []
        , setOption = \_ _ -> Right test001
        , run = do return $ Finished 
                          $ if show epoch == "1970-01-01T00:00:00Z"
                              then Pass
                              else Fail (show epoch)
        }

test002 :: TestInstance
test002 = TestInstance
        { run = return $ Finished $ Fail "Always fails!"
        , name = "fails"
        , tags = []
        , options = []
        , setOption = \_ _ -> Right test001
        }