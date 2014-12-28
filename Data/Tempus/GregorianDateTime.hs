module Data.Tempus.GregorianDateTime
  ( -- * Type
    GregorianDateTime()
  , rfc3339Parser
  ) where

import Data.Attoparsec.ByteString ( Parser, skipWhile, choice, option, satisfy )
import Data.Attoparsec.ByteString.Char8 ( char, isDigit_w8 )

-- | A time and date representation based on years, months and days.
-- This representation is closest to RFC3339 (a stricter profile of ISO8601) strings. 
--
-- Use it if
--
--   * you are parsing and rendering RFC3339 strings and only use
--     Gregorian operations in between.
--   * you need to be able to represent leap seconds.
--   * you need to be able to represent a local offset (timezone).
--   * you don't care about a value's memory footprint.
data GregorianDateTime
   = GregorianDateTime
     { grYear    :: Int
     , grMonth   :: Int
     , grDay     :: Int
     , grHour    :: Int
     , grMinute  :: Int
     , grSecond  :: Int
     , grmSecond :: Int
     , grOffset  :: Offset
     }

data Offset
   = Offset Int
   | OffsetUnknown

-- | 
rfc3339Parser :: Parser GregorianDateTime
rfc3339Parser 
  = do fullDate
       char 'T'
       fullTime
       return GregorianDateTime
              { grYear     = 0
              , grMonth    = 0
              , grDay      = 0
              , grHour     = 0
              , grMinute   = 0
              , grSecond   = 0
              , grmSecond  = 0
              , grOffset   = Offset 0
              }
  where
    dateFullYear
      = decimal4
    dateMonth
      = decimal2
    dateMDay
      = decimal2
    fullDate
      = do dateFullYear
           char '-'
           dateMonth
           char '-'
           dateMDay
    timeHour
      = decimal2
    timeMinute
      = decimal2
    timeSecond
      = decimal2
    timeSecfrac :: Parser Int
    timeSecfrac
      = do char '.'
           choice
             [ do d <- decimal3
                  skipWhile isDigit_w8
                  return d
             , do d <- decimal2
                  return (d * 10)
             , do d <- decimal1
                  return d
             ]
    timeOffset
      = choice
          [ do char 'Z'
               return $ Offset 0
          , do char '+'
               x1 <- decimal2
               char ':'
               x2 <- decimal2
               return $ Offset
                      $ x1 * 360000
                      + x2 * 60000
          , do char '-'
               char '0'
               char '0'
               char ':'
               char '0'
               char '0'
               return OffsetUnknown
          , do char '-'
               x1 <- decimal2
               char ':'
               x2 <- decimal2
               return $ Offset
                      $ negate
                      $ x1 * 360000
                      + x2 * 60000
          ]
    partialTime
      = do timeHour
           char ':'
           timeMinute
           char ':'
           timeSecond
           option 0 timeSecfrac
    fullTime
      = do partialTime
           timeOffset

    decimal1
      = do w8 <- satisfy isDigit_w8
           return (fromIntegral (w8 - 48))
    decimal2
      = do d1 <- decimal1
           d2 <- decimal1
           return $ d1 * 10
                  + d2
    decimal3
      = do d1 <- decimal1
           d2 <- decimal1
           d3 <- decimal1
           return $ d1 * 100
                  + d2 * 10
                  + d3
    decimal4
      = do d1 <- decimal1
           d2 <- decimal1
           d3 <- decimal1
           d4 <- decimal1
           return $ d1 * 1000
                  + d2 * 100
                  + d3 * 10
                  + d4
