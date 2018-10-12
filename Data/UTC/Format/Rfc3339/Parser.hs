{-# LANGUAGE Safe #-}
module Data.UTC.Format.Rfc3339.Parser
  ( rfc3339Parser
  ) where

import Control.Monad.Catch

import Data.Ratio

import Data.Attoparsec.ByteString ( Parser, skipWhile, choice, option, satisfy )
import Data.Attoparsec.ByteString.Char8 ( char, isDigit_w8 )

import Data.UTC.Class.Epoch
import Data.UTC.Class.IsDate
import Data.UTC.Class.IsTime
import Data.UTC.Type.Local

rfc3339Parser :: (MonadThrow m, IsDate t, IsTime t) => Parser (m (Local t))
rfc3339Parser
  = do year'    <- dateFullYear
       _        <- char '-'
       month'   <- dateMonth
       _        <- char '-'
       day'     <- dateMDay
       _        <- char 'T'
       hour'    <- timeHour
       _        <- char ':'
       minute'  <- timeMinute
       _        <- char ':'
       second'  <- timeSecond
       secfrac' <- option 0 timeSecfrac
       offset'  <- timeOffset

       return $ do datetime <- setYear           year' epoch
                           >>= setMonth          month'
                           >>= setDay            day'
                           >>= setHour           hour'
                           >>= setMinute         minute'
                           >>= setSecond         second'
                           >>= setSecondFraction secfrac'
                   return (Local offset' datetime)
  where
    dateFullYear
      = decimal4
    dateMonth
      = decimal2
    dateMDay
      = decimal2
    timeHour
      = decimal2
    timeMinute
      = decimal2
    timeSecond
      = decimal2
    timeSecfrac
      = do _ <- char '.'
           choice
             [ do d <- decimal3
                  skipWhile isDigit_w8
                  return (d % 1000)
             , do d <- decimal2
                  return (d % 100)
             , do d <- decimal1
                  return (d % 10)
             ]
    timeOffset
      = choice
          [ do _  <- char 'Z'
               return $ Just 0
          , do _  <- char '+'
               x1 <- decimal2
               _  <- char ':'
               x2 <- decimal2
               return $ Just
                      $ (x1 * 3600 + x2 * 60) % 1
          , do _  <- char '-'
               _  <- char '0'
               _  <- char '0'
               _  <- char ':'
               _  <- char '0'
               _  <- char '0'
               return Nothing
          , do _  <- char '-'
               x1 <- decimal2
               _  <- char ':'
               x2 <- decimal2
               return $ Just
                      $ negate
                      $ (x1 * 3600 + x2 * 60) % 1
          ]

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
