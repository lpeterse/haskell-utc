{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.UTC.Literals
  ( Date
  , Time
  , DateTime
  ) where

import           Control.Applicative        (liftA2, (*>), (<*))
import           Control.Monad              (guard)
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text       (Parser, decimal, digit, parseOnly,
                                             skip, char)
import           Data.Char                  (ord)
import           Data.Maybe                 (fromJust, isJust)
import           Data.Monoid                ((<>))
import           Data.Ratio
import qualified Data.Text                  as T
import           GHC.Exts                   (IsString (..))

import           Data.UTC.Class.Epoch
import           Data.UTC.Class.IsDate
import           Data.UTC.Class.IsTime
import           Data.UTC.Type.Date
import           Data.UTC.Type.DateTime
import           Data.UTC.Type.Time


instance IsString Date where
  fromString s = case parseOnly (iso8601Parser <* endOfInput) (T.pack s) of
    Right d -> d
    _ -> error ("Could not parse date '" <> s <> "' (expected YYYY-MM-DD)")

decimal1 :: Parser Integer
decimal1 = do
  d <- digit
  return (fromIntegral (ord d - 48))

decimal2 :: Parser Integer
decimal2 =
  do d1 <- decimal1
     d2 <- decimal1
     return $ d1 * 10  + d2

iso8601Parser :: Parser Date
iso8601Parser = do
  y <- decimal <* skip (=='-')
  m <- decimal2 <* skip (=='-')
  d <- decimal2
  let r = setYear y epoch >>= setMonth m >>= setDay d
  guard (isJust r)
  return (fromJust r)


instance IsString Time where
  fromString s = case parseOnly (timeParser <* endOfInput) (T.pack s) of
    Right t -> t
    _ -> error ("Could not parse time '"<> s <> "' (expected HH:MM:SS[.mmm])")

timeParser :: Parser Time
timeParser = do
  h <- decimal2 <* skip (==':')
  m <- decimal2 <* skip (==':')
  s <- decimal2
  f <- option 0 timeSecfrac <?> "time fraction"
  let r = setHour h epoch >>= setMinute m >>= setSecond s >>= setSecondFraction f
  guard (isJust r)
  return (fromJust r)

timeSecfrac :: Parser (Ratio Integer)
timeSecfrac = do
  decs <- char '.' *> many1 digit
  return $ (read decs :: Integer) % (10 ^ length decs)


instance IsString DateTime where
  fromString s = case parseOnly datetimeParser (T.pack s) of
    Right dt -> dt
    _ -> error ("Could not parse datetime '"<> s <> "' (expected YYYY-MM-DD HH:MM:SS[.mmm])")

datetimeParser :: Parser DateTime
datetimeParser = liftA2 DateTime (iso8601Parser <* char ' ') (timeParser <* endOfInput)
