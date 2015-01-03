{-# LANGUAGE OverloadedStrings #-}
module Data.Tempus.GregorianTime
  ( -- * Type
    GregorianTime()
    -- * RFC 3339
    -- ** Parsing
  , parseRfc3339String
  , parseRfc3339Text
  , parseRfc3339LazyText
  , parseRfc3339ByteString
  , parseRfc3339LazyByteString
    -- ** Rendering
  , renderRfc3339String
  , renderRfc3339Text
  , renderRfc3339LazyText
  , renderRfc3339ByteString
  , renderRfc3339LazyByteString
    -- ** Rendering (Low-Level)
  , rfc3339Parser
  , rfc3339Builder

  -- * Conversion
  -- ** Unix Time
  , toUnixTime, fromUnixTime
    -- * Validation
  , validate
  , isLeapYear'
  , yearToDays
  ) where

import Control.Monad

import Debug.Trace

import Data.Int
import Data.Monoid
import Data.String

import Data.Attoparsec.ByteString ( Parser, parseOnly, skipWhile, choice, option, satisfy )
import Data.Attoparsec.ByteString.Char8 ( char, isDigit_w8 )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Data.Tempus.Class
import Data.Tempus.UnixTime
import Data.Tempus.GregorianTime.Internal

validate :: MonadPlus m => GregorianTime -> m GregorianTime
validate gdt
  = do validateYear
       validateMonthAndDay
       validateMinutes
       validateMilliSeconds
       validateOffset
       return gdt
  where
    validateYear
      = if 0 <= gdtYear gdt && gdtYear gdt <= 9999
          then return ()
          else mzero
    validateMonthAndDay
      = if 1 <= gdtMonth gdt && gdtMonth gdt <= 12
          then case gdtMonth gdt of
                 1  -> validateDays31
                 2  -> validateDays28or29
                 3  -> validateDays31
                 4  -> validateDays30
                 5  -> validateDays31
                 6  -> validateDays30
                 7  -> validateDays31
                 8  -> validateDays31
                 9  -> validateDays30
                 10 -> validateDays31
                 11 -> validateDays30
                 12 -> validateDays31
                 _  -> mzero
          else mzero
    validateDays31
      | 1 <= gdtDay gdt && gdtDay gdt <= 31           = return ()
      | otherwise                                     = mzero
    validateDays30
      | 1 <= gdtDay gdt && gdtDay gdt <= 30           = return ()
      | otherwise                                     = mzero
    validateDays28or29
      | 1 <= gdtDay gdt && gdtDay gdt <= 28           = return ()
      | gdtDay gdt == 29 && isLeapYear gdt            = return ()
      | otherwise                                     = mzero
    validateMinutes
      | 0 <= gdtMinutes gdt && gdtMinutes gdt < 24*60 = return ()
      | otherwise                                     = mzero
    validateMilliSeconds
      | 0 <= gdtMinutes gdt && gdtMinutes gdt < 61000 = return ()
      | otherwise                                     = mzero
    validateOffset
      = case gdtOffset gdt of
          OffsetUnknown   -> return ()
          OffsetMinutes o -> if negate (24*60) < o && o < (24*60)
                               then return ()
                               else mzero

rfc3339Builder :: GregorianTime -> BS.Builder
rfc3339Builder gdt
  = mconcat
      [ BS.word16HexFixed (y3*16*16*16 + y2*16*16 + y1*16 + y0)
      , BS.char7 '-'
      , BS.word8HexFixed (m1*16 + m0)
      , BS.char7 '-'
      , BS.word8HexFixed (d1*16 + d0)
      , BS.char7 'T'
      , BS.word8HexFixed (h1*16 + h0)
      , BS.char7 ':'
      , BS.word8HexFixed (n1*16 + n0)
      , BS.char7 ':'
      , BS.word8HexFixed (s1*16 + s0)
      , if f0 == 0
          then if f1 == 0
                 then if f2 == 0
                        then mempty
                        else BS.char7 '.' `mappend` BS.intDec f2
                 else BS.char7 '.' `mappend` BS.intDec f2 `mappend` BS.intDec f1
          else BS.char7 '.' `mappend` BS.intDec f2 `mappend` BS.intDec f1 `mappend` BS.intDec f0
      , case gdtOffset gdt of
          OffsetUnknown   -> BS.string7 "-00:00"
          OffsetMinutes 0 -> BS.char7 'Z'
          OffsetMinutes o -> let oh1 = fromIntegral $ abs o `quot` 600          `rem` 10
                                 oh0 = fromIntegral $ abs o `quot` 60           `rem` 10
                                 om1 = fromIntegral $ abs o `rem`  60 `quot` 10 `rem` 10
                                 om0 = fromIntegral $ abs o `rem`  60           `rem` 10
                             in  mconcat
                                   [ if o < 0
                                       then BS.char7 '-'
                                       else BS.char7 '+'
                                   , BS.word8HexFixed (oh1*16 + oh0)
                                   , BS.char7 ':'
                                   , BS.word8HexFixed (om1*16 + om0)
                                   ]
      ]
  where
    y3 = fromIntegral $ gdtYear         gdt `quot` 1000         `rem` 10
    y2 = fromIntegral $ gdtYear         gdt `quot` 100          `rem` 10
    y1 = fromIntegral $ gdtYear         gdt `quot` 10           `rem` 10
    y0 = fromIntegral $ gdtYear         gdt                     `rem` 10
    m1 = fromIntegral $ gdtMonth        gdt `quot` 10           `rem` 10
    m0 = fromIntegral $ gdtMonth        gdt                     `rem` 10
    d1 = fromIntegral $ gdtDay          gdt `quot` 10           `rem` 10
    d0 = fromIntegral $ gdtDay          gdt                     `rem` 10
    h1 = fromIntegral $ gdtMinutes      gdt `quot` 600          `rem` 10
    h0 = fromIntegral $ gdtMinutes      gdt `quot` 60           `rem` 10
    n1 = fromIntegral $ gdtMinutes      gdt `rem`  60 `quot` 10 `rem` 10
    n0 = fromIntegral $ gdtMinutes      gdt `rem`  60           `rem` 10
    s1 = fromIntegral $ gdtMilliSeconds gdt `quot` 10000        `rem` 10
    s0 = fromIntegral $ gdtMilliSeconds gdt `quot` 1000         `rem` 10
    f2 = fromIntegral $ gdtMilliSeconds gdt `quot` 100          `rem` 10
    f1 = fromIntegral $ gdtMilliSeconds gdt `quot` 10           `rem` 10
    f0 = fromIntegral $ gdtMilliSeconds gdt                     `rem` 10

-- | 
rfc3339Parser :: Parser GregorianTime
rfc3339Parser 
  = do year    <- dateFullYear
       _       <- char '-'
       month   <- dateMonth
       _       <- char '-'
       day     <- dateMDay
       _       <- char 'T'
       hour    <- timeHour
       _       <- char ':'
       minute  <- timeMinute
       _       <- char ':'
       second  <- timeSecond
       msecond <- option 0 timeSecfrac
       offset  <- timeOffset
       validate 
         $ GregorianTime
           { gdtYear          = year
           , gdtMonth         = month
           , gdtDay           = day
           , gdtMinutes       = hour * 60 + minute
           , gdtMilliSeconds  = second * 1000 + msecond
           , gdtOffset        = offset
           }
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
                  return d
             , do d <- decimal2
                  return (d * 10)
             , do d <- decimal1
                  return (d * 100)
             ]
    timeOffset
      = choice
          [ do _  <- char 'Z'
               return $ OffsetMinutes 0
          , do _  <- char '+'
               x1 <- decimal2
               _  <- char ':'
               x2 <- decimal2
               return $ OffsetMinutes
                      $ x1 * 60
                      + x2
          , do _  <- char '-'
               _  <- char '0'
               _  <- char '0'
               _  <- char ':'
               _  <- char '0'
               _  <- char '0'
               return OffsetUnknown
          , do _  <- char '-'
               x1 <- decimal2
               _  <- char ':'
               x2 <- decimal2
               return $ OffsetMinutes
                      $ negate
                      $ x1 * 360000
                      + x2 * 60000
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

renderRfc3339LazyByteString :: GregorianTime -> BSL.ByteString
renderRfc3339LazyByteString gdt
  = BS.toLazyByteString (rfc3339Builder gdt)

renderRfc3339ByteString :: GregorianTime -> BS.ByteString
renderRfc3339ByteString gdt
  = BSL.toStrict (renderRfc3339LazyByteString gdt)

renderRfc3339Text :: GregorianTime -> T.Text
renderRfc3339Text gdt
  = T.decodeUtf8 (renderRfc3339ByteString gdt)

renderRfc3339LazyText :: GregorianTime -> TL.Text
renderRfc3339LazyText gdt
  = TL.decodeUtf8 (renderRfc3339LazyByteString gdt)

renderRfc3339String :: GregorianTime -> String
renderRfc3339String gdt
  = T.unpack (renderRfc3339Text gdt)

parseRfc3339ByteString :: (MonadPlus m) => BS.ByteString -> m GregorianTime
parseRfc3339ByteString s
  = case parseOnly rfc3339Parser s of
      Right t -> return t
      Left e  -> mzero

parseRfc3339LazyByteString :: (MonadPlus m) => BSL.ByteString -> m GregorianTime
parseRfc3339LazyByteString s
  = parseRfc3339ByteString (BSL.toStrict s)

parseRfc3339Text :: (MonadPlus m) => T.Text -> m GregorianTime
parseRfc3339Text s
  = parseRfc3339ByteString (T.encodeUtf8 s)

parseRfc3339LazyText :: (MonadPlus m) => TL.Text -> m GregorianTime
parseRfc3339LazyText s
  = parseRfc3339LazyByteString (TL.encodeUtf8 s)

parseRfc3339String :: (MonadPlus m) => String -> m GregorianTime
parseRfc3339String s
  = parseRfc3339Text (T.pack s)

instance Show GregorianTime where
  show = renderRfc3339String

instance IsString GregorianTime where
  fromString s
    = case parseOnly rfc3339Parser (T.encodeUtf8 $ T.pack s) of
        Right s -> s
        Left  e -> error $ "Invalid Date '" ++ s ++ "'"

instance Tempus GregorianTime where
  isLeapYear gdt
    = isLeapYear' (gdtYear gdt)

  getYear gt
    = return (gdtYear gt)
  getMonth gt
    = return (gdtMonth gt)
  getDay gt
    = return (gdtDay gt)
  getHour gt
    = return (gdtMinutes gt `quot` 60)
  getMinute gt
    = return (gdtMinutes gt `rem` 60)
  getSecond gt
    = return (gdtMilliSeconds gt `quot` 1000)
  getMilliSecond gt
    = return (gdtMilliSeconds gt `rem` 1000)
  setYear x gt
    = validate $ gt { gdtYear = x }
  setMonth x gt
    = validate $ gt { gdtMonth = x }
  setDay x gt
    = validate $ gt { gdtDay = x }
  setHour x gt
    = validate $ gt { gdtMinutes = x*60 + (gdtMinutes gt `rem` 60) }
  setMinute x gt
    = validate $ gt { gdtMinutes = (gdtMinutes gt `quot` 60)*60 + x }
  setSecond x gt
    = validate $ gt { gdtMilliSeconds = x*1000 + (gdtMilliSeconds gt `rem` 1000) }
  setMilliSecond x gt
    = validate $ gt { gdtMilliSeconds = (gdtMilliSeconds gt `quot` 1000)*1000 + x }


unixEpoch :: GregorianTime
unixEpoch
  = "1970-01-01T00:00:00Z"

toUnixTime :: MonadPlus m => GregorianTime -> m UnixTime
toUnixTime t
  = undefined

-- | Influenced by an ingenious solution from @caf found here:
--   https://stackoverflow.com/questions/1274964/how-to-decompose-unix-time-in-c
fromUnixTime :: MonadPlus m => UnixTime -> m GregorianTime
fromUnixTime (UnixTime i)
  = do yearMarFeb  <- shrinkYearMarFeb 0 9999
       let remainingDays = days - (yearToDays yearMarFeb)
       monthMarFeb <- selectMonthMarFeb remainingDays
       let (yearJanDec, monthJanDec) = if monthMarFeb > 10
                                         then (yearMarFeb + 1, monthMarFeb - 10)
                                         else (yearMarFeb,     monthMarFeb + 2)
       validate $ GregorianTime
               { gdtYear         = fromIntegral yearJanDec
               , gdtMonth        = fromIntegral monthJanDec
               , gdtDay          = fromIntegral $ remainingDays - (367 * monthMarFeb `quot` 12)
               , gdtMinutes      = fromIntegral $ abs i `quot` 60000 `rem` (24*60)
               , gdtMilliSeconds = fromIntegral $ abs i `rem` 60000
               , gdtOffset       = OffsetMinutes 0
               }
  where
    -- days from 0000-01-01
    days 
      = i `quot` (24*60*60*1000) + 719499
    shrinkYearMarFeb lower upper
      -- we found the year satifying the condition
      | lower == upper                      = return lower
      -- just a fail-safe recursion breaker
      | lower > upper                       = mzero
      -- the tested year has more or equally many days than what we are looking for
      -- induction guarantee: unless 'lower == upper' (catched above) it always holds 'mid < upper'
      | days <= yearToDays (mid   + 1) + 30 = shrinkYearMarFeb lower mid
      -- the tested year has less days than what we are looking for
      -- induction guarantee: it always holds 'mid + 1 > lower'
      | days >  yearToDays (mid   + 1) + 30 = shrinkYearMarFeb (mid + 1) upper
      -- should not happen
      | otherwise                           = mzero
      where
        mid = (lower + upper) `quot` 2
    selectMonthMarFeb d
      | d <= 367 *  1 `quot` 12 = return  1
      | d <= 367 *  2 `quot` 12 = return  2
      | d <= 367 *  3 `quot` 12 = return  3
      | d <= 367 *  4 `quot` 12 = return  4
      | d <= 367 *  5 `quot` 12 = return  5
      | d <= 367 *  6 `quot` 12 = return  6
      | d <= 367 *  7 `quot` 12 = return  7
      | d <= 367 *  8 `quot` 12 = return  8
      | d <= 367 *  9 `quot` 12 = return  9
      | d <= 367 * 10 `quot` 12 = return 10
      | d <= 367 * 11 `quot` 12 = return 11
      | otherwise               = return 12


isLeapYear' :: Int -> Bool
isLeapYear' year
  = (year `mod` 4 == 0) && ((year `mod` 400 == 0) || (year `mod` 100 /= 0))

yearToDays :: Int64 -> Int64
yearToDays year
  = (year * 365) + (year `quot` 4) - (year `quot` 100) + (year `quot` 400)
