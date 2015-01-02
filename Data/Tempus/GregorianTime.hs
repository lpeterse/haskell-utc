module Data.Tempus.GregorianTime
  ( -- * Type
    GregorianTime()
    -- * RFC 3339
    -- ** Rendering
  , toRfc3339String
  , toRfc3339Text
  , toRfc3339LazyText
  , toRfc3339ByteString
  , toRfc3339LazyByteString
    -- ** Low-Level
  , rfc3339Parser
  , rfc3339Builder
  ) where

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
import Data.Tempus.GregorianTime.Internal

rfc3339Builder :: GregorianTime -> BS.Builder
rfc3339Builder InvalidTime
  = BS.string7 "InvalidTime"
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
       return GregorianTime
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
    timeSecfrac :: Parser Int
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

toRfc3339LazyByteString :: GregorianTime -> BSL.ByteString
toRfc3339LazyByteString gdt
  = BS.toLazyByteString (rfc3339Builder gdt)

toRfc3339ByteString :: GregorianTime -> BS.ByteString
toRfc3339ByteString gdt
  = BSL.toStrict (toRfc3339LazyByteString gdt)

toRfc3339Text :: GregorianTime -> T.Text
toRfc3339Text gdt
  = T.decodeUtf8 (toRfc3339ByteString gdt)

toRfc3339LazyText :: GregorianTime -> TL.Text
toRfc3339LazyText gdt
  = TL.decodeUtf8 (toRfc3339LazyByteString gdt)

toRfc3339String :: GregorianTime -> String
toRfc3339String gdt
  = T.unpack (toRfc3339Text gdt)

instance Show GregorianTime where
  show = toRfc3339String

instance IsString GregorianTime where
  fromString s
    = case parseOnly rfc3339Parser (T.encodeUtf8 $ T.pack s) of
        Right s -> s
        Left  e -> InvalidTime

instance Tempus GregorianTime where
  toGregorianTime
    = id
  fromGregorianTime
    = id
  isInvalid _
    = False -- FIXME