module Data.Tempus.Rfc3339.Builder
  ( rfc3339Builder
  ) where

import Control.Monad

import Data.Monoid
import Data.ByteString.Builder as BS

import Data.Tempus.Class

rfc3339Builder :: (MonadPlus m, Tempus t) => t -> m BS.Builder
rfc3339Builder t
  = do year    <- getYear t
       month   <- getMonth t
       day     <- getDay t
       hour    <- getHour t
       minute  <- getMinute t
       second  <- getSecond t
       msecond <- getMilliSecond t
       offset  <- getLocalOffset t
       -- calculate the single digits
       let y3 = fromIntegral $ year    `div` 1000 `mod` 10
           y2 = fromIntegral $ year    `div` 100  `mod` 10
           y1 = fromIntegral $ year    `div` 10   `mod` 10
           y0 = fromIntegral $ year    `div` 1    `mod` 10
           m1 = fromIntegral $ month   `div` 10   `mod` 10
           m0 = fromIntegral $ month   `div` 1    `mod` 10
           d1 = fromIntegral $ day     `div` 10   `mod` 10
           d0 = fromIntegral $ day     `div` 1    `mod` 10
           h1 = fromIntegral $ hour    `div` 10   `mod` 10
           h0 = fromIntegral $ hour    `div` 1    `mod` 10
           n1 = fromIntegral $ minute  `div` 10   `mod` 10
           n0 = fromIntegral $ minute  `div` 1    `mod` 10
           s1 = fromIntegral $ second  `div` 10   `mod` 10
           s0 = fromIntegral $ second  `div` 1    `mod` 10
           f2 = fromIntegral $ msecond `div` 100  `mod` 10
           f1 = fromIntegral $ msecond `div` 10   `mod` 10
           f0 = fromIntegral $ msecond `div` 1    `mod` 10
       return $ mconcat
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
        , case offset of
            Nothing -> BS.string7 "-00:00"
            Just 0  -> BS.char7 'Z'
            Just o  -> let oh1 = fromIntegral $ abs o `quot` 600          `rem` 10
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
