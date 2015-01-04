module Data.Tempus.GregorianTime.Rfc3339Builder
  ( rfc3339Builder
  ) where

import Data.Monoid
import Data.ByteString.Builder as BS

import Data.Tempus.GregorianTime.Type

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
