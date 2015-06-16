{-# LANGUAGE Safe #-}
module Data.UTC.Format.Rfc3339.Builder
  ( rfc3339Builder
  ) where

import Data.Monoid
import Data.ByteString.Builder as BS

import Data.UTC.Type.Local
import Data.UTC.Class.IsDate
import Data.UTC.Class.IsTime

rfc3339Builder :: (Monad m, IsDate t, IsTime t) => Local t -> m BS.Builder
rfc3339Builder (Local os t)
  = do -- calculate the single digits
       let y3 = fromIntegral $ year t   `div` 1000 `mod` 10
           y2 = fromIntegral $ year t   `div` 100  `mod` 10
           y1 = fromIntegral $ year t   `div` 10   `mod` 10
           y0 = fromIntegral $ year t   `div` 1    `mod` 10
           m1 = fromIntegral $ month t  `div` 10   `mod` 10
           m0 = fromIntegral $ month t  `div` 1    `mod` 10
           d1 = fromIntegral $ day t    `div` 10   `mod` 10
           d0 = fromIntegral $ day t    `div` 1    `mod` 10
           h1 = fromIntegral $ hour t   `div` 10   `mod` 10
           h0 = fromIntegral $ hour t   `div` 1    `mod` 10
           n1 = fromIntegral $ minute t `div` 10   `mod` 10
           n0 = fromIntegral $ minute t `div` 1    `mod` 10
           s1 = fromIntegral $ second t `div` 10   `mod` 10
           s0 = fromIntegral $ second t `div` 1    `mod` 10
           f2 = truncate (secondFraction t * 10) `mod` 10
           f1 = truncate (secondFraction t * 100) `mod` 10
           f0 = truncate (secondFraction t * 1000) `mod` 10
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
        , case os of
            Nothing -> BS.string7 "-00:00"
            Just 0  -> BS.char7 'Z'
            Just o  -> let oh1 = fromIntegral $ abs (truncate o `quot` 600          `rem` 10 :: Integer)
                           oh0 = fromIntegral $ abs (truncate o `quot` 60           `rem` 10 :: Integer)
                           om1 = fromIntegral $ abs (truncate o `rem`  60 `quot` 10 `rem` 10 :: Integer)
                           om0 = fromIntegral $ abs (truncate o `rem`  60           `rem` 10 :: Integer)
                       in  mconcat
                             [ if o < 0
                                 then BS.char7 '-'
                                 else BS.char7 '+'
                             , BS.word8HexFixed (oh1*16 + oh0)
                             , BS.char7 ':'
                             , BS.word8HexFixed (om1*16 + om0)
                             ]
        ]
