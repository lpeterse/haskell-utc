{-# LANGUAGE FlexibleInstances #-}
module Data.UTC.Format.Iso8601 where

import Data.Monoid
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL

import Data.UTC.Class.IsDate

class Iso8601Renderer string where
  -- | __YYYYMMDD__
  renderIso8601BasicCalendarDate :: (Monad m, IsDate t) => t -> m string
  -- | __YYYY-MM-DD__
  renderIso8601ExtendedCalendarDate :: (Monad m, IsDate t) => t -> m string

instance Iso8601Renderer BS.ByteString where
  renderIso8601BasicCalendarDate t
    = renderIso8601BasicCalendarDate t     >>= return . BSL.toStrict
  renderIso8601ExtendedCalendarDate t
    = renderIso8601ExtendedCalendarDate t     >>= return . BSL.toStrict

instance Iso8601Renderer T.Text where
  renderIso8601BasicCalendarDate t
    = renderIso8601BasicCalendarDate     t >>= return . T.decodeUtf8
  renderIso8601ExtendedCalendarDate t
    = renderIso8601ExtendedCalendarDate     t >>= return . T.decodeUtf8

instance Iso8601Renderer TL.Text where
  renderIso8601BasicCalendarDate t
    = renderIso8601BasicCalendarDate     t >>= return . TL.decodeUtf8
  renderIso8601ExtendedCalendarDate t
    = renderIso8601ExtendedCalendarDate     t >>= return . TL.decodeUtf8

instance Iso8601Renderer [Char] where
  renderIso8601BasicCalendarDate t 
    = renderIso8601BasicCalendarDate     t >>= return . T.unpack
  renderIso8601ExtendedCalendarDate t 
    = renderIso8601ExtendedCalendarDate     t >>= return . T.unpack

instance Iso8601Renderer BSL.ByteString where
  renderIso8601BasicCalendarDate t
    | 0 <= yyyy && yyyy <= 9999
    = return
    $ BS.toLazyByteString
    $ mconcat
        [ BS.word16HexFixed (y3*16*16*16 + y2*16*16 + y1*16 + y0)
        , BS.word8HexFixed (m1*16 + m0)
        , BS.word8HexFixed (d1*16 + d0)
        ]
    | otherwise
    = fail "Data.UTC.Format.Iso8601.renderIso8601BasicCalendarDate: year out of range"
    where
      yyyy = year  t
      mm   = month t
      dd   = day   t
      y3   = fromIntegral $ yyyy  `div` 1000 `mod` 10
      y2   = fromIntegral $ yyyy  `div` 100  `mod` 10
      y1   = fromIntegral $ yyyy  `div` 10   `mod` 10
      y0   = fromIntegral $ yyyy  `div` 1    `mod` 10
      m1   = fromIntegral $ mm    `div` 10   `mod` 10
      m0   = fromIntegral $ mm    `div` 1    `mod` 10
      d1   = fromIntegral $ dd    `div` 10   `mod` 10
      d0   = fromIntegral $ dd    `div` 1    `mod` 10

  renderIso8601ExtendedCalendarDate t
    | 0 <= yyyy && yyyy <= 9999
    = return
    $ BS.toLazyByteString
    $ mconcat
        [ BS.word16HexFixed (y3*16*16*16 + y2*16*16 + y1*16 + y0)
        , BS.char7 '-'
        , BS.word8HexFixed (m1*16 + m0)
        , BS.char7 '-'
        , BS.word8HexFixed (d1*16 + d0)
        ]
    | otherwise
    = fail "Data.UTC.Format.Iso8601.renderIso8601ExtendedCalendarDate: year out of range"
    where
      yyyy = year  t
      mm   = month t
      dd   = day   t
      y3   = fromIntegral $ yyyy  `div` 1000 `mod` 10
      y2   = fromIntegral $ yyyy  `div` 100  `mod` 10
      y1   = fromIntegral $ yyyy  `div` 10   `mod` 10
      y0   = fromIntegral $ yyyy  `div` 1    `mod` 10
      m1   = fromIntegral $ mm    `div` 10   `mod` 10
      m0   = fromIntegral $ mm    `div` 1    `mod` 10
      d1   = fromIntegral $ dd    `div` 10   `mod` 10
      d0   = fromIntegral $ dd    `div` 1    `mod` 10

