{-# LANGUAGE FlexibleInstances #-}
module Data.UTC.Format.Iso8601 where

import Control.Monad.Catch

import Data.Monoid
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL

import Data.UTC.Class.IsDate
import Data.UTC.Class.IsTime
import Data.UTC.Type.Exception

class Iso8601Renderer string where
  -- | __YYYYMMDD__
  renderIso8601CalendarDate  :: (MonadThrow m, IsDate t) => t -> m string
  -- | __YYYY-MM-DD__ (extended format)
  renderIso8601CalendarDate' :: (MonadThrow m, IsDate t) => t -> m string

  -- | __hhmmss__
  renderIso8601TimeHms       :: (MonadThrow m, IsTime t) => t -> m string
  -- | __hh:mm:ss__ (extended format)
  renderIso8601TimeHms'      :: (MonadThrow m, IsTime t) => t -> m string
  -- | __hhmm__
  renderIso8601TimeHm        :: (MonadThrow m, IsTime t) => t -> m string
  -- | __hh:mm__ (extended format)
  renderIso8601TimeHm'       :: (MonadThrow m, IsTime t) => t -> m string

instance Iso8601Renderer BS.ByteString where
  renderIso8601CalendarDate t
    = renderIso8601CalendarDate     t >>= return . BSL.toStrict
  renderIso8601CalendarDate' t
    = renderIso8601CalendarDate'    t >>= return . BSL.toStrict
  renderIso8601TimeHms t
    = renderIso8601TimeHms          t >>= return . BSL.toStrict
  renderIso8601TimeHms' t
    = renderIso8601TimeHms'         t >>= return . BSL.toStrict
  renderIso8601TimeHm t
    = renderIso8601TimeHm           t >>= return . BSL.toStrict
  renderIso8601TimeHm' t
    = renderIso8601TimeHm'          t >>= return . BSL.toStrict

instance Iso8601Renderer T.Text where
  renderIso8601CalendarDate t
    = renderIso8601CalendarDate     t >>= return . T.decodeUtf8
  renderIso8601CalendarDate' t
    = renderIso8601CalendarDate'    t >>= return . T.decodeUtf8
  renderIso8601TimeHms t
    = renderIso8601TimeHms          t >>= return . T.decodeUtf8
  renderIso8601TimeHms' t
    = renderIso8601TimeHms'         t >>= return . T.decodeUtf8
  renderIso8601TimeHm t
    = renderIso8601TimeHm           t >>= return . T.decodeUtf8
  renderIso8601TimeHm' t
    = renderIso8601TimeHm'          t >>= return . T.decodeUtf8

instance Iso8601Renderer TL.Text where
  renderIso8601CalendarDate t
    = renderIso8601CalendarDate     t >>= return . TL.decodeUtf8
  renderIso8601CalendarDate' t
    = renderIso8601CalendarDate'    t >>= return . TL.decodeUtf8
  renderIso8601TimeHms t
    = renderIso8601TimeHms          t >>= return . TL.decodeUtf8
  renderIso8601TimeHms' t
    = renderIso8601TimeHms'         t >>= return . TL.decodeUtf8
  renderIso8601TimeHm t
    = renderIso8601TimeHm           t >>= return . TL.decodeUtf8
  renderIso8601TimeHm' t
    = renderIso8601TimeHm'          t >>= return . TL.decodeUtf8

instance Iso8601Renderer [Char] where
  renderIso8601CalendarDate t 
    = renderIso8601CalendarDate     t >>= return . T.unpack
  renderIso8601CalendarDate' t 
    = renderIso8601CalendarDate'    t >>= return . T.unpack
  renderIso8601TimeHms t
    = renderIso8601TimeHms          t >>= return . T.unpack
  renderIso8601TimeHms' t
    = renderIso8601TimeHms'         t >>= return . T.unpack
  renderIso8601TimeHm t
    = renderIso8601TimeHm           t >>= return . T.unpack
  renderIso8601TimeHm' t
    = renderIso8601TimeHm'          t >>= return . T.unpack

instance Iso8601Renderer BSL.ByteString where
  renderIso8601CalendarDate t
    | 0 <= yyyy && yyyy <= 9999
    = return
    $ BS.toLazyByteString
    $ mconcat
        [ BS.word16HexFixed (y3*16*16*16 + y2*16*16 + y1*16 + y0)
        , BS.word8HexFixed (m1*16 + m0)
        , BS.word8HexFixed (d1*16 + d0)
        ]
    | otherwise
    = throwM $ UtcException $ "Iso8601: renderIso8601CalendarDate (year " ++ show yyyy ++ " out of range 0-9999)"
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

  renderIso8601CalendarDate' t
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
    = throwM $ UtcException $ "Iso8601: renderIso8601CalendarDate (year " ++ show yyyy ++ " out of range 0-9999)"
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

  renderIso8601TimeHms t
    = return
    $ BS.toLazyByteString
    $ mconcat
        [ BS.word8HexFixed (h1*16 + h0)
        , BS.word8HexFixed (m1*16 + m0)
        , BS.word8HexFixed (s1*16 + s0)
        ]
    where
      (h1,h0,m1,m0,s1,s0) = timeDigits t

  renderIso8601TimeHms' t
    = return
    $ BS.toLazyByteString
    $ mconcat
        [ BS.word8HexFixed (h1*16 + h0)
        , BS.char7 '-'
        , BS.word8HexFixed (m1*16 + m0)
        , BS.char7 '-'
        , BS.word8HexFixed (s1*16 + s0)
        ]
    where
      (h1,h0,m1,m0,s1,s0) = timeDigits t

  renderIso8601TimeHm t
    = return
    $ BS.toLazyByteString
    $ mconcat
        [ BS.word8HexFixed (h1*16 + h0)
        , BS.word8HexFixed (m1*16 + m0)
        ]
    where
      (h1,h0,m1,m0,_,_) = timeDigits t

  renderIso8601TimeHm' t
    = return
    $ BS.toLazyByteString
    $ mconcat
        [ BS.word8HexFixed (h1*16 + h0)
        , BS.char7 '-'
        , BS.word8HexFixed (m1*16 + m0)
        ]
    where
      (h1,h0,m1,m0,_,_) = timeDigits t

timeDigits t
  = (h1,h0,m1,m0,s1,s0)
  where
    hh   = hour   t
    mm   = minute t
    ss   = second t
    h1   = fromIntegral $ hh `div` 10   `mod` 10
    h0   = fromIntegral $ hh `div` 1    `mod` 10
    m1   = fromIntegral $ mm `div` 10   `mod` 10
    m0   = fromIntegral $ mm `div` 1    `mod` 10
    s1   = fromIntegral $ ss `div` 10   `mod` 10
    s0   = fromIntegral $ ss `div` 1    `mod` 10

