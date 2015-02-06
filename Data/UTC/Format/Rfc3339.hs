{-# LANGUAGE FlexibleInstances #-}
module Data.UTC.Format.Rfc3339
  ( -- * Parsing
    Rfc3339Parser(..)
    -- * Rendering
  , Rfc3339Renderer(..)
    -- * Low-Level
  , rfc3339Parser, rfc3339Builder
  ) where

import Control.Monad.Catch

import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Builder    as B
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL

import Data.UTC.Class.Epoch
import Data.UTC.Class.IsDate
import Data.UTC.Class.IsTime
import Data.UTC.Type.Local
import Data.UTC.Type.Exception
import Data.UTC.Format.Rfc3339.Parser
import Data.UTC.Format.Rfc3339.Builder

class Rfc3339Parser a where
  parseRfc3339 :: (MonadThrow m, IsDate t, IsTime t, Epoch t) => a -> m (Local t)

instance Rfc3339Parser BS.ByteString where
  parseRfc3339 s
    = case Atto.parseOnly rfc3339Parser s of
        Right t -> t
        Left  _ -> throwM $ UtcException $ "Rfc3339Parser: parseRfc3339 " ++ show s

instance Rfc3339Parser BSL.ByteString where
  parseRfc3339 s
    = parseRfc3339 (BSL.toStrict s)

instance Rfc3339Parser T.Text where
  parseRfc3339 s
    = parseRfc3339 (T.encodeUtf8 s)

instance Rfc3339Parser TL.Text where
  parseRfc3339 s
    = parseRfc3339 (TL.encodeUtf8 s)

instance Rfc3339Parser [Char] where
  parseRfc3339 s
    = parseRfc3339 (T.pack s)

class Rfc3339Renderer string where
  renderRfc3339 :: (Monad m, IsDate t, IsTime t, Epoch t) => Local t -> m string

instance Rfc3339Renderer BS.ByteString where
  renderRfc3339 t
    = renderRfc3339 t     >>= return . BSL.toStrict

instance Rfc3339Renderer BSL.ByteString where
  renderRfc3339 t
    = rfc3339Builder t    >>= return . B.toLazyByteString

instance Rfc3339Renderer T.Text where
  renderRfc3339 t
    = renderRfc3339     t >>= return . T.decodeUtf8

instance Rfc3339Renderer TL.Text where
  renderRfc3339 t
    = renderRfc3339     t >>= return . TL.decodeUtf8

instance Rfc3339Renderer [Char] where
  renderRfc3339 t 
    = renderRfc3339     t >>= return . T.unpack
