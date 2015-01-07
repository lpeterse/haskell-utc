module Data.Tempus.Rfc3339
  ( -- * Parsing
    parseRfc3339String, parseRfc3339Text, parseRfc3339LazyText, parseRfc3339ByteString, parseRfc3339LazyByteString
    -- * Rendering
  , renderRfc3339String, renderRfc3339Text, renderRfc3339LazyText, renderRfc3339ByteString, renderRfc3339LazyByteString
    -- * Low-Level
  , rfc3339Parser, rfc3339Builder
  ) where

import Control.Monad

import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Builder    as B
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL

import Data.Tempus.GregorianTime
import Data.Tempus.Rfc3339.Parser
import Data.Tempus.Rfc3339.Builder

-- | All method's results are wrapped in 'Control.Monad.MonadPlus' to reflect possible
--   failure of the computation. To easily obtain a plain value just use the 'Data.Maybe.Maybe'
--   instance and supply a replacement value:
--
-- > fromMaybe "Invalid Date" (renderRfc3339String (UnixTime minBound))
-- > > "Invalid Date"
-- > fromMaybe (UnixTime 0)   (parseRfc3339String "1970-01-32T00:00:00Z")
-- > > "1970-01-01T00:00:00Z"

renderRfc3339ByteString        :: (MonadPlus m, GregorianTime a, LocalOffset a) => a -> m BS.ByteString
renderRfc3339ByteString t
  = renderRfc3339LazyByteString t >>= return . BSL.toStrict

renderRfc3339LazyByteString    :: (MonadPlus m, GregorianTime a, LocalOffset a) => a -> m BSL.ByteString
renderRfc3339LazyByteString t
  = rfc3339Builder t              >>= return . B.toLazyByteString

renderRfc3339Text              :: (MonadPlus m, GregorianTime a, LocalOffset a) => a -> m T.Text
renderRfc3339Text t
  = renderRfc3339ByteString     t >>= return . T.decodeUtf8

renderRfc3339LazyText          :: (MonadPlus m, GregorianTime a, LocalOffset a) => a -> m TL.Text
renderRfc3339LazyText         t
  = renderRfc3339LazyByteString t >>= return . TL.decodeUtf8

renderRfc3339String            :: (MonadPlus m, GregorianTime a, LocalOffset a) => a -> m String
renderRfc3339String t
  = renderRfc3339Text           t >>= return . T.unpack

parseRfc3339ByteString         :: (MonadPlus m, GregorianTime a, LocalOffset a) => BS.ByteString  -> m a
parseRfc3339ByteString s
  = case Atto.parseOnly rfc3339Parser s of
      Right (Just t) -> return t
      Right _        -> mzero
      _              -> mzero

parseRfc3339LazyByteString     :: (MonadPlus m, GregorianTime a, LocalOffset a) => BSL.ByteString -> m a
parseRfc3339LazyByteString s
  = parseRfc3339ByteString      (BSL.toStrict s)

parseRfc3339Text               :: (MonadPlus m, GregorianTime a, LocalOffset a) => T.Text         -> m a
parseRfc3339Text s
  = parseRfc3339ByteString      (T.encodeUtf8 s)

parseRfc3339LazyText           :: (MonadPlus m, GregorianTime a, LocalOffset a) => TL.Text        -> m a
parseRfc3339LazyText s
  = parseRfc3339LazyByteString (TL.encodeUtf8 s)

parseRfc3339String             :: (MonadPlus m, GregorianTime a, LocalOffset a) => String         -> m a
parseRfc3339String s
  = parseRfc3339Text                  (T.pack s)