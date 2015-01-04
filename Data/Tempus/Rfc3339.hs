module Data.Tempus.Rfc3339 where

import Control.Monad

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL

-- | All method's results are wrapped in 'Control.Monad.MonadPlus' to reflect possible
--   failure of the computation. To easily obtain a plain value just use the 'Data.Maybe.Maybe'
--   instance and supply a replacement value:
--
-- > fromMaybe "Invalid Date" (renderRfc3339String (UnixTime minBound))
-- > > "Invalid Date"
-- > fromMaybe (UnixTime 0)   (parseRfc3339String "1970-01-32T00:00:00Z")
-- > > "1970-01-01T00:00:00Z"
class Rfc3339 a where
  renderRfc3339ByteString        :: (MonadPlus m) => a -> m BS.ByteString
  renderRfc3339LazyByteString    :: (MonadPlus m) => a -> m BSL.ByteString
  renderRfc3339LazyByteString   t = renderRfc3339ByteString     t >>= return . BSL.fromStrict
  renderRfc3339Text              :: (MonadPlus m) => a -> m T.Text
  renderRfc3339Text             t = renderRfc3339ByteString     t >>= return . T.decodeUtf8
  renderRfc3339LazyText          :: (MonadPlus m) => a -> m TL.Text
  renderRfc3339LazyText         t = renderRfc3339LazyByteString t >>= return . TL.decodeUtf8
  renderRfc3339String            :: (MonadPlus m) => a -> m String
  renderRfc3339String           t = renderRfc3339Text           t >>= return . T.unpack
  parseRfc3339ByteString         :: (MonadPlus m) => BS.ByteString  -> m a
  parseRfc3339LazyByteString     :: (MonadPlus m) => BSL.ByteString -> m a
  parseRfc3339LazyByteString    s = parseRfc3339ByteString      (BSL.toStrict s)
  parseRfc3339Text               :: (MonadPlus m) => T.Text         -> m a
  parseRfc3339Text              s = parseRfc3339ByteString      (T.encodeUtf8 s)
  parseRfc3339LazyText           :: (MonadPlus m) => TL.Text        -> m a
  parseRfc3339LazyText          s = parseRfc3339LazyByteString (TL.encodeUtf8 s)
  parseRfc3339String             :: (MonadPlus m) => String         -> m a
  parseRfc3339String            s = parseRfc3339Text                  (T.pack s)