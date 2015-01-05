module Data.Tempus.Epoch where

class UnixEpoch a where
  -- | > fromRfc3339String "1970-01-01T00:00:00Z" == Just unixEpoch
  unixEpoch   :: a

class CommonEpoch a where
  -- | > fromRfc3339String "0000-01-01T00:00:00Z" == Just commonEpoch
  commonEpoch :: a