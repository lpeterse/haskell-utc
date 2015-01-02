{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Data.Tempus
  ( -- * Flavors
  -- ** Unix Time
    UXT.UnixTime (..)
  -- ** Gregorian Calendar Time
  , GDT.GregorianTime ()
  -- * Tempus Class
  , Data.Tempus.Class.Tempus (..)

  ) where

import Control.Monad

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Data.Tempus.Class
import qualified Data.Tempus.UnixTime as UXT
import qualified Data.Tempus.GregorianTime as GDT


