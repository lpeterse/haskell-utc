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

import Data.Tempus.Class
import qualified Data.Tempus.UnixTime as UXT
import qualified Data.Tempus.GregorianTime as GDT