{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Data.Tempus
  ( -- * Flavors
  -- ** Unix Time
    UXT.UnixTime (..)
  -- ** Gregorian Calendar Time
  , GDT.GregorianTime ()
  -- * Tempus Class
  , Data.Tempus.Class.Tempus (..)
    -- * Gregorian Calendar Operations
  , TempusMonad(..)
  , modifyTime
  ) where

import Control.Monad

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Data.Tempus.Class
import qualified Data.Tempus.UnixTime as UXT
import qualified Data.Tempus.GregorianTime as GDT



modifyTime :: (Tempus t, TempusMonad m, MonadPlus n) => t -> m () -> n t
modifyTime
  = undefined

class TempusMonad m where
  -- | > getYears   "2014-⁠12-⁠24T18:11:47Z" == 2014
  getYears      :: m Int
  -- | > getMonths  "2014-⁠12-⁠24T18:11:47Z" == 12
  getMonths     :: m Int
  -- | > getDays    "2014-⁠12-⁠24T18:11:47Z" == 24
  getDays       :: m Int
  -- | > getHours   "2014-⁠12-⁠24T18:11:47Z" == 18
  getHours      :: m Int
  -- | > getMinutes "2014-⁠12-⁠24T18:11:47Z" == 11
  getMinutes    :: m Int
  -- | > getSeconds "2014-⁠12-⁠24T18:11:47Z" == 47
  getSeconds    :: m Int
  -- | > getMillis  "2014-⁠12-⁠24T18:11:47Z" == 0
  getMilliSeconds :: m Int
  setYears      :: Int -> m ()
  setMonths     :: Int -> m ()
  setDays       :: Int -> m ()
  setHours      :: Int -> m ()
  setMinutes    :: Int -> m ()
  setSeconds    :: Int -> m ()
  setMilliSeconds :: Int -> m ()
  addYears      :: Int -> m ()
  addMonths     :: Int -> m ()
  addDays       :: Int -> m ()
  addHours      :: Int -> m ()
  addMinutes    :: Int -> m ()
  addSeconds    :: Int -> m ()
  addMillis     :: Int -> m ()
