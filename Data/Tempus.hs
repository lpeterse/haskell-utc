{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Data.Tempus
  ( -- * Flavors
  -- ** Unix Offset / Unix Time
    UXO.UnixOffset (..)
  , UXT.UnixTime (..)
  -- ** Gregorian Date Time
  , GDT.GregorianDateTime ()
  -- * Tempus Class
  , Data.Tempus.Class.Tempus (..)
  -- * Unix Time Operations
  , UXT.fromUnixOffset, UXT.toUnixOffset
  , UXT.fromUnixTime, UXT.toUnixTime
  --, getTime, setTime, getOffset, setOffset
    -- * Gregorian Calendar Operations
    -- ** Getters
  , getYears, getMonths, getDays, getHours, getMinutes, getSeconds, getMillis
    -- ** Adding Intervals
  , addYears, addMonths, addDays, addHours, addMinutes, addSeconds, addMillis
    -- * Encoding
    -- ** RCF 3999
  , Rfc3999(..)
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Data.Tempus.Class
import qualified Data.Tempus.UnixTime as UXT
import qualified Data.Tempus.UnixOffset as UXO
import qualified Data.Tempus.GregorianDateTime as GDT


-- | > getYears   "2014-⁠12-⁠24T18:11:47Z" == 2014
getYears      :: Integral a => UXT.UnixTime -> a
getYears
  = undefined

-- | > getMonths  "2014-⁠12-⁠24T18:11:47Z" == 12
getMonths     :: Integral a => UXT.UnixTime -> a
getMonths
  = undefined

-- | > getDays    "2014-⁠12-⁠24T18:11:47Z" == 24
getDays       :: Integral a => UXT.UnixTime -> a
getDays
  = undefined

-- | > getHours   "2014-⁠12-⁠24T18:11:47Z" == 18
getHours      :: Integral a => UXT.UnixTime -> a
getHours
  = undefined

-- | > getMinutes "2014-⁠12-⁠24T18:11:47Z" == 11
getMinutes    :: Integral a => UXT.UnixTime -> a
getMinutes
  = undefined

-- | > getSeconds "2014-⁠12-⁠24T18:11:47Z" == 47
getSeconds    :: Integral a => UXT.UnixTime -> a
getSeconds
  = undefined

-- | > getMillis  "2014-⁠12-⁠24T18:11:47Z" == 0
getMillis     :: Integral a => UXT.UnixTime -> a
getMillis
  = undefined



addYears   :: Integral a => a -> UXT.UnixTime -> UXT.UnixTime
addYears
  = undefined

addMonths  :: Integral a => a -> UXT.UnixTime -> UXT.UnixTime
addMonths
  = undefined

addDays    :: Integral a => a -> UXT.UnixTime -> UXT.UnixTime
addDays
  = undefined

addHours   :: Integral a => a -> UXT.UnixTime -> UXT.UnixTime
addHours
  = undefined

addMinutes :: Integral a => a -> UXT.UnixTime -> UXT.UnixTime
addMinutes
  = undefined

addSeconds :: Integral a => a -> UXT.UnixTime -> UXT.UnixTime
addSeconds
  = undefined

addMillis  :: Integral a => a -> UXT.UnixTime -> UXT.UnixTime
addMillis
  = undefined


class Rfc3999 a where
  parseRfc3999  :: a -> Either String UXT.UnixTime
  renderRfc3999 :: UXT.UnixTime -> a

instance Rfc3999 [Char] where
  parseRfc3999 _
    = undefined
  renderRfc3999 _
    = undefined

instance Rfc3999 T.Text where
  parseRfc3999 _
    = undefined
  renderRfc3999 _
    = undefined

instance Rfc3999 TL.Text where
  parseRfc3999 _
    = undefined
  renderRfc3999 _
    = undefined

instance Rfc3999 BS.ByteString where
  parseRfc3999 _
    = undefined
  renderRfc3999 _
    = undefined

instance Rfc3999 BSL.ByteString where
  parseRfc3999 _
    = undefined
  renderRfc3999 _
    = undefined

