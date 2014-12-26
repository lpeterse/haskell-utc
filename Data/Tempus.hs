{-# LANGUAGE FlexibleInstances #-}
module Data.Tempus
  ( -- * Construction/Export
    Tempus ()
  -- ** Unix Time
  , epoch
  , fromUnixTime, toUnixTime
  --, getTime, setTime, getOffset, setOffset
    -- * Gregorian Calendar
    -- ** Getters
  , getYears, getMonths, getDays, getHours, getMinutes, getSeconds, getMillis
    -- ** Adding Intervals
  , addYears, addMonths, addDays, addHours, addMinutes, addSeconds, addMillis
    -- * Encoding
    -- ** RCF 3999
  , Rfc3999(..)
  )
  where

import Data.Int
import Data.Word

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

data Tempus
   = Tempus
     { time   :: Word64
     , offset :: Int16
     }

getTime :: Tempus -> Word64
getTime
  = undefined

setTime :: Word64 -> Tempus -> Tempus
setTime
  = undefined

getOffset :: Integral a => Tempus -> a
getOffset
  = undefined

setOffset :: Integral a => a -> Tempus -> Tempus
setOffset
  = undefined

-- | > epoch == "1970-01-01T00:00:00Z"
epoch :: Tempus
epoch
  = Tempus 0 0

instance Show Tempus where
  show _ = "1970-undefined-FIXME"

fromUnixTime :: Word64 -> Tempus
fromUnixTime w64
  = setTime w64 epoch

toUnixTime :: Tempus -> Word64
toUnixTime t
  = getTime t

-- | > getYears   "2014-⁠12-⁠24T18:11:47Z" == 2014
getYears      :: Integral a => Tempus -> a
getYears
  = undefined

-- | > getMonths  "2014-⁠12-⁠24T18:11:47Z" == 12
getMonths     :: Integral a => Tempus -> a
getMonths
  = undefined

-- | > getDays    "2014-⁠12-⁠24T18:11:47Z" == 24
getDays       :: Integral a => Tempus -> a
getDays
  = undefined

-- | > getHours   "2014-⁠12-⁠24T18:11:47Z" == 18
getHours      :: Integral a => Tempus -> a
getHours
  = undefined

-- | > getMinutes "2014-⁠12-⁠24T18:11:47Z" == 11
getMinutes    :: Integral a => Tempus -> a
getMinutes
  = undefined

-- | > getSeconds "2014-⁠12-⁠24T18:11:47Z" == 47
getSeconds    :: Integral a => Tempus -> a
getSeconds
  = undefined

-- | > getMillis  "2014-⁠12-⁠24T18:11:47Z" == 0
getMillis     :: Integral a => Tempus -> a
getMillis
  = undefined



addYears   :: Integral a => a -> Tempus -> Tempus
addYears
  = undefined

addMonths  :: Integral a => a -> Tempus -> Tempus
addMonths
  = undefined

addDays    :: Integral a => a -> Tempus -> Tempus
addDays
  = undefined

addHours   :: Integral a => a -> Tempus -> Tempus
addHours
  = undefined

addMinutes :: Integral a => a -> Tempus -> Tempus
addMinutes
  = undefined

addSeconds :: Integral a => a -> Tempus -> Tempus
addSeconds
  = undefined

addMillis  :: Integral a => a -> Tempus -> Tempus
addMillis
  = undefined


class Rfc3999 a where
  parseRfc3999  :: a -> Either String Tempus
  renderRfc3999 :: Tempus -> a

instance Rfc3999 [Char] where
  parseRfc3999 s
    = undefined
  renderRfc3999 t
    = undefined

instance Rfc3999 T.Text where
  parseRfc3999 s
    = undefined
  renderRfc3999 t
    = undefined

instance Rfc3999 TL.Text where
  parseRfc3999 s
    = undefined
  renderRfc3999 t
    = undefined

instance Rfc3999 BS.ByteString where
  parseRfc3999 s
    = undefined
  renderRfc3999 t
    = undefined

instance Rfc3999 BSL.ByteString where
  parseRfc3999 s
    = undefined
  renderRfc3999 t
    = undefined