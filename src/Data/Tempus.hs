module Data.Tempus
  ( -- * Construction
    Tempus ()
  , getTime, setTime, getOffset, setOffset
    -- * Gregorian Calendar
    -- ** Getters
  , getYears, getMonths, getDays, getHours, getMinutes, getSeconds, getMillis
    -- * Modification
    -- ** Adding Intervals
  , addYears, addMonths, addDays, addHours, addMinutes, addSeconds, addMillis
  )
  where

import Data.Int
import Data.Word

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


