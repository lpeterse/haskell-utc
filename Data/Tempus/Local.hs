module Data.Tempus.Local
  ( Local (..)
  , unknown
  ) where

import Data.Tempus.Class.HasDate
import Data.Tempus.Class.HasTime
import Data.Tempus.Class.HasEpoch

data Local time
   = Local 
     { utc    :: time
     , offset :: Maybe Rational
     }

instance Eq t => Eq (Local t) where
  (==) (Local a _) (Local b _)
    = a == b

instance Ord t => Ord (Local t) where
  compare (Local a _) (Local b _)
    = compare a b

instance HasEpoch t => HasEpoch (Local t) where
  epoch
    = unknown epoch

instance Functor Local where
  fmap f (Local t o)
    = Local (f t) o

instance HasDate t => HasDate (Local t) where
  year      = undefined
  month     = undefined
  day       = undefined
  setYear   = undefined
  setMonth  = undefined
  setDay    = undefined

instance HasTime t => HasTime (Local t) where
  hour  = undefined
  minute = undefined
  second  = undefined
  secondFraction  = undefined
  setHour = undefined
  setMinute  = undefined
  setSecond   = undefined
  setSecondFraction  = undefined
  midnight = unknown midnight

unknown :: t -> Local t
unknown t
  = Local t Nothing