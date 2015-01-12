module Data.Tempus.Local
  ( Local (..)
  , unknown
  ) where

import Data.Tempus.Class.Epoch
import Data.Tempus.Class.Midnight
import Data.Tempus.Class.IsDate
import Data.Tempus.Class.IsTime

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

instance Epoch t => Epoch (Local t) where
  epoch
    = unknown epoch

instance Midnight t => Midnight (Local t) where
  midnight
    = unknown midnight

instance Functor Local where
  fmap f (Local t o)
    = Local (f t) o

instance Bounded t => Bounded (Local t) where
  minBound  = unknown minBound
  maxBound  = unknown maxBound

instance IsDate t => IsDate (Local t) where
  year      = undefined
  month     = undefined
  day       = undefined
  setYear   = undefined
  setMonth  = undefined
  setDay    = undefined

instance IsTime t => IsTime (Local t) where
  hour  = undefined
  minute = undefined
  second  = undefined
  secondFraction  = undefined
  setHour = undefined
  setMinute  = undefined
  setSecond   = undefined
  setSecondFraction  = undefined

unknown :: t -> Local t
unknown t
  = Local t Nothing