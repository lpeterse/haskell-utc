module Data.Tempus.Local
  ( Local (..)
  , unknown
  ) where

import Data.Tempus.Epoch
import Data.Tempus.GregorianTime

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

instance Functor Local where
  fmap f (Local t o)
    = Local (f t) o

instance Dated t => Dated (Local t) where
  year      = undefined
  month     = undefined
  day       = undefined
  setYear   = undefined
  setMonth  = undefined
  setDay    = undefined

instance Timed t => Timed (Local t) where
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