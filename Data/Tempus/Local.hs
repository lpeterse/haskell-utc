module Data.Tempus.Local
  ( Local (..)
  , unknown
  ) where

import Data.Tempus.Epoch
import Data.Tempus.GregorianTime

data Local t
   = Local 
     { utc    :: t
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
  year  (Local t _) = year t
  month (Local t _) = month t
  day   (Local t _) = day t
  setYear  y t = undefined
  setMonth m t = undefined
  setDay   d t = undefined

instance Timed t => Timed (Local t) where
  hour  (Local t _) = undefined
  minute (Local t _) = undefined
  second   (Local t _) = undefined
  secondFraction   (Local t _) = undefined
  setHour h (Local t _) = undefined
  setMinute m (Local t _) = undefined
  setSecond  s (Local t _) = undefined
  setSecondFraction f (Local t _) = undefined

unknown :: t -> Local t
unknown t
  = Local t Nothing