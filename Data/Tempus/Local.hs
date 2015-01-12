module Data.Tempus.Local
  ( Local (..)
  , unknown
  ) where

import Data.Tempus.Epoch

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

unknown :: t -> Local t
unknown t
  = Local t Nothing