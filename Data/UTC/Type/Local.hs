{-# LANGUAGE Safe, FlexibleInstances #-}
module Data.UTC.Type.Local
  ( Local (..)
  , unknown
  ) where

import Data.Maybe

import Data.UTC.Class.Epoch
import Data.UTC.Class.Midnight
import Data.UTC.Class.IsTime
import Data.UTC.Type.Time

-- | This type is used to extend UTC time types by a local offset.
--
--   /Beware/: A local offset is not a time zone. It is just a fix
--   period of time. In contrast to a time zone this does not take
--   summer or winter time into account.
data Local time
   = Local 
     { -- | The time to be interpreted as UTC+00:00 (__W__estern __E__uropean __T__ime)
       utc    :: time
     , -- | ['Nothing'] The local offset is unknown (behaves like __W__estern __E__uropean __T__ime)
       --
       -- ['Just' 0] UTC+00:00 (__W__estern __E__uropean __T__ime)
       --
       -- ['Just' 3600] UTC+01:00 (__C__entral __E__uropean __T__ime)
       offset :: Maybe Rational
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

-- assumption: addSecondFractions for Time is always successful
instance IsTime (Local Time) where
  hour (Local t Nothing)
    = hour t
  hour (Local t (Just 0))
    = hour t
  hour (Local t (Just o))
    = hour
    $ fromMaybe undefined $ addSecondFractions o t
  minute (Local t Nothing)
    = minute t
  minute (Local t (Just 0))
    = minute t
  minute (Local t (Just o))
    = minute
    $ fromMaybe undefined $ addSecondFractions o t
  second (Local t Nothing)
    = second t
  second (Local t (Just 0))
    = second t
  second  (Local t (Just o))
    = second
    $ fromMaybe undefined $ addSecondFractions o t
  secondFraction (Local t Nothing)
    = secondFraction t
  secondFraction (Local t (Just 0))
    = secondFraction t
  secondFraction (Local t (Just o))
    = secondFraction
    $ fromMaybe undefined $ addSecondFractions o t
  setHour h (Local t o@Nothing)
    = do t' <- setHour h t
         return (Local t' o)
  setHour h (Local t o@(Just 0))
    = do t' <- setHour h t
         return (Local t' o)
  setHour h (Local t o@(Just i))
    = do t' <- addSecondFractions i t >>= setHour h >>= addSecondFractions (negate i)
         return (Local t' o)
  setMinute h (Local t o@Nothing)
    = do t' <- setMinute h t
         return (Local t' o)
  setMinute h (Local t o@(Just 0))
    = do t' <- setMinute h t
         return (Local t' o)
  setMinute h (Local t o@(Just i))
    = do t' <- addSecondFractions i t >>= setMinute h >>= addSecondFractions (negate i)
         return (Local t' o)
  setSecond h (Local t o@Nothing)
    = do t' <- setSecond h t
         return (Local t' o)
  setSecond h (Local t o@(Just 0))
    = do t' <- setSecond h t
         return (Local t' o)
  setSecond h (Local t o@(Just i))
    = do t' <- addSecondFractions i t >>= setSecond h >>= addSecondFractions (negate i)
         return (Local t' o)
  setSecondFraction h (Local t o@Nothing)
    = do t' <- setSecondFraction h t
         return (Local t' o)
  setSecondFraction h (Local t o@(Just 0))
    = do t' <- setSecondFraction h t
         return (Local t' o)
  setSecondFraction h (Local t o@(Just i))
    = do t' <- addSecondFractions i t >>= setSecondFraction h >>= addSecondFractions (negate i)
         return (Local t' o)



unknown :: t -> Local t
unknown t
  = Local t Nothing