{-# LANGUAGE Safe, FlexibleInstances #-}
module Data.UTC.Type.Local
  ( Local (..)
  ) where

import Data.Maybe

import Data.UTC.Class.Epoch
import Data.UTC.Class.IsTime
import Data.UTC.Type.Time

-- | This type is used to extend UTC time types by a local offset in seconds (positive or negative).
--
--   /Beware/: A local offset is not a time zone. It is just a fix
--   period of time. In contrast to a time zone this does not take
--   summer or winter time into account.
data Local time
   = Local 
     { -- | ['Nothing'] The local offset is unknown (behaves like __W__estern __E__uropean __T__ime)
       --
       -- ['Just' 0] UTC+00:00 (__W__estern __E__uropean __T__ime)
       --
       -- ['Just' 3600] UTC+01:00 (__C__entral __E__uropean __T__ime)
       offset :: Maybe Rational
       -- | The time to be interpreted as UTC+00:00 (__W__estern __E__uropean __T__ime)
     , utc    :: time
     }

instance Eq t => Eq (Local t) where
  (==) (Local _ a) (Local _ b)
    = a == b

instance Ord t => Ord (Local t) where
  compare (Local _ a) (Local _ b)
    = compare a b

instance Epoch t => Epoch (Local t) where
  epoch
    = Local Nothing epoch

instance Functor Local where
  fmap f (Local o t)
    = Local o (f t)

instance Bounded t => Bounded (Local t) where
  minBound  = Local Nothing minBound
  maxBound  = Local Nothing maxBound

-- assumption: addSecondFractions for Time is always successful
instance IsTime (Local Time) where
  hour (Local Nothing t)
    = hour t
  hour (Local (Just 0) t)
    = hour t
  hour (Local (Just o) t)
    = hour
    $ fromMaybe undefined $ addSecondFractions o t
  minute (Local Nothing t)
    = minute t
  minute (Local (Just 0) t)
    = minute t
  minute (Local (Just o) t)
    = minute
    $ fromMaybe undefined $ addSecondFractions o t
  second (Local Nothing t)
    = second t
  second (Local (Just 0) t)
    = second t
  second  (Local (Just o) t)
    = second
    $ fromMaybe undefined $ addSecondFractions o t
  secondFraction (Local Nothing t)
    = secondFraction t
  secondFraction (Local (Just 0) t)
    = secondFraction t
  secondFraction (Local (Just o) t)
    = secondFraction
    $ fromMaybe undefined $ addSecondFractions o t
  setHour h (Local o@Nothing t)
    = setHour h t >>= return . Local o
  setHour h (Local o@(Just 0) t)
    = setHour h t >>= return . Local o
  setHour h (Local o@(Just i) t)
    = addSecondFractions i t >>= setHour h >>= addSecondFractions (negate i) >>= return . Local o
  setMinute h (Local o@Nothing t)
    = setMinute h t >>= return . Local o
  setMinute h (Local o@(Just 0) t)
    = setMinute h t >>= return . Local o
  setMinute h (Local o@(Just i) t)
    = addSecondFractions i t >>= setMinute h >>= addSecondFractions (negate i) >>= return . Local o
  setSecond h (Local o@Nothing t)
    = setSecond h t >>= return . Local o
  setSecond h (Local o@(Just 0) t)
    = setSecond h t >>= return . Local o
  setSecond h (Local o@(Just i) t)
    = addSecondFractions i t >>= setSecond h >>= addSecondFractions (negate i) >>= return . Local o
  setSecondFraction h (Local o@Nothing t)
    = setSecondFraction h t >>= return . Local o
  setSecondFraction h (Local o@(Just 0) t)
    = setSecondFraction h t >>= return . Local o
  setSecondFraction h (Local o@(Just i) t)
    = addSecondFractions i t >>= setSecondFraction h >>= addSecondFractions (negate i) >>= return . Local o
