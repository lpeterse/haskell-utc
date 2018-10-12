{-# LANGUAGE Safe #-}
module Data.UTC.Type.Time
  ( Time ()
  ) where

import Control.Monad.Catch

import Data.Ratio

import Data.UTC.Class.Epoch
import Data.UTC.Class.IsTime
import Data.UTC.Class.IsUnixTime
import Data.UTC.Internal
import Data.UTC.Type.Exception

-- | This type represents time instants during a day (__00:00:00 - 23:59:59.999__..)
--   with arbitrary precision (uses 'Prelude.Integer' internally).
--
--   * The internal structure is not exposed to avoid the creation of
--     invalid values.
--     Use 'Data.UTC.epoch' or a parser to construct values.
--   * The instance of 'Prelude.Show' is only meant for debugging purposes
--     and is subject to change.
--
-- > > show (epoch :: Time)
-- > 00:00:00
data Time
   = Time
     { tHour           :: Integer
     , tMinute         :: Integer
     , tSecond         :: Integer
     , tSecondFraction :: Rational
     } deriving (Eq, Ord)

instance Show Time where
  show (Time hh mm ss ff)
    = concat
        [ fixedDecimal 2 hh
        , ":"
        , fixedDecimal 2 mm
        , ":"
        , fixedDecimal 2 ss
        , decimalFraction 12 ff
        ]

instance Epoch Time where
  epoch
    = Time 0 0 0 0

instance IsUnixTime Time where
  unixSeconds t
    = (hour t     * secsPerHour   % 1)
    + (minute t   * secsPerMinute % 1)
    + (second t                   % 1)
    + (secondFraction t)
  fromUnixSeconds s
    = return $ Time
               { tHour           = truncate s `div` secsPerHour   `mod` hoursPerDay
               , tMinute         = truncate s `div` secsPerMinute `mod` minsPerHour
               , tSecond         = truncate s                     `mod` secsPerMinute
               , tSecondFraction = s - (truncate s % 1)
               }

instance IsTime Time where
  hour
    = tHour
  minute
    = tMinute
  second
    = tSecond
  secondFraction
    = tSecondFraction

  setHour x t
    | x < 0 || 23 < x = throwM $ UtcException $ "Time: setHour "           ++ show x ++ " " ++ show t
    | otherwise       = return $ t { tHour           = x }
  setMinute x t
    | x < 0 || 59 < x = throwM $ UtcException $ "Time: setMinute "         ++ show x ++ " " ++ show t
    | otherwise       = return $ t { tMinute         = x }
  setSecond x t
    | x < 0 || 59 < x = throwM $ UtcException $ "Time: setSecond "         ++ show x ++ " " ++ show t
    | otherwise       = return $ t { tSecond         = x }
  setSecondFraction x t
    | x < 0 || 1 <= x = throwM $ UtcException $ "Time: setSecondFraction " ++ show x ++ " " ++ show t
    | otherwise       = return $ t { tSecondFraction = x }
