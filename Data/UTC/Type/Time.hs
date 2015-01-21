module Data.UTC.Type.Time
  ( Time ()
  , midnight
  ) where

import Data.Ratio

import Data.UTC.Class.Midnight
import Data.UTC.Class.IsTime
import Data.UTC.Class.IsUnixTime
import Data.UTC.Internal

-- | This type represents time instants during a day (__00:00:00 - 23:59:59.999__..)
--   with arbitrary precision (uses 'Prelude.Integer' internally).
--
--   * The internal structure is not exposed to avoid the creation of
--     invalid values.
--     Use 'Data.UTC.midnight' or a parser to construct values.
--   * The instance of 'Prelude.Show' is only meant for debugging purposes
--     and is subject to change.
--
-- > > show midnight :: Time
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

instance Midnight Time where
  midnight
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
    | x < 0 || 23 < x = fail   $ "Time.setHour "           ++ show x
    | otherwise       = return $ t { tHour           = x }
  setMinute x t
    | x < 0 || 59 < x = fail   $ "Time.setMinute "         ++ show x
    | otherwise       = return $ t { tMinute         = x }
  setSecond x t
    | x < 0 || 59 < x = fail   $ "Time.setSecond "         ++ show x
    | otherwise       = return $ t { tSecond         = x }
  setSecondFraction x t
    | x < 0 || 1 <= x = fail   $ "Time.setSecondFraction " ++ show x
    | otherwise       = return $ t { tSecondFraction = x }