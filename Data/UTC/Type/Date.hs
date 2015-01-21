module Data.UTC.Type.Date
  ( Date ()
  ) where

import Data.Ratio

import Data.UTC.Class.Epoch
import Data.UTC.Class.IsDate
import Data.UTC.Class.IsUnixTime
import Data.UTC.Internal

-- | This type represents dates in the __Proleptic Gregorian Calendar__.
--
--   * It can represent any date in the past and in the future by using
--     'Prelude.Integer' internally.
--   * The internal structure is not exposed to avoid the construction of invalid values.
--     Use 'Data.UTC.epoch' or a parser to construct values.
--   * The instance of 'Prelude.Show' is only meant for debugging purposes
--     and is subject to change.
data Date
   = Date
     { dYear           :: Integer
     , dMonth          :: Integer
     , dDay            :: Integer
     } deriving (Eq, Ord)

instance Show Date where
  show (Date yy mm dd)
    = concat
        [ if yy < 0
            then "-"
            else ""
        , if abs yy > 9999
            then show (abs yy)
            else fixedDecimal 4 (abs yy)
        , "-"
        , fixedDecimal 2 mm
        , "-"
        , fixedDecimal 2 dd
        ]

instance Epoch Date where
  epoch
    = Date
      { dYear           = 1970
      , dMonth          = 1
      , dDay            = 1
      }

instance IsUnixTime Date where
  unixSeconds t
    = (days       * secsPerDay    % 1)
    - deltaUnixEpochCommonEpoch
    where
      days = yearMonthDayToDays (year t, month t, day t)
  fromUnixSeconds u
    = return
    $ Date
      { dYear           = y
      , dMonth          = m
      , dDay            = d
      }
    where
      s         = u + deltaUnixEpochCommonEpoch
      (y, m, d) = daysToYearMonthDay (truncate s `div` secsPerDay)

instance IsDate Date where
  year
    = dYear
  month
    = dMonth
  day
    = dDay
  setYear x t
    = if isValidDate (x, month t, day t)
      then return $ t { dYear  = x }
      else fail   $ "Dated.setYear "  ++ show x
  setMonth x t
    = if isValidDate (year t, x, day t)
      then return $ t { dMonth = x }
      else fail   $ "Dated.setMonth " ++ show x
  setDay x t
    = if isValidDate (year t, month t, x)
      then return $ t { dDay   = x }
      else fail   $ "Dated.setDay "   ++ show x

