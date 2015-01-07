module Data.Tempus.GregorianTime
  ( -- * Type
    GregorianTime()
  -- * Creation
  , fromUnixTime
  ) where

import Data.String
import Data.Maybe

import Data.Tempus.GregorianCalendar
import Data.Tempus.GregorianTime.Type
import Data.Tempus.GregorianTime.FromUnixTime
import Data.Tempus.UnixTime.Type
import Data.Tempus.Rfc3339

instance Show GregorianTime where
  -- The assumption is that every GregorianTime is valid and renderable as Rfc3339 string
  -- and rendering failure is impossible.
  show = fromMaybe "0000-00-00T00:00:00Z" . renderRfc3339String

instance IsString GregorianTime where
  fromString = fromMaybe commonEpoch . parseRfc3339String

instance GregorianCalendar GregorianTime where
  commonEpoch
    = GregorianTime
      { gdtYear         = 0
      , gdtMonth        = 1
      , gdtDay          = 1
      , gdtMinutes      = 0
      , gdtMilliSeconds = 0
      , gdtOffset       = OffsetMinutes 0
      }
  getYear gt
    = return (gdtYear gt)
  getMonth gt
    = return (gdtMonth gt)
  getDay gt
    = return (gdtDay gt)
  getHour gt
    = return (gdtMinutes gt `div` 60)
  getMinute gt
    = return (gdtMinutes gt `mod` 60)
  getSecond gt
    = return (gdtMilliSeconds gt `div` 1000)
  getMilliSecond gt
    = return (gdtMilliSeconds gt `mod` 1000)
  getLocalOffset gt
    = return $ case gdtOffset gt of
        OffsetUnknown  -> Nothing
        OffsetMinutes m -> Just m
  setYear x gt
    = validate $ gt { gdtYear         = x }
  setMonth x gt
    = validate $ gt { gdtMonth        = x }
  setDay x gt
    = validate $ gt { gdtDay          = x }
  setHour x gt
    = validate $ gt { gdtMinutes      = (x * 60) + (gdtMinutes gt `mod` 60) }
  setMinute x gt
    = validate $ gt { gdtMinutes      = (gdtMinutes gt `div` 60) * 60 + x }
  setSecond x gt
    = validate $ gt { gdtMilliSeconds = (x * 1000) + (gdtMilliSeconds gt `mod` 1000) }
  setMilliSecond x gt
    = validate $ gt { gdtMilliSeconds = (gdtMilliSeconds gt `div` 1000) * 1000 + x }
  setLocalOffset mm gt
    = validate $ case mm of
       Nothing -> gt { gdtOffset = OffsetUnknown }
       Just o  -> gt { gdtOffset = OffsetMinutes o }
