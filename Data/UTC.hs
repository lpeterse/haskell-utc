module Data.UTC
  ( 
  -- * Introduction
  -- ** Quickstart
  -- $quickstart

  -- ** General Concept
  -- $concept

  -- * Interfaces
  -- ** Date
    IsDate(..)
  -- ** Time
  , IsTime (..)
  -- ** Unix Time
  , IsUnixTime(..)
  -- ** Epoch / Midnight
  , Epoch(..)
  , Midnight(..)
    -- ** Getting (Current) Timestamps
  , HasUnixTime (..)
  -- * Generic Date/Time Types
  -- ** Date
  , Date
  -- ** Time
  , Time
  -- ** DateTime
  , DateTime (..)
  -- ** Local Time
  , Local (..)

  -- * Formatting
  -- ** RFC 3339
  -- *** Parsing
  , Rfc3339Parser(..)
  -- *** Rendering
  , Rfc3339Renderer(..)
  ) where

import Data.UTC.Class.Epoch
import Data.UTC.Class.Midnight
import Data.UTC.Class.IsDate
import Data.UTC.Class.IsTime
import Data.UTC.Class.IsUnixTime
import Data.UTC.Class.HasUnixTime
import Data.UTC.Type.Date
import Data.UTC.Type.Time
import Data.UTC.Type.DateTime
import Data.UTC.Type.Local
import Data.UTC.Format.Rfc3339

-- $quickstart
--
-- Use the 'DateTime' type! It supports all functions you find below.
-- Use 'Maybe' for all occurences of 'm'.

-- $concept
-- TODO: describe the library's concept here.