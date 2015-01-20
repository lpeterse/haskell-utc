module Data.UTC
  ( 
  -- * Introduction
  -- ** Quick Start
  -- $quickstart

  -- ** General Concepts

  -- *** Handling Failure
  -- $failure

  -- *** Integer vs. Int
  -- $integerint

  -- *** Leap Seconds
  -- $leapseconds

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
--
-- > (parseRfc3339 "2014-12-24T13:37:00Z" :: Maybe (Local DateTime)) >>= addHours 25 >>= setMonth 1 >>= renderRfc3339
-- > > Just "2014-01-25T14:37:00Z"

-- $failure
--
-- The library's main idea is to make it hard to use it wrong. It should
-- be impossible by the API's design to construct invalid date or time values.
--
-- Furthermore, the library is safe in the sense that its functions don't do
-- anything that is not visible in the functions signature. Escpecially, none of the
-- functions throw exceptions via 'Prelude.error' or 'Prelude.undefined'.
--
-- Whenever a function cannot be total, its result is wrapped in a type variable with
-- a 'Prelude.Monad' restriction on it. You can always just use 'Prelude.Maybe' and
-- 'Data.Maybe.fromMaybe' to obtain a plain value:
--
-- >  fromMaybe epoch (addDays 24 epoch) :: Date
-- > > 1970-01-25
--
-- Using another 'Prelude.Monad' instance might give you additional information in case of failure:
--
-- > setHour 10 midnight >>= setMinute 61 :: IO Time
-- > > *** Exception: user error (Time.setMinute 61)

-- $integerint
--
-- This library uses 'Prelude.Integer' instead of 'Prelude.Int'. This bears the advantage
-- of easier reasoning about the code's correctness and the ability
-- to work on date and time with arbitrary range and precision.
--
-- One might think that 'Prelude.Integer' is slower, but indeed it uses 'Prelude.Int'
-- internally unless its range is exceeded. The dispatching should not take more than a
-- few cycles. Using 'Prelude.Int' in the first place can rightly be considered
-- /premature optimisation/. If this really is your application's bottle neck
-- you should first consider creating your own time type (i.e. a newtyped 'Prelude.Int'
-- representing Unixtime) and making it an instance of the relevant classes.
-- Do the critical operations on the bare type. If that is not an option consider
-- using a more specialised library.

-- $leapseconds
--
-- As most other date and time libraries this library does __not support__ handling of leap seconds.
--
-- The problem is not so much that this task would be tedious and difficult, but
-- rather that future leap seconds are not known in advance and are announced
-- just a few weeks before they occur.
-- How should a library deal with this? Changing the function's semantic from version to version whenever
-- a leap second occured? Probably not desireable. The only sane answer seems to be: /Not at all!/
--
-- In reality the problem is less severe than it seems: Your system clock is most probably
-- counting unix seconds and does not know about leap seconds either. So chances are that
-- when dealing with computer generated timestamps you'll never encounter problems with
-- leap seconds.