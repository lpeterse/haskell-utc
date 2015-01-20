{-# LANGUAGE Safe #-}
module Data.UTC.Class.IsUnixTime
  ( IsUnixTime(..)
  ) where

-- | This class is for types that have a well-defined
-- mapping to and from the <https://en.wikipedia.org/wiki/Unix_time Unix Time> system.
--
-- /Beware/: It is a common misconception that the Unix time in general counts
-- <https://en.wikipedia.org/wiki/International_System_of_Units SI> seconds since 1970-01-01T00:00:00Z.
-- There is a common definition that may be called /Unix time based on UTC/: In general, the second
-- boundaries match with UTC, but in the event of a positive (or negative) leap second
-- the Unix second has a duration of 2 (or 0) SI seconds. This library is in accordance with
-- this definition. This definition can also be understood as "ignoring leap seconds"
-- (a Unix day therefore always has 86400 Unix seconds).
--
-- The concrete behaviour of your system clock is implementation dependant.
class IsUnixTime t where
  unixSeconds      :: t -> Rational
  fromUnixSeconds  :: Monad m => Rational -> m t