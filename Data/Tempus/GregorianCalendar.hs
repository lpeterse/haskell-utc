module Data.Tempus.GregorianCalendar
  ( GregorianCalendar(..)
  , convertGregorianCalendar
  ) where

import Control.Monad

class GregorianCalendar a where
  -- | > fromRfc3339String "0000-00-00T00:00:00Z"  == Just commonEpoch
  commonEpoch        :: a

  -- | > getYear        "2014-⁠12-⁠24T18:11:47.042Z" == Just 2014
  getYear            :: (MonadPlus m) => a -> m Int
  -- | > getMonth       "2014-⁠12-⁠24T18:11:47.042Z" == Just   12
  getMonth           :: (MonadPlus m) => a -> m Int
  -- | > getDay         "2014-⁠12-⁠24T18:11:47.042Z" == Just   24
  getDay             :: (MonadPlus m) => a -> m Int
  -- | > getHour        "2014-⁠12-⁠24T18:11:47.042Z" == Just   18
  getHour            :: (MonadPlus m) => a -> m Int
  -- | > getMinute      "2014-⁠12-⁠24T18:11:47.042Z" == Just   11
  getMinute          :: (MonadPlus m) => a -> m Int
  -- | > getSecond      "2014-⁠12-⁠24T18:11:47.042Z" == Just   47
  getSecond          :: (MonadPlus m) => a -> m Int
  -- | > getMilliSecond "2014-⁠12-⁠24T18:11:47.042Z" == Just   42
  getMilliSecond        :: (MonadPlus m) => a -> m Int
  getLocalOffset        :: (MonadPlus m) => a -> m (Maybe Int)
  setYear               :: (MonadPlus m) => Int -> a -> m a
  setMonth              :: (MonadPlus m) => Int -> a -> m a
  setDay                :: (MonadPlus m) => Int -> a -> m a
  setHour               :: (MonadPlus m) => Int -> a -> m a
  setMinute             :: (MonadPlus m) => Int -> a -> m a
  setSecond             :: (MonadPlus m) => Int -> a -> m a
  setMilliSecond        :: (MonadPlus m) => Int -> a -> m a
  setLocalOffset        :: (MonadPlus m) => Maybe Int -> a -> m a
  addYears              :: Int -> a -> Maybe a
  addMonths             :: Int -> a -> Maybe a
  addDays               :: Int -> a -> Maybe a
  addHours              :: Int -> a -> Maybe a
  addMinutes            :: Int -> a -> Maybe a
  addSeconds            :: Int -> a -> Maybe a
  addMilliSeconds       :: Int -> a -> Maybe a

convertGregorianCalendar :: (MonadPlus m, GregorianCalendar a, GregorianCalendar b) => a -> m b
convertGregorianCalendar t
  = do year    <- getYear        t
       month   <- getMonth       t
       day     <- getDay         t
       minute  <- getMinute      t
       second  <- getSecond      t
       msecond <- getMilliSecond t
       offset  <- getLocalOffset t
       return commonEpoch
         >>= setYear        year
         >>= setMonth       month
         >>= setDay         day
         >>= setMinute      minute
         >>= setSecond      second
         >>= setMilliSecond msecond
         >>= setLocalOffset offset