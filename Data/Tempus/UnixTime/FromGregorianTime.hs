module Data.Tempus.UnixTime.FromGregorianTime where

import Control.Monad

import Data.Tempus.GregorianTime.Type
import Data.Tempus.UnixTime.Type
import Data.Tempus.Internal

fromGregorianTime :: MonadPlus m => GregorianTime -> m UnixTime
fromGregorianTime gdt
  = do days <- yearMonthDayToDays (gdtYear gdt, gdtMonth gdt, gdtDay gdt)
       return $ UnixTime $ ((fromIntegral days)   * 24 * 60 * 60 * 1000)
                         + ((fromIntegral $ gdtMinutes gdt) * 60 * 1000)
                         + (fromIntegral  $ gdtMilliSeconds gdt)
                         - 62167219200000 -- ms between 0000-01-01 and 1970-01-01
