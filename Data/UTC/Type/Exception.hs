{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.UTC.Type.Exception where

import Control.Exception

import Data.Typeable

-- | All non-total functions within this library throw a 'UtcException' exception
--   within a 'Control.Monad.Catch.MonadThrow' context. Use 'Control.Monad.Catch.MonadCatch'
--   to specifically catch this exception.
--
--   The 'Prelude.String' contains information that might be
--   useful for debugging, but its specific form is undefined and must not be relied on.
data UtcException
   = UtcException String
   deriving (Show, Typeable)

instance Exception UtcException
