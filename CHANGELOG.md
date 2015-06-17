0.2.0.1 Lars Petersen <info@lars-petersen.net> 2015-06-17

  * Resolved issue with Monad/Applicative/Functor (affected only older base).
  * Fixed several warnings about scope ambiguties.

0.2.0.0 Lars Petersen <info@lars-petersen.net> 2015-06-16

  * Flipped order of fields in `Local` type.
  * Removed `Midnight` in favour of `Epoch`.
  * Only expose module `Data.UTC` as it contains everything anyway.
  * Added functions for rendering according to ISO8601.
  * Use `MonadThrow` and `UtcException` instead of `Monad.fail` 
    (`fail` is just for pattern-match fail as I learned).