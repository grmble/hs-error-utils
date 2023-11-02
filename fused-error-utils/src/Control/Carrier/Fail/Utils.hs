-- |
-- Description : Combinators for working with MonadFail
-- Copyright   : Juergen Gmeiner
-- License     : MIT
-- Maintainer  : spamless.juergen@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Fused effect's 'FailC' lets you get at a 'MonadFail' error string
-- from pure code.  Maybe will discard the message, and IO will
-- throw an exception, FailC can give you an Either.
--
-- As long as you are OK with string errors, this lets
-- you to work with 'MonadFail' instances, 'Maybe' and 'Either'
-- returns at the same time without lots of conditionals.
--
-- The alternative is using an 'ExceptT' monad.
-- This lets you work with sum error types, but
-- 'MonadFail' is only defined for the underlying
-- monad.
--
-- With @fused-effects@ you can just pop into
-- the 'Fail' effect where you need it (or where you
-- want to use these combinators), as soon as that
-- effect is gone you can start mapping the error
-- type.  This is also a good point map into
-- another text type using 'IsString'.
--
-- Most utility functions are just re-exported
-- from 'fail-utils'.  But there also 2
-- functions with a fixed fused effect carrier:
-- 'mkFail' is a typed constructor,
-- 'runPure' applies the effect in a pure context.
-- These 2 helpers are most useful in tests
-- and in the repl.
module Control.Carrier.Fail.Utils
  ( MonadFail (..),
    FailC (..),
    failEmpty,
    failIf,
    failLeft,
    failNothing,
    failPred,
    failReadS,
    mkFail,
    runPure,
    runFail,
    run,
  )
where

import Control.Carrier.Error.Either
import Control.Carrier.Fail.Either
import Control.Monad.Fail.Utils
import Data.Functor.Identity

-- | Type-Contrained constructor for testing
--
-- Nice to have in test cases - if you want to get at
-- the fail value, you have to specify a monad.
--
-- This is the simplest one.
mkFail :: a -> FailC Identity a
mkFail = pure

-- | Type-constrained unwrapper
--
-- Again, most useful in doctests
runPure :: FailC Identity a -> Either String a
runPure = run . runFail
