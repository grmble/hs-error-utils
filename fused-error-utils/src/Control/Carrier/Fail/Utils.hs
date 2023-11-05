{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Re-interpreting 'Fail' into 'Throw'
-- Copyright   : Juergen Gmeiner
-- License     : MIT
-- Maintainer  : spamless.juergen@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The fail utilites from 'Control.Monad.Fail.Utils' are re-exported
--
-- fused-effects builtin 'Throw' and 'Fail' effects
-- allow you to extract the error message (the catch effect
-- is also an option).
--
-- 'ToThrowC' re-interprets the 'Fail' effect
-- into an outer 'Throw' effect while transforming
-- fails error string into the error type of the outer monad.
--
module Control.Carrier.Fail.Utils
  ( MonadFail (..),
    FailC (..),
    ToThrowC (..),
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
    withFail,
  )
where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Carrier.Error.Either
import Control.Carrier.Fail.Either
import Control.Carrier.Reader
import Control.Monad (MonadPlus)
import Control.Monad.Fail.Utils
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor.Identity

-- | Re-interprets 'Fail' into 'Throw'
newtype ToThrowC err m a = ToThrowC {runToThrow :: m a}
  deriving (Alternative, Applicative, Functor, Monad, MonadFix, MonadIO, MonadPlus)

instance (Has (Throw err) sig m, Has (Reader (String -> err)) sig m) => MonadFail (ToThrowC err m) where
  fail = send . Fail

instance (Algebra sig m, Has (Throw err) sig m, Has (Reader (String -> err)) sig m) => Algebra (Fail :+: sig) (ToThrowC err m) where
  alg hdl sig ctx = case sig of
    L (Fail err) -> (<$ ctx) <$> (ask @(String -> err) >>= \f -> throwError (f err))
    R other -> ToThrowC (alg (runToThrow . hdl) other ctx)

-- | Change from 'Fail' to 'Throw' while mapping the error type
withFail :: (String -> err) -> ToThrowC err (ReaderC (String -> err) m) a -> m a
withFail f = runReader f . runToThrow

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
