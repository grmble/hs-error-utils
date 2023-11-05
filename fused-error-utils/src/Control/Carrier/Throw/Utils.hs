{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Re-interpreting 'Throw Text'
-- Copyright   : Juergen Gmeiner
-- License     : MIT
-- Maintainer  : spamless.juergen@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Offers the functionality of 'Control.Monad.Fail.Utils'
-- but for 'Text'.  If you are using a sum error type:
-- running the effect and 'liftEither' is your friend.
module Control.Carrier.Throw.Utils
  ( ThrowC,
    -- ReThrowC (..),
    -- withThrow,
    throwEmpty,
    throwIf,
    throwLeft,
    throwNothing,
    throwPred,
    throwReadS,
    runPure,
    run,
  )
where

import Control.Algebra
import Control.Carrier.Error.Either
import Control.Carrier.Throw.Either (ThrowC, runThrow)
import Control.Monad (when)
import Data.Functor.Identity
import Data.Text

-- $setup
-- >>> import Data.Function ((&))

-- | Lift a Nothing into the 'Throw' effect, specialised for 'Text'
--
-- >>> throwNothing "boo!" (Nothing :: Either Text Int) & runPure
-- Left "boo!"
throwNothing :: (Has (Throw Text) sig m) => Text -> Maybe a -> m a
throwNothing err = maybe (throwError err) pure

throwLeft :: (Has (Throw Text) sig m) => Either Text a -> m a
throwLeft = liftEither

throwIf :: (Has (Throw Text) sig m) => Text -> Bool -> m ()
throwIf err b = when b (throwError err)

throwPred :: (Has (Throw Text) sig m) => Text -> (a -> Bool) -> a -> m ()
throwPred err f = throwIf err . f

throwEmpty :: (Has (Throw Text) sig m) => Text -> [a] -> m [a]
throwEmpty err [] = throwError err
throwEmpty _ x = pure x

throwReadS :: (Has (Throw Text) sig m) => Text -> [(a, String)] -> m a
throwReadS _ [(a, "")] = pure a
throwReadS _ ((a, "") : _) = pure a
throwReadS err _ = throwError err

{--

-- | Re-interprets 'Throw Text' into 'Throw'
newtype ReThrowC err m a = ReThrowC {runReThrow :: m a}
  deriving (Alternative, Applicative, Functor, Monad, MonadFix, MonadIO, MonadPlus)

instance (Algebra sig m, Has (Throw err2) sig m, Has (Reader (err1 -> err2)) sig m) => Algebra (Throw err1 :+: sig) (ReThrowC err1 m) where
  alg hdl sig ctx = case sig of
    L (Throw err) -> (<$ ctx) <$> (ask >>= \f -> throwError (f err))
    R other -> ReThrowC (alg (runReThrow . hdl) other ctx)

-- | Change from 'Fail' to 'Throw' while mapping the error type
withThrow :: (err1 -> err2) -> ReThrowC err (ReaderC (err1 -> err2) m) a -> m a
withThrow f = runReader f . runReThrow
--}

-- | Type-constrained unwrapper
--
-- Again, most useful in doctests
runPure :: ThrowC Text Identity a -> Either Text a
runPure = run . runThrow
