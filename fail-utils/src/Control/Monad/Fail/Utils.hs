-- |
-- Description : Utilities for MonadFail
-- Copyright   : Juergen Gmeiner
-- License     : MIT
-- Maintainer  : spamless.juergen@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Simple utility functions to ease working with
-- 'MonadFail'.  The main utility are the constrained
-- types: text constants tend to need a lot of
-- type annotations.
--
-- The functions can be used with any 'MonadFail',
-- but the useful instances in base are limited.
--
-- * 'Maybe' turns the failure into 'Nothing'
-- * 'List' turns the failure into the empty list
-- * 'IO' will throw a runtime exception
--
-- For some reason the compiler tends to prefer the
-- list monad, which can produce strange errors.
--
-- There is no mainstream way to get a the error message,
-- you have to use some other package for that.
--
-- * FailT
-- * om-fail
-- * my own poison of choise is fused-effects, it has all
--   the needed instances.
--
-- But what you can do, even in base, is work
-- with 3 kinds of error encodings (Maybe, Either, MonadFail)
-- in a uniform way.
--
-- Then you either turn it into a 'Maybe', loosing the error
-- message, or turn it into an exception by using the IO instance.
module Control.Monad.Fail.Utils where

import Control.Monad (when)

-- | Lift an 'Either' into 'MonadIO'
--
-- This allows you to use the Either as Maybe or as Either.
--
-- Examples showing these conversion (but you may want to stay
-- in 'MonadFail' for a bit longer ...)
-- >>> failLeft (Left "boo!") :: Maybe Int
-- Nothing
--
-- >>> failLeft (Left "boo!") :: IO ()
-- *** Exception: user error (boo!)
--
-- >>> failLeft (Right True) :: Maybe Bool
-- Just True
failLeft :: (MonadFail m) => Either String a -> m a
failLeft = either fail pure

-- | Lift a 'Maybe' into 'MonadFail'
--
-- Maybe has a MonadFail instance, but once the type gets fixed
-- to Nothing, all those nice error messages get turned into
-- Nothing.
--
-- This allows you to handle explicit Maybes from 3rd party code.
-- Internally, you should stay in MonadFail for as long as possible.
--
-- >>> failNothing "boo!" Nothing :: [Bool]
-- []
--
-- >>> (,) <$> failLeft (Right True) <*> failNothing "boo!" Nothing :: IO (Bool,Int)
-- *** Exception: user error (boo!)
--
-- >>> failNothing "boo!" (Just True)
-- True
failNothing :: (MonadFail m) => String -> Maybe a -> m a
failNothing err = maybe (fail err) pure

-- | Lift a 'Bool' into 'MonadFail'
--
-- The boolean does not carry a useful value, so we map 'True'
-- to the unit typee.
failIf :: (MonadFail m) => String -> Bool -> m ()
failIf err b = when b (fail err)

-- | Lift a predicate and value into 'MonadFail'
failPred :: (MonadFail m) => String -> (a -> Bool) -> a -> m ()
failPred err f a = when (f a) (fail err)

-- | Lift a list into 'MonadFail'
--
-- Sometimes an empty list is an error condition.
failEmpty :: (MonadFail m) => String -> [a] -> m [a]
failEmpty err [] = fail err
failEmpty _ x = pure x

-- | Lift the result of a @ReadS@
--
-- This comes up when using the builtin parser
-- combinators (@ReadP@)
failReadS :: (MonadFail m) => String -> [(a, String)] -> m a
failReadS _ [(a, "")] = pure a
failReadS _ ((a, "") : _) = pure a
failReadS err _ = fail err
