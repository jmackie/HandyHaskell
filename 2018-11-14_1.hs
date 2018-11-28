{-# OPTIONS -Weverything -fno-warn-implicit-prelude #-}

import qualified Data.Bifunctor
import Control.Applicative.Lift (eitherToErrors, runErrors)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)


sequenceEither :: (Traversable t, Monoid e) => t (Either e a) -> Either e (t a)
sequenceEither = sequenceEither' id


sequenceEither'
    :: (Traversable t, Monoid e)
    => (e' -> e)
    -> t (Either e' a)
    -> Either e (t a)
sequenceEither' f = runErrors . traverse (eitherToErrors . mapLeft f)


sequenceExceptT
    :: (Traversable t, Monoid e, Applicative f)
    => t (ExceptT e f a)
    -> ExceptT e f (t a)
sequenceExceptT = sequenceExceptT' id


sequenceExceptT'
    :: (Traversable t, Monoid e, Applicative f)
    => (e' -> e)
    -> t (ExceptT e' f a)
    -> ExceptT e f (t a)
sequenceExceptT' f es = ExceptT (sequenceEither' f <$> traverse runExceptT es)


mapLeft :: (e' -> e) -> Either e' a -> Either e a
mapLeft = Data.Bifunctor.first
