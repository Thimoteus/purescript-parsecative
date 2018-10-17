module Parsecative
  ( Parsecative(..)
  , parse
  , exec
  , withError
  , withState
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref)
import Control.Monad.Eff.Ref as Ref
import Control.Plus (class Plus)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Validation.Semigroup (V, unV, invalid)
import Parsecative.Error (class ParserError, fromString)

newtype Parsecative eff e s a =
  Parsecative
    (Ref s -> ContT Unit (Eff (ref :: REF | eff)) (V e a))

derive instance newtypeParsecative :: Newtype (Parsecative eff e s a) _

instance functorParsecative :: Semigroup e => Functor (Parsecative eff e s) where
  map = liftA1

instance applyParsecative :: Semigroup e => Apply (Parsecative eff e s) where
  apply (Parsecative pab) (Parsecative pa) =
    Parsecative \ref -> do
      ab <- pab ref
      a <- pa ref
      pure $ ab <*> a

instance applicativeParsecative :: Semigroup e => Applicative (Parsecative eff e s) where
  pure x = Parsecative \_ -> ContT (_ $ pure x)

-- | The `Alt` instance runs two parsers in parallel. If the first succeeds, its result is
-- | used and the second's is discarded. If the first fails, the second one is used.
-- | If the first hasn't completed by the time the second one has, the second waits until
-- | it knows how to act.
instance altParsecative :: Semigroup e => Alt (Parsecative eff e s) where
  alt (Parsecative pa) (Parsecative pb) =
    Parsecative \ref ->
      ContT \cb -> do
        -- Copy the stream so we're not concurrently mutating the same stream
        stream <- Ref.readRef ref
        ref' <- Ref.newRef stream

        -- Set the first parser's success state to "indeterminate"
        successRef <- Ref.newRef Nothing

        runContT (pa ref) \ea ->
          unV
            (\_ -> do
              Ref.writeRef successRef (Just false)
            )
            (\_ -> do
              Ref.writeRef successRef (Just true)
              cb ea
            )
            ea

        runContT (pb ref') \eb ->
          let
            go = do
              done <- Ref.readRef successRef
              case done of
                Just false -> cb eb
                Just true -> mempty
                _ -> go
          in go

instance plusParsecative :: (Semigroup e, ParserError e) => Plus (Parsecative eff e s) where
  empty = Parsecative \_ -> ContT (_ $ invalid (fromString "empty called"))

instance alternativeParsecative :: (Semigroup e, ParserError e) => Alternative (Parsecative eff e s)

instance semigroupParsecative :: (Semigroup e, Semigroup a) => Semigroup (Parsecative eff e s a) where
  append = lift2 append

instance monoidParsecative :: (Semigroup e, Monoid a) => Monoid (Parsecative eff e s a) where
  mempty = pure mempty

-- | Run a parser given an initial stream and a continuation.
parse :: ∀ eff e s a. Parsecative eff e s a -> s -> (Either e a -> Eff (ref :: REF | eff) Unit) -> Eff (ref :: REF | eff) Unit
parse p stream cb = exec p stream \ea _ -> cb ea

exec :: ∀ eff e s a. Parsecative eff e s a -> s -> (Either e a -> s -> Eff (ref :: REF | eff) Unit) -> Eff (ref :: REF | eff) Unit
exec (Parsecative p) stream cb = do
  streamRef <- Ref.newRef stream
  runContT (p streamRef) \ea -> do
    str <- Ref.readRef streamRef
    cb (unV Left Right ea) str

withError :: ∀ eff e e' s a. (e -> e') -> Parsecative eff e s a -> Parsecative eff e' s a
withError f (Parsecative pa) = Parsecative (map (lmap f) <<< pa)

withState :: ∀ eff e s a. (s -> s) -> Parsecative eff e s a -> Parsecative eff e s a
withState f (Parsecative p) =
  Parsecative \ref -> do
    liftEff $ Ref.modifyRef ref f
    p ref
