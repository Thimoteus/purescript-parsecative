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
import Control.Plus (class Plus)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Validation.Semigroup (V, unV, invalid)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Parsecative.Error (class ParserError, fromString)

newtype Parsecative e s a =
  Parsecative
    (Ref s -> ContT Unit Effect (V e a))

derive instance newtypeParsecative :: Newtype (Parsecative e s a) _

instance functorParsecative :: Semigroup e => Functor (Parsecative e s) where
  map = liftA1

instance applyParsecative :: Semigroup e => Apply (Parsecative e s) where
  apply (Parsecative pab) (Parsecative pa) =
    Parsecative \ref -> ado
      ab <- pab ref
      a <- pa ref
      in ab <*> a

instance applicativeParsecative :: Semigroup e => Applicative (Parsecative e s) where
  pure x = Parsecative \_ -> ContT (_ $ pure x)

-- | The `Alt` instance runs two parsers in parallel. If the first succeeds, its result is
-- | used and the second's is discarded. If the first fails, the second one is used.
-- | If the first hasn't completed by the time the second one has, the second waits until
-- | it knows how to act.
instance altParsecative :: Semigroup e => Alt (Parsecative e s) where
  alt (Parsecative pa) (Parsecative pb) =
    Parsecative \ref ->
      ContT \cb -> do
        -- Copy the stream so we're not concurrently mutating the same stream
        stream <- Ref.read ref
        ref' <- Ref.new stream

        -- Set the first parser's success state to "indeterminate"
        successRef <- Ref.new Nothing

        runContT (pa ref) \ea ->
          unV
            (\_ -> do
              Ref.write (Just false) successRef
            )
            (\_ -> do
              Ref.write (Just true) successRef
              cb ea
            )
            ea

        runContT (pb ref') \eb ->
          let
            go = do
              done <- Ref.read successRef
              case done of
                Just false -> cb eb
                Just true -> mempty
                _ -> go
          in go

instance plusParsecative :: (Semigroup e, ParserError e) => Plus (Parsecative e s) where
  empty = Parsecative \_ -> ContT (_ $ invalid (fromString "empty called"))

instance alternativeParsecative :: (Semigroup e, ParserError e) => Alternative (Parsecative e s)

instance semigroupParsecative :: (Semigroup e, Semigroup a) => Semigroup (Parsecative e s a) where
  append = lift2 append

instance monoidParsecative :: (Semigroup e, Monoid a) => Monoid (Parsecative e s a) where
  mempty = pure mempty

-- | Run a parser given an initial stream and a continuation.
parse :: ∀ e s a. Parsecative e s a -> s -> (Either e a -> Effect Unit) -> Effect Unit
parse p stream cb = exec p stream \ea _ -> cb ea

exec :: ∀ e s a. Parsecative e s a -> s -> (Either e a -> s -> Effect Unit) -> Effect Unit
exec (Parsecative p) stream cb = do
  streamRef <- Ref.new stream
  runContT (p streamRef) \ea -> do
    str <- Ref.read streamRef
    cb (unV Left Right ea) str

withError :: ∀ e e' s a. (e -> e') -> Parsecative e s a -> Parsecative e' s a
withError f (Parsecative pa) = Parsecative (map (lmap f) <<< pa)

withState :: ∀ e s a. (s -> s) -> Parsecative e s a -> Parsecative e s a
withState f (Parsecative p) =
  Parsecative \ref -> do
    liftEffect $ Ref.modify_ f ref
    p ref
