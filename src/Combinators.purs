module Parsecative.Combinators where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Cont (ContT(..))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref as Ref
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodePoints (CodePoint)
import Data.String.CodePoints as CodePoint
import Data.Validation.Semigroup (invalid, unV)
import Parsecative (Parsecative(..))
import Parsecative.Error (class ParserError, fromString)

----------------------------------
-- | Higher-order combinators | --
----------------------------------

fail :: ∀ eff e s a. e -> Parsecative eff e s a
fail e = Parsecative \_ -> ContT (_ $ invalid e)

orFailWith :: ∀ eff e s a. Semigroup e => Parsecative eff e s a -> e -> Parsecative eff e s a
orFailWith p e = p <|> fail e

infix 0 orFailWith as <?>

-- | Peek into the result of running a parser.
lookahead :: ∀ eff e s a. Parsecative eff e s a -> Parsecative eff e s a
lookahead (Parsecative p) =
  Parsecative \ref -> do
    str <- liftEff $ Ref.readRef ref

    ea <- p ref
    liftEff $ Ref.writeRef ref str
    pure ea

optional :: ∀ eff e s a. Semigroup e => Parsecative eff e s a -> Parsecative eff e s (Maybe a)
optional (Parsecative p) =
  Parsecative \ref -> pure <<< pure <<< unV (\_ -> Nothing) Just =<< p ref

satisfies :: ∀ eff e s a. Semigroup e => ParserError e => (a -> Boolean) -> Parsecative eff e s a -> Parsecative eff e s a
satisfies pred (Parsecative p) =
  Parsecative \ref -> do
    ea <- p ref
    unV
      (\_ -> pure ea)
      (\a ->
        pure if pred a
          then ea
          else invalid (fromString "Failed to match predicate")
      )
      ea

suchThat :: ∀ eff e s a. Semigroup e => ParserError e => Parsecative eff e s a -> (a -> Boolean) -> Parsecative eff e s a
suchThat = flip satisfies

infixl 5 suchThat as |=

-- | Reset the stream in case of failure.
try :: ∀ eff e s a. Parsecative eff e s a -> Parsecative eff e s a
try (Parsecative p) =
  Parsecative \ref -> do
    str <- liftEff $ Ref.readRef ref

    ea <- p ref
    unV
      (\_ -> do
        liftEff $ Ref.writeRef ref str
        pure ea
      )
      (\_ -> pure ea
      )
      ea

----------------------------------------------
-- | Combinators for working with streams | --
----------------------------------------------

uncons :: ∀ eff e s a. Semigroup e => ParserError e => (s -> Maybe {head :: a, tail :: s}) -> Parsecative eff e s a
uncons uncons' =
  Parsecative \ref -> do
    str <- liftEff $ Ref.readRef ref
    case uncons' str of
      Just {head, tail} -> do
        liftEff $ Ref.writeRef ref tail
        pure $ pure head
      _ -> pure (invalid (fromString "Empty stream"))

stream :: ∀ eff e s. Semigroup e => Parsecative eff e s s
stream = Parsecative (pure <<< pure <=< liftEff <<< Ref.readRef)

-- | Lift a custom parsing function without consuming the stream
liftMaybeParser :: ∀ eff e s a. Semigroup e => ParserError e => (s -> Maybe a) -> Parsecative eff e s a
liftMaybeParser p =
  Parsecative \ref -> do
    str <- liftEff $ Ref.readRef ref
    pure case p str of
      Just x -> pure x
      _ -> invalid (fromString "Custom parser failed")

-- | Lift a custom parsing function without consuming the stream
liftEitherParser :: ∀ eff e s a. Semigroup e => (s -> Either e a) -> Parsecative eff e s a
liftEitherParser p =
  Parsecative \ref -> do
    str <- liftEff $ Ref.readRef ref
    pure case p str of
      Right x -> pure x
      Left e -> invalid e

-------------------
-- | Utilities | --
-------------------

fromChars :: ∀ f. Foldable f => f Char -> String
fromChars = foldMap String.singleton

fromCodePoints :: ∀ f. Foldable f => f CodePoint -> String
fromCodePoints = foldMap CodePoint.singleton