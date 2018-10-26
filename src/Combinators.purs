module Parsecative.Combinators where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Cont (ContT(..))
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint)
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Validation.Semigroup (invalid, unV)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Parsecative (Parsecative(..))
import Parsecative.Error (class ParserError, fromString)

----------------------------------
-- | Higher-order combinators | --
----------------------------------

fail :: ∀ e s a. e -> Parsecative e s a
fail e = Parsecative \_ -> ContT (_ $ invalid e)

orFailWith :: ∀ e s a. Semigroup e => Parsecative e s a -> e -> Parsecative e s a
orFailWith p e = p <|> fail e

infix 0 orFailWith as <?>

-- | Peek into the result of running a parser.
lookahead :: ∀ e s a. Parsecative e s a -> Parsecative e s a
lookahead (Parsecative p) =
  Parsecative \ref -> do
    str <- liftEffect $ Ref.read ref

    ea <- p ref
    liftEffect $ Ref.write str ref
    pure ea

optional :: ∀ e s a. Semigroup e => Parsecative e s a -> Parsecative e s (Maybe a)
optional (Parsecative p) =
  Parsecative \ref -> pure <<< pure <<< unV (\_ -> Nothing) Just =<< p ref

satisfies :: ∀ e s a. Semigroup e => ParserError e => (a -> Boolean) -> Parsecative e s a -> Parsecative e s a
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

suchThat :: ∀ e s a. Semigroup e => ParserError e => Parsecative e s a -> (a -> Boolean) -> Parsecative e s a
suchThat = flip satisfies

infixl 5 suchThat as |=

-- | Reset the stream in case of failure.
try :: ∀ e s a. Parsecative e s a -> Parsecative e s a
try (Parsecative p) =
  Parsecative \ref -> do
    str <- liftEffect $ Ref.read ref

    ea <- p ref
    unV
      (\_ -> ado
        liftEffect $ Ref.write str ref
        in ea
      )
      (\_ -> pure ea
      )
      ea

----------------------------------------------
-- | Combinators for working with streams | --
----------------------------------------------

uncons :: ∀ e s a. Semigroup e => ParserError e => (s -> Maybe {head :: a, tail :: s}) -> Parsecative e s a
uncons uncons' =
  Parsecative \ref -> do
    str <- liftEffect $ Ref.read ref
    case uncons' str of
      Just {head, tail} -> ado
        liftEffect $ Ref.write tail ref
        in pure head
      _ -> pure (invalid (fromString "Empty stream"))

stream :: ∀ e s. Semigroup e => Parsecative e s s
stream = Parsecative (pure <<< pure <=< liftEffect <<< Ref.read)

-- | Lift a custom parsing function without consuming the stream
liftMaybeParser :: ∀ e s a. Semigroup e => ParserError e => (s -> Maybe a) -> Parsecative e s a
liftMaybeParser p =
  Parsecative \ref -> ado
    str <- liftEffect $ Ref.read ref
    in case p str of
      Just x -> pure x
      _ -> invalid (fromString "Custom parser failed")

-- | Lift a custom parsing function without consuming the stream
liftEitherParser :: ∀ e s a. Semigroup e => (s -> Either e a) -> Parsecative e s a
liftEitherParser p =
  Parsecative \ref -> ado
    str <- liftEffect $ Ref.read ref
    in case p str of
      Right x -> pure x
      Left e -> invalid e

-------------------
-- | Utilities | --
-------------------

fromChars :: ∀ f. Foldable f => f Char -> String
fromChars = foldMap CodeUnits.singleton

fromCodePoints :: ∀ f. Foldable f => f CodePoint -> String
fromCodePoints = foldMap String.singleton