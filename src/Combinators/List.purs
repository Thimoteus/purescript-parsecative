module Parsecative.Combinators.List where

import Prelude

import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEList
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (invalid, unV)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Global (isFinite, isNaN, readFloat)
import Parsecative (Parsecative(..))
import Parsecative.Combinators ((|=))
import Parsecative.Combinators as C
import Parsecative.Error (class ParserError, fromString)

uncons :: ∀ e a. Semigroup e => ParserError e => Parsecative e (List a) a
uncons = C.uncons List.uncons

elementBy :: ∀ e a. Semigroup e => ParserError e => (a -> a -> Boolean) -> a -> Parsecative e (List a) Unit
elementBy eq a = void (uncons |= eq a)

element :: ∀ e a. Semigroup e => ParserError e => Eq a => a -> Parsecative e (List a) Unit
element = elementBy eq

untilBy :: ∀ e a. Semigroup e => ParserError e => (List a -> List a -> Boolean) -> (a -> Boolean) -> Parsecative e (List a) Unit
untilBy eq pred = void (manyBy eq (uncons |= not <<< pred))

until :: ∀ e a. Semigroup e => ParserError e => Eq a => (a -> Boolean) -> Parsecative e (List a) Unit
until = untilBy eq

nil :: ∀ e a. Semigroup e => ParserError e => Parsecative e (List a) Unit
nil =
  Parsecative \ref -> ado
    str <- liftEffect $ Ref.read ref
    in case str of
      List.Nil -> pure unit
      _ -> invalid (fromString "Expected Nil")

-- | Given an equality test on streams, parse many occurrences of a given parser.
-- | If the parser fails to consume the stream, the accumulated list will be returned.
-- | Consumption is just a check if the old stream is equivalent to the
-- | current stream, using the given equivalence function.
manyBy :: ∀ e s a. Semigroup e => (s -> s -> Boolean) -> Parsecative e s a -> Parsecative e s (List a)
manyBy eq (Parsecative p) =
  Parsecative \ref -> do
    str <- liftEffect $ Ref.read ref
    let
      go acc str' = do
        ea <- p ref
        str'' <- liftEffect $ Ref.read ref
        if not (str' `eq` str'')
          then unV (\_ -> pure $ pure acc) (\a -> go (List.Cons a acc) str'') ea
          else pure $ pure acc
    go List.Nil str

someBy :: ∀ e s a. Semigroup e => (s -> s -> Boolean) -> Parsecative e s a -> Parsecative e s (NonEmptyList a)
someBy pred p = ado
  a <- p
  as <- manyBy pred p
  in NEList.cons' a as

many :: ∀ e s a. Semigroup e => Eq s => Parsecative e s a -> Parsecative e s (List a)
many = manyBy eq

some :: ∀ e s a. Semigroup e => Eq s => Parsecative e s a -> Parsecative e s (NonEmptyList a)
some = someBy eq

int :: ∀ e. Semigroup e => ParserError e => Parsecative e (List String) Int
int =
  Parsecative \ref -> do
    str <- liftEffect $ Ref.read ref
    case str of
      List.Cons x xs -> do
        case Int.fromString x of
          Just n -> pure (pure n)
          _ -> do
            liftEffect $ Ref.write xs ref
            pure (invalid (fromString "Could not parse string as an int"))
      _ -> pure (invalid (fromString "Empty stream"))

number :: ∀ e. Semigroup e => ParserError e => Parsecative e (List String) Number
number =
  Parsecative \ref -> do
    str <- liftEffect $ Ref.read ref
    case str of
      List.Cons x xs -> do
        liftEffect $ Ref.write xs ref
        let
          n = readFloat x
        pure if isNaN n || not (isFinite n)
          then pure n
          else invalid (fromString "Could not parse string as an int")
      _ -> pure (invalid (fromString "Empty stream"))

-- | Takes the next two elements in the stream
pair :: ∀ e a. Semigroup e => ParserError e => Parsecative e (List a) (Tuple a a)
pair = ado
  fst <- uncons
  snd <- uncons
  in Tuple fst snd