module Parsecative.Combinators.Array where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (invalid, unV)
import Effect.Ref as Ref
import Global (isFinite, isNaN, readFloat)
import Parsecative (Parsecative(..), liftEffect)
import Parsecative.Combinators ((|=))
import Parsecative.Combinators as C
import Parsecative.Error (class ParserError, fromString)

uncons :: ∀ e a. Semigroup e => ParserError e => Parsecative e (Array a) a
uncons = C.uncons Array.uncons

elementBy :: ∀ e a. Semigroup e => ParserError e => (a -> a -> Boolean) -> a -> Parsecative e (Array a) Unit
elementBy eq a = void (uncons |= eq a)

element :: ∀ e a. Semigroup e => ParserError e => Eq a => a -> Parsecative e (Array a) Unit
element = elementBy eq

untilBy :: ∀ e a. Semigroup e => ParserError e => (Array a -> Array a -> Boolean) -> (a -> Boolean) -> Parsecative e (Array a) Unit
untilBy eq pred = void (manyBy eq (uncons |= not <<< pred))

until :: ∀ e a. Semigroup e => ParserError e => Eq a => (a -> Boolean) -> Parsecative e (Array a) Unit
until = untilBy eq

empty :: ∀ e a. Semigroup e => ParserError e => Parsecative e (Array a) Unit
empty =
  Parsecative \ref -> ado
    str <- liftEffect $ Ref.read ref
    in case str of
      [] -> pure unit
      _ -> invalid (fromString "Expected empty array")

-- | Given an equality test on streams, parse many occurrences of a given parser.
-- | If the parser fails to consume the stream, the accumulated list will be returned.
-- | Consumption is just a check if the old stream is equivalent to the
-- | current stream, using the given equivalence function.
manyBy :: ∀ e s a. Semigroup e => (s -> s -> Boolean) -> Parsecative e s a -> Parsecative e s (Array a)
manyBy eq (Parsecative p) =
  Parsecative \ref -> do
    str <- liftEffect $ Ref.read ref
    let
      go acc str' = do
        ea <- p ref
        str'' <- liftEffect $ Ref.read ref
        if not (str' `eq` str'')
          then unV (\_ -> pure $ pure acc) (\a -> go (Array.cons a acc) str'') ea
          else pure $ pure acc
    go [] str

someBy :: ∀ e s a. Semigroup e => (s -> s -> Boolean) -> Parsecative e s a -> Parsecative e s (NonEmptyArray a)
someBy pred p = ado
  a <- p
  as <- manyBy pred p
  in NEArray.cons' a as

many :: ∀ e s a. Semigroup e => Eq s => Parsecative e s a -> Parsecative e s (Array a)
many = manyBy eq

some :: ∀ e s a. Semigroup e => Eq s => Parsecative e s a -> Parsecative e s (NonEmptyArray a)
some = someBy eq

int :: ∀ e. Semigroup e => ParserError e => Parsecative e (Array String) Int
int =
  Parsecative \ref -> do
    str <- liftEffect $ Ref.read ref
    case Array.uncons str of
      Just {head, tail} -> ado
        liftEffect $ Ref.write tail ref
        in case Int.fromString head of
          Just n -> pure n
          _ -> invalid (fromString "Could not parse string as an int")
      _ -> pure (invalid (fromString "Empty stream"))

number :: ∀ e. Semigroup e => ParserError e => Parsecative e (Array String) Number
number =
  Parsecative \ref -> do
    str <- liftEffect $ Ref.read ref
    case Array.uncons str of
      Just {head, tail} -> do
        liftEffect $ Ref.write tail ref
        let
          n = readFloat head
        pure if isNaN n || not (isFinite n)
          then pure n
          else invalid (fromString "Could not parse string as an int")
      _ -> pure (invalid (fromString "Empty stream"))

pair :: ∀ e a. Semigroup e => ParserError e => Parsecative e (Array a) (Tuple a a)
pair = ado
  fst <- uncons
  snd <- uncons
  in Tuple fst snd