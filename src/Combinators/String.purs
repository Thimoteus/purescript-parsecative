module Parsecative.Combinators.String where

import Prelude

import Data.Char.Unicode (isDigit)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (CodePoint)
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Validation.Semigroup (invalid)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Global (readFloat)
import Parsecative (Parsecative(..))
import Parsecative.Combinators (fromChars, (<?>), (|=))
import Parsecative.Combinators as C
import Parsecative.Combinators.List (some)
import Parsecative.Error (class ParserError, fromString)

uncons :: ∀ e. Semigroup e => ParserError e => Parsecative e String CodePoint
uncons = C.uncons String.uncons

uncons' :: ∀ e. Semigroup e => ParserError e => Parsecative e String Char
uncons' = C.uncons CodeUnits.uncons

eof :: ∀ e. Semigroup e => ParserError e => Parsecative e String Unit
eof =
  Parsecative \ref -> ado
    str <- liftEffect $ Ref.read ref
    in case str of
      "" -> pure unit
      _ -> invalid (fromString "Expected EOF")

string :: ∀ e. Semigroup e => ParserError e => String -> Parsecative e String Unit
string s =
  Parsecative \ref -> do
    str <- liftEffect $ Ref.read ref

    let
      l = String.length s
      init = String.take l str

    liftEffect $ Ref.modify_ (String.drop l) ref

    pure if init == s
      then pure unit
      else
        invalid $
          fromString $
            "Expected '" <> s <> "' but found '" <> init <> "'"

char :: ∀ e. Semigroup e => ParserError e => Char -> Parsecative e String Unit
char c = void (uncons' |= (_ == c) <?> fromString ("Expected " <> show c))

codePoint :: ∀ e. Semigroup e => ParserError e => CodePoint -> Parsecative e String Unit
codePoint cp = void (uncons |= (_ == cp) <?> fromString ("Expected " <> show cp))

integral :: ∀ e. Semigroup e => ParserError e => Parsecative e String String
integral = fromChars <$> some (uncons' |= isDigit)

-- | In case of integer overflow, returns 0.
int :: ∀ e. Semigroup e => ParserError e => Parsecative e String Int
int = ado
  i <- integral
  in case Int.fromString i of
    Just x -> x
    _ -> 0

number :: ∀ e. Semigroup e => ParserError e => Parsecative e String Number
number = ado
  nat <- integral
  char '.'
  dec <- integral
  in readFloat (nat <> "." <> dec)