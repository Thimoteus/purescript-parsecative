module Parsecative.Combinators.String where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref as Ref
import Data.Char.Unicode (isDigit)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodePoints (CodePoint)
import Data.String.CodePoints as CodePoint
import Data.Validation.Semigroup (invalid)
import Global (readFloat)
import Parsecative (Parsecative(..))
import Parsecative.Combinators (fromChars, (<?>), (|=))
import Parsecative.Combinators as C
import Parsecative.Combinators.List (some)
import Parsecative.Error (class ParserError, fromString)

uncons :: ∀ eff e. Semigroup e => ParserError e => Parsecative eff e String CodePoint
uncons = C.uncons CodePoint.uncons

uncons' :: ∀ eff e. Semigroup e => ParserError e => Parsecative eff e String Char
uncons' = C.uncons String.uncons

eof :: ∀ eff e. Semigroup e => ParserError e => Parsecative eff e String Unit
eof =
  Parsecative \ref -> do
    str <- liftEff $ Ref.readRef ref
    pure case str of
      "" -> pure unit
      _ -> invalid (fromString "Expected EOF")

string :: ∀ eff e. Semigroup e => ParserError e => String -> Parsecative eff e String Unit
string s =
  Parsecative \ref -> do
    str <- liftEff $ Ref.readRef ref

    let
      l = String.length s
      init = String.take l str

    liftEff $ Ref.modifyRef ref (String.drop l)

    pure if init == s
      then pure unit
      else
        invalid $
          fromString $
            "Expected '" <> s <> "' but found '" <> init <> "'"

char :: ∀ eff e. Semigroup e => ParserError e => Char -> Parsecative eff e String Unit
char c = void (uncons' |= (_ == c) <?> fromString ("Expected " <> show c))

codePoint :: ∀ eff e. Semigroup e => ParserError e => CodePoint -> Parsecative eff e String Unit
codePoint cp = void (uncons |= (_ == cp) <?> fromString ("Expected " <> show cp))

integral :: ∀ eff e. Semigroup e => ParserError e => Parsecative eff e String String
integral = fromChars <$> some (uncons' |= isDigit)

-- | In case of integer overflow, returns 0.
int :: ∀ eff e. Semigroup e => ParserError e => Parsecative eff e String Int
int = f <$> integral
  where
  f i = case Int.fromString i of
    Just x -> x
    _ -> 0

number :: ∀ eff e. Semigroup e => ParserError e => Parsecative eff e String Number
number =
  (\nat _ dec -> readFloat (nat <> "." <> dec)) <$>
  integral <*>
  char '.' <*>
  integral