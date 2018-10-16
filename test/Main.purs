module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Parsecative (Parsecative, parse)
import Parsecative.Combinators (char, int, unconsString')
import Parsecative.Error (class ParserError)

newtype ParserError = ParserError Unit

derive newtype instance semigroup :: Semigroup ParserError

derive newtype instance parserError :: ParserError ParserError

couple :: Char -> Char -> Parsecative ParserError String (Tuple Char Char)
couple open close = ado
  char open
  fst <- unconsString'
  char ','
  snd <- unconsString'
  char close
  in Tuple fst snd

parens :: Parsecative ParserError String (Tuple Char Char)
parens = couple '(' ')'

brackets :: Parsecative ParserError String (Tuple Char Char)
brackets = couple '[' ']'

braces :: Parsecative ParserError String (Tuple Char Char)
braces = couple '{' '}'

logResult :: ∀ a. Show a => Either ParserError a -> Effect Unit
logResult = case _ of
  Right t -> log (show t)
  _ -> log "Error!"

main :: Effect Unit
main = do
  for_ ["{1,j}", "(a,7)", "[9,9]"] \str -> parse (parens <|> brackets <|> braces) str logResult
  
  parse int "1209" logResult
