module Data.Medea.Parser.Parsing where

import MedeaPrelude

import Control.Alternative ((<|>))
import Data.Newtype (wrap)
import Data.String (drop) as String
import Data.String.CodeUnits (singleton, toCharArray, fromCharArray) as String
import Data.String.Common (split)
import Data.String.Regex as R
import Data.String.Regex.Flags as RF
import Parsing (ParseState(..), ParserT, Position(..), fail, getParserT, stateParserT)
import Parsing.String (eof, string)
import Partial.Unsafe (unsafePartial)

isWhitespace ∷ Char → Boolean
isWhitespace = R.test wsRegex <<< String.singleton
  where
  wsRegex ∷ R.Regex
  wsRegex =
    unsafePartial (fromRight
      $ R.regex "^\\s$" RF.noFlags)
  fromRight :: forall a b. Partial => Either a b -> b
  fromRight eab = case eab of
                      Right b -> b

eol :: forall m. Monad m => ParserT String m Unit
eol = (void $ string "\n") <|> (void $ string "\r\n") <|> eof <|> fail "expected EOL"
