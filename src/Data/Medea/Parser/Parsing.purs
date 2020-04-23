module Data.Medea.Parser.Parsing where

import MedeaPrelude
import Control.Alternative ((<|>))
import Control.Monad.State (gets, modify_)
import Data.String (drop) as String
import Data.String.CodeUnits (singleton, toCharArray, fromCharArray) as String
import Data.String.Regex as R
import Data.String.Regex.Flags as RF
import Text.Parsing.Parser (ParserT, ParseState(..), fail)
import Text.Parsing.Parser.String (class StringLike, eof, string)
import Text.Parsing.Parser.Pos (updatePosString)
import Partial.Unsafe (unsafePartial)
-- this module is used to tack combinators onto `purescript-parsing`
-- in order to shore up semantics with what we're using out of MegaParsec
-- and other haskell libs

---BUG - i suspect takeWhile1P is not consuming input correctly
takeWhile1P :: forall m. Monad m => String -> (Char -> Boolean) -> ParserT String m (Array Char)
takeWhile1P errormsg pred = do
  input <- String.toCharArray <$> gets \(ParseState input _ _) -> input
  let result = takeWhile pred input
  if length result == 0 
    then fail errormsg
    else do
       -- consume
    modify_ \(ParseState _ position _) -> 
      ParseState (String.drop (length result) $ String.fromCharArray input)
                 (updatePosString position $ String.fromCharArray result)
                 true
    pure result


isWhitespace ∷ Char → Boolean
isWhitespace = R.test wsRegex <<< String.singleton
  where
  wsRegex ∷ R.Regex
  wsRegex = unsafePartial fromRight $
    R.regex "^\\s$" RF.noFlags

eol :: forall s m. StringLike s => Monad m => ParserT s m Unit
eol = (void $ string "\n") <|> (void $ string "\r\n") <|> eof <|> fail "expected EOL"

