module Data.Medea.Parser.Parsing where

import MedeaPrelude
import Control.Alternative ((<|>))
import Control.Monad.State (gets)
import Data.String.CodeUnits (toCharArray) as String
import Text.Parsing.Parser (ParserT, ParseState(..), fail)
import Text.Parsing.Parser.String (class StringLike, string)
-- this module is used to tack combinators onto `purescript-parsing`
-- in order to shore up semantics with what we're using out of MegaParsec
-- and other haskell libs

takeWhile1P :: forall m. Monad m => String -> (Char -> Boolean) -> ParserT String m (Array Char)
takeWhile1P errormsg pred = do
  input <- String.toCharArray <$> gets \(ParseState input _ _) -> input
  let result = takeWhile pred input
  if length result == 0 
    then fail errormsg
    else pure result


eol :: forall s m. StringLike s => Monad m => ParserT s m String
eol = string "\n" <|> string "\r\n"

