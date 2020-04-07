module Data.Medea.Parser.Spec.Array where

import MedeaPrelude
import Control.Alternative ((<|>))
import Data.Natural (Natural)
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (try)

import Data.Medea.Parser.Permutation (toPermutationWithDefault, runPermutation)
import Data.Medea.Parser.Primitive (parseKeyVal, parseReservedChunk, parseNatural, parseLine)
import Data.Medea.Parser.Types (MedeaParser, MedeaParseErr(..))

-- missing in PS - permutations, runPermutation, ToPermutation

data Specification = Specification {
  minLength :: Maybe Natural,
  maxLength :: Maybe Natural
}

derive instance eqSpecification :: Eq Specification

defaultSpec :: Specification
defaultSpec = Specification { minLength: Nothing, maxLength: Nothing }

combineSpec :: Specification -> Specification -> Specification
combineSpec 
  (Specification { minLength: a1, maxLength: b1 })
  (Specification { minLength: a2, maxLength: b2 })
  = Specification { minLength: a1 <|> a2, maxLength: b1 <|> b2 }

parseSpecification :: MedeaParser Specification
parseSpecification = do
  _ <- parseLine 4 $ parseReservedChunk "length"
  spec <- try permute
  case spec of 
       Specification { minLength: Nothing, maxLength: Nothing } -> fail $ show EmptyLengthSpec
       _ -> pure spec
  where 
    permute = runPermutation $ (\minLength maxLength -> Specification { minLength, maxLength })
      <$> toPermutationWithDefault Nothing (try parseMinSpec)
      <*> toPermutationWithDefault Nothing (try parseMaxSpec)
    parseMinSpec = parseLine 8 $ Just <$> parseKeyVal "minimum" parseNatural
    parseMaxSpec = parseLine 8 $ Just <$> parseKeyVal "maximum" parseNatural

