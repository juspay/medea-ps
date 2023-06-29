module Data.Medea.Parser.Spec.Array
  ( Specification(..)
  , parseSpecification
  , mkSpec
  , defaultSpec
  ) where

import MedeaPrelude
import Control.Alternative ((<|>))
import Data.Natural (Natural)
import Data.Medea.Parser.Permutation (toPermutationWithDefault, runPermutation)
import Data.Medea.Parser.Primitive (Identifier, ReservedIdentifier(..), parseIdentifier, parseKeyVal, parseReserved, parseNatural, parseLine)
import Data.Medea.Parser.Types (MedeaParser, MedeaParseErr(..))
import Parsing (fail)
import Parsing.Combinators (try)

data Specification
  = Specification
    { minLength :: Maybe Natural
    , maxLength :: Maybe Natural
    , elementType :: Maybe Identifier
    , tupleSpec :: Maybe (Array Identifier)
    }

-- tupleSpec with an empty list indicates an empty tuple/encoding of unit
-- tupleSpec of Nothing indicates that there is no tuple spec at all
mkSpec :: Maybe Natural -> Maybe Natural -> Maybe Identifier -> Maybe (Array Identifier) -> Specification
mkSpec minLen maxLen elemType tupleS = Specification { minLength: minLen, maxLength: maxLen, elementType: elemType, tupleSpec: tupleS }

-- getters
minLength :: Specification -> Maybe Natural
minLength (Specification { minLength: m }) = m

maxLength :: Specification -> Maybe Natural
maxLength (Specification { maxLength: m }) = m

derive instance eqSpecification :: Eq Specification

defaultSpec :: Specification
defaultSpec = Specification { minLength: Nothing, maxLength: Nothing, elementType: Nothing, tupleSpec: Nothing }

combineSpec :: Specification -> Specification -> Specification
combineSpec (Specification { minLength: a1, maxLength: b1, elementType: c1, tupleSpec: d1 }) (Specification { minLength: a2, maxLength: b2, elementType: c2, tupleSpec: d2 }) = Specification { minLength: a1 <|> a2, maxLength: b1 <|> b2, elementType: c1 <|> c2, tupleSpec: d1 <|> d2 }

parseSpecification :: MedeaParser Specification
parseSpecification = do
  spec <- try permute
  case spec of
    Specification { minLength: Nothing, maxLength: Nothing, elementType: Nothing, tupleSpec: Nothing } -> fail $ show EmptyLengthArraySpec
    Specification { elementType: (Just _), tupleSpec: (Just _) } -> fail $ show ConflictingSpecRequirements
    Specification { minLength: (Just _), tupleSpec: (Just _) } -> fail $ show ConflictingSpecRequirements
    Specification { maxLength: (Just _), tupleSpec: (Just _) } -> fail $ show EmptyLengthArraySpec
    _ -> pure spec
  where
  permute =
    runPermutation $ mkSpec
      <$> toPermutationWithDefault Nothing (try parseMinSpec)
      <*> toPermutationWithDefault Nothing (try parseMaxSpec)
      <*> toPermutationWithDefault Nothing (try parseElementType)
      <*> toPermutationWithDefault Nothing (try parseTupleSpec)

parseMinSpec :: MedeaParser (Maybe Natural)
parseMinSpec = parseLine 4 $ Just <$> parseKeyVal RMinLength parseNatural

parseMaxSpec :: MedeaParser (Maybe Natural)
parseMaxSpec = parseLine 4 $ Just <$> parseKeyVal RMaxLength parseNatural

parseElementType :: MedeaParser (Maybe Identifier)
parseElementType = do
  _ <- parseLine 4 $ parseReserved RElementType
  element <- parseLine 8 parseIdentifier <|> (fail $ show EmptyArrayElements)
  pure $ Just element

parseTupleSpec :: MedeaParser (Maybe (Array Identifier))
parseTupleSpec = do
  _ <- parseLine 4 $ parseReserved RTuple
  elemList <- many $ try $ parseLine 8 parseIdentifier
  pure $ Just elemList
