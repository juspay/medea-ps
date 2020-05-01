module Data.Medea.Parser.Spec.Property
  (Specification, parseSpecification, mkSpec, propName, propSchema, propOptional) where

import MedeaPrelude
import Text.Parsing.Parser.Combinators (option, try)
import Data.Medea.Parser.Primitive (Identifier, MedeaString, parseIdentifier, parseLine, parseReservedChunk, parseString, parseKeyVal)
import Data.Medea.Parser.Types (MedeaParser)

data Specification 
  = Specification 
  { propName :: MedeaString
  , propSchema :: Maybe Identifier
  , propOptional :: Boolean
  }

derive instance genericSpecification :: Generic Specification _

instance showSpecification :: Show Specification where
  show x = genericShow x

propName :: Specification -> MedeaString
propName (Specification { propName }) = propName

propSchema :: Specification -> Maybe Identifier
propSchema (Specification { propSchema }) = propSchema

propOptional :: Specification -> Boolean
propOptional (Specification { propOptional }) = propOptional

derive instance eqSpecification :: Eq Specification

mkSpec :: MedeaString -> Maybe Identifier -> Boolean -> Specification
mkSpec propName propSchema propOptional = Specification {propName, propSchema, propOptional}

parseSpecification :: MedeaParser Specification
parseSpecification 
  = mkSpec
    <$> parsePropName
    <*> parsePropSchema
    <*> parsePropOptional
  where
    parsePropName = parseLine 8 $ parseKeyVal "property-name" parseString
    parsePropSchema = option Nothing <<< try <<< parseLine 8 $
      Just <$> parseKeyVal "property-schema" parseIdentifier
    parsePropOptional = option false <<< try <<< parseLine 8 $
      parseReservedChunk "optional-property" $> true
