module Data.Medea.Parser.Spec.Property (Specification, parseSpecification, mkSpec, propName, propSchema, propOptional) where

import MedeaPrelude
import Data.Medea.Parser.Primitive (Identifier, MedeaString, ReservedIdentifier(..), parseIdentifier, parseLine, parseReserved, parseString, parseKeyVal)
import Data.Medea.Parser.Types (MedeaParser)
import Parsing.Combinators (option, try)

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
propName (Specification { propName: pn }) = pn

propSchema :: Specification -> Maybe Identifier
propSchema (Specification { propSchema: ps }) = ps

propOptional :: Specification -> Boolean
propOptional (Specification { propOptional: po }) = po

derive instance eqSpecification :: Eq Specification

mkSpec :: MedeaString -> Maybe Identifier -> Boolean -> Specification
mkSpec pn ps po = Specification { propName: pn, propSchema: ps, propOptional: po }

parseSpecification :: MedeaParser Specification
parseSpecification =
  mkSpec
    <$> parsePropName
    <*> parsePropSchema
    <*> parsePropOptional
  where
  parsePropName = parseLine 8 $ parseKeyVal RPropertyName parseString

  parsePropSchema =
    option Nothing <<< try <<< parseLine 8
      $ Just
      <$> parseKeyVal RPropertySchema parseIdentifier

  parsePropOptional =
    option false <<< try <<< parseLine 8
      $ parseReserved ROptionalProperty
      $> true
