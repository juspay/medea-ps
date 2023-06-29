module Data.Medea.Parser.Spec.Object
  ( Specification(..)
  , parseSpecification
  , mkSpec
  , properties
  , additionalAllowed
  , additionalSchema
  ) where

import MedeaPrelude
import Data.Medea.Parser.Primitive (Identifier, ReservedIdentifier(..), parseIdentifier, parseKeyVal, parseLine, parseReserved)
import Data.Medea.Parser.Spec.Property as Property
import Data.Medea.Parser.Types (MedeaParser, MedeaParseErr(..))
import Parsing (fail)
import Parsing.Combinators (option, try)

data Specification
  = Specification
    { properties :: Array Property.Specification
    , additionalAllowed :: Boolean
    , additionalSchema :: Maybe Identifier
    }

--getters
properties :: Specification -> Array Property.Specification
properties (Specification { properties: p }) = p

additionalAllowed :: Specification -> Boolean
additionalAllowed (Specification { additionalAllowed: a }) = a

additionalSchema :: Specification -> Maybe Identifier
additionalSchema (Specification { additionalSchema: as }) = as

derive instance eqSpecification :: Eq Specification

mkSpec :: Array Property.Specification -> Boolean -> Maybe Identifier-> Specification
mkSpec p aa as = Specification { properties: p, additionalAllowed: aa, additionalSchema: as }

parseSpecification :: MedeaParser Specification
parseSpecification = do
  _ <- parseLine 4 $ parseReserved RProperties
  props <- parseProperties
  additionalAllowed' <- parseAdditionalAllowed
  additionalSchema' <- parseAdditionalSchema
  when ((not additionalAllowed') && (isJust additionalSchema')) $
    fail $ show $ ConflictingSpecRequirements
  pure $ mkSpec props additionalAllowed' additionalSchema'
  where
  parseProperties = many (try Property.parseSpecification)

  parseAdditionalAllowed = option false $ try $ (parseLine 8 $ parseReserved RAdditionalPropertiesAllowed $> true)
  parseAdditionalSchema :: MedeaParser (Maybe Identifier)
  parseAdditionalSchema = option Nothing <<< map Just <<< try <<< parseLine 8 $ parseKeyVal RAdditionalPropertySchema parseIdentifier
