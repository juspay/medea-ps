module Data.Medea.Parser.Spec.Object where

import MedeaPrelude
import Data.Medea.Parser.Primitive (parseLine, parseReservedChunk)
import Data.Medea.Parser.Spec.Property as Property
import Data.Medea.Parser.Types (MedeaParser)
import Text.Parsing.Parser.Combinators (option, try)

data Specification 
  = Specification 
  { properties :: Array Property.Specification
  , additionalAllowed :: Boolean
  }

derive instance eqSpecification :: Eq Specification

mkSpec :: Array Property.Specification -> Boolean -> Specification
mkSpec properties additionalAllowed = Specification { properties, additionalAllowed }

defaultSpec :: Specification
defaultSpec = mkSpec [] true

parseSpecification :: MedeaParser Specification
parseSpecification 
  = do
    _ <- parseLine 4 $ parseReservedChunk "properties"
    mkSpec <$> parseProperties <*> try parseAdditionalAllowed
  where
    parseProperties = many (try Property.parseSpecification)
    parseAdditionalAllowed = option false <<< parseLine 8 $
      parseReservedChunk "additional-properties-allowed" $> true
