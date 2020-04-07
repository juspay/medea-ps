module Data.Medea.Parser.Spec.Type where

import MedeaPrelude
import Text.Parsing.Parser.Combinators (try)
import Data.Medea.Parser.Primitive (Identifier, parseLine, parseReservedChunk, parseIdentifier)
import Data.Medea.Parser.Types (MedeaParser)

newtype Specification = Specification (Array Identifier)

derive instance eqSpecification :: Eq Specification

derive instance newtypeSpecification :: Newtype Specification _

defaultSpec :: Specification
defaultSpec = Specification []


getReferences :: Specification -> Array Identifier
getReferences = unwrap

parseSpecification :: MedeaParser Specification 
parseSpecification = do
  _ <- parseLine 4 $ parseReservedChunk "type"
  types <- some <<< try $ parseLine 8 parseIdentifier
  pure <<< Specification $ types
