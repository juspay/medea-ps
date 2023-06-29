module Data.Medea.Parser.Spec.Type
  ( Specification(..)
  , defaultSpec
  , parseSpecification
  ) where

import MedeaPrelude
import Data.Medea.Parser.Primitive (Identifier, ReservedIdentifier(..), parseLine, parseReserved, parseIdentifier)
import Data.Medea.Parser.Types (MedeaParser)
import Parsing.Combinators (try)

newtype Specification
  = Specification (Array Identifier)

derive instance eqSpecification :: Eq Specification

derive instance newtypeSpecification :: Newtype Specification _

defaultSpec :: Specification
defaultSpec = Specification []

getReferences :: Specification -> Array Identifier
getReferences = unwrap

parseSpecification :: MedeaParser Specification
parseSpecification = do
  _ <- parseLine 4 $ parseReserved RType
  types <- some <<< try $ parseLine 8 parseIdentifier
  pure <<< Specification $ types
