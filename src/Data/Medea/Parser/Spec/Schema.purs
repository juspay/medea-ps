module Data.Medea.Parser.Spec.Schema
  ( Specification(..)
  , parseSpecification
  , mkSpec
  , name
  ) where

import MedeaPrelude
import Data.Medea.Parser.Permutation (runPermutation, toPermutationWithDefault)
import Data.Medea.Parser.Types (MedeaParser)
import Data.Medea.Parser.Primitive (Identifier, ReservedIdentifier(..), parseKeyVal, parseIdentifier, parseLine)
import Data.Medea.Parser.Spec.Array as Array
import Data.Medea.Parser.Spec.Object as Object
import Data.Medea.Parser.Spec.Type as Type
import Data.Medea.Parser.Spec.String as String
import Parsing.Combinators (try)

data Specification
  = Specification
    { name :: Identifier
    , types :: Type.Specification
    , stringVals :: String.Specification
    , array :: Array.Specification
    , object :: Maybe Object.Specification
    }

name :: Specification -> Identifier
name (Specification { name: n }) = n

mkSpec :: Identifier -> Type.Specification -> String.Specification -> Array.Specification -> Maybe Object.Specification -> Specification
mkSpec n t sv a o =
  Specification
    { name: n
    , types: t
    , stringVals: sv
    , array: a
    , object: o
    }

derive instance eqSpecification :: Eq Specification

parseSpecification :: MedeaParser Specification
parseSpecification = do
  schemaName <- parseLine 0 $ parseKeyVal RSchema parseIdentifier
  runPermutation $ mkSpec schemaName
    <$> toPermutationWithDefault Type.defaultSpec (try Type.parseSpecification)
    <*> toPermutationWithDefault String.defaultSpec (try String.parseSpecification)
    <*> toPermutationWithDefault Array.defaultSpec (try Array.parseSpecification)
    <*> toPermutationWithDefault Nothing (Just <$> try Object.parseSpecification)
