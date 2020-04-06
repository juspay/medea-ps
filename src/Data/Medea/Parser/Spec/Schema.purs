module Data.Medea.Parser.Spec.Schema where

import MedeaPrelude
import Data.Medea.Parser.Permutation (runPermutation, toPermutationWithDefault)

import Data.Medea.Parser.Types (MedeaParser)
import Data.Medea.Parser.Primitive (Identifier, parseKeyVal, parseIdentifier, parseLine)
import Data.Medea.Parser.Spec.Array as Array
import Data.Medea.Parser.Spec.Object as Object
import Data.Medea.Parser.Spec.Type as Type
import Text.Parsing.Parser.Combinators (try)

data Specification 
  = Specification 
  { name :: Identifier
  , types :: Type.Specification
  , array :: Array.Specification
  , object :: Object.Specification
  }

mkSpec :: Identifier -> Type.Specification -> Array.Specification -> Object.Specification -> Specification
mkSpec name types array object 
  = Specification  
  { name
  , types
  , array
  , object
  }

derive instance eqSpecification :: Eq Specification

parseSpecification :: MedeaParser Specification
parseSpecification
  = do
     schemaName <- parseLine 0 $ parseKeyVal "schema" parseIdentifier
     runPermutation $ mkSpec schemaName
       <$> toPermutationWithDefault Type.defaultSpec (try Type.parseSpecification)
       <*> toPermutationWithDefault Array.defaultSpec (try Array.parseSpecification)
       <*> toPermutationWithDefault Object.defaultSpec (try Object.parseSpecification)

