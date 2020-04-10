module Data.Medea.Schema where

import MedeaPrelude
import Data.AcyclicAdjacencyMap (AcyclicAdjacencyMap)
import Data.Medea.Analysis (TypeNode, ReducedSchema)
import Data.Medea.Parser.Primitive (Identifier)

data Schema 
  = Schema 
  { typeGraph :: AcyclicAdjacencyMap TypeNode
  , reducedSpec :: Map Identifier ReducedSchema
  }

typeGraph :: Schema -> AcyclicAdjacencyMap TypeNode
typeGraph (Schema {typeGraph: tg}) = tg 

reducedSpec :: Schema -> Map Identifier ReducedSchema
reducedSpec (Schema {reducedSpec: rs}) = rs 

mkSchema :: AcyclicAdjacencyMap TypeNode -> Map Identifier ReducedSchema -> Schema
mkSchema typeGraph reducedSpec = Schema { typeGraph, reducedSpec }

derive instance genericSchema :: Generic Schema _

instance showSchema :: Show Schema where
  show x = genericShow x
