module Data.Medea.Schema where

import MedeaPrelude
import Data.AcyclicAdjacencyMap (AcyclicAdjacencyMap)
import Data.Medea.Analysis (TypeNode, ReducedSchema)
import Data.Medea.Parser.Primitive (Identifier)

data Schema 
  = Schema 
  { typeGraph :: AcyclicAdjacencyMap TypeNode
  , reduceSpec :: Map Identifier ReducedSchema
  }

mkSchema :: AcyclicAdjacencyMap TypeNode -> Map Identifier ReducedSchema -> Schema
mkSchema typeGraph reduceSpec = Schema { typeGraph, reduceSpec }

derive instance genericSchema :: Generic Schema _

instance showSchema :: Show Schema where
  show x = genericShow x
