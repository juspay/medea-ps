module Data.Medea.Schema
  ( Schema(..)
  ) where

import MedeaPrelude
import Data.Medea.Analysis (CompiledSchema)
import Data.Medea.Parser.Primitive (Identifier)

newtype Schema
  = Schema (Map Identifier CompiledSchema)

derive instance genericSchema :: Generic Schema _

derive instance newtypeSchema :: Newtype Schema _

instance showSchema :: Show Schema where
  show x = genericShow x
