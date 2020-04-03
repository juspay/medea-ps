module Data.Medea where

import MedeaPrelude
import Control.Comonad.Cofree as Cofree
import Data.Hashable (class Hashable, hash)

-- | The schema-derived information attached to the current node.
data SchemaInformation
  = AnySchema
  | NullSchema
  | BooleanSchema
  | NumberSchema
  | StringSchema
  | ArraySchema
  | ObjectSchema
  | StartSchema
  | UserDefined String

derive instance eqSchemaInformation :: Eq SchemaInformation 

derive instance genericSchemaInformation :: Generic SchemaInformation _

instance showSchemaInformation :: Show SchemaInformation where
  show x = genericShow x

-- TODO instances for Data, NFData for SchemaInformation

instance hashableSchemaInformation :: Hashable SchemaInformation where
  hash AnySchema = 0
  hash NullSchema = 1
  hash BooleanSchema = 2
  hash NumberSchema = 3
  hash StringSchema = 4
  hash ArraySchema = 5
  hash ObjectSchema = 6
  hash StartSchema = 7
  hash (UserDefined s) = 8 + hash (s)

-- | JSON, with additional schema-derived information as an annotation.

-- TODO implement after ValidJSONF

{-- newtype ValidatedJSON = ValidatedJSON (Cofree ValidJSONF SchemaInformation) --}

{-- derive newtype instance eqValidatedJSON :: Eq ValidatedJSON  --}

{-- derive newtype instance showValidatedJSON :: Show ValidatedJSON --}

-- TODO instances for Data, NFData for ValidatedJSON

{-- instance hashableValidatedJSON :: Hashable ValidatedJSON where  --}
  {-- hash =  --}

  -- TODO: add a hashWithSalt method
