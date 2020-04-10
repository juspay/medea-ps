module Data.Medea.JSONType where

import MedeaPrelude
import Data.Argonaut (Json, caseJson)

data JSONType 
  = JSONNull
  | JSONBoolean
  | JSONNumber
  | JSONString
  | JSONArray
  | JSONObject

derive instance eqJSONType :: Eq JSONType

derive instance ordJSONType :: Ord JSONType

derive instance genericJSONType :: Generic JSONType _

instance showJSONType :: Show JSONType where
  show x = genericShow x

typeOf :: Json -> JSONType
typeOf j = caseJson
  (\u -> JSONNull)
  (\b -> JSONBoolean)
  (\n -> JSONNumber)
  (\s -> JSONString)
  (\a -> JSONArray)
  (\o -> JSONObject)
  j
