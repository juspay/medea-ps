module Data.Medea.Loader
  ( LoaderError(..)
  , buildSchema
  , loadSchemaFromFile
  ) where

import MedeaPrelude
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (runExcept)
import Data.Medea.Schema (Schema(..))
import Data.Medea.Analysis (AnalysisError(..), compileSchemata)
import Data.Medea.Parser.Spec.Schemata as Schemata
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (ParseError, runParser)

data LoaderError
  -- | The data provided wasn't UTF-8.
  = NotUtf8
  -- | An identifier was longer than allowed.
  | IdentifierTooLong
  -- | A length specification had no minimum/maximum specification.
  | EmptyLengthSpec
  -- | Parsing failed.
  | ParserError ParseError
  -- | No schema labelled $start was provided.
  | StartSchemaMissing
  -- | A schema was typed in terms of itself.
  | SelfTypingSchema
  -- | A schema was defined more than once.
  | MultipleSchemaDefinition String
  -- | name of the undefined schema  and the schema that references it.
  | MissingSchemaDefinition String String
  -- | A schema with non-start reserved naming identifier.
  | SchemaNameReserved String -- name of the reserved identifier
  -- | An isolated schema was found.
  | IsolatedSchemata String
  -- | name of undefined property schema and the schema that references it
  | MissingPropSchemaDefinition String String
  -- | name of undefined list element type and the schema that references it.
  | MissingListSchemaDefinition String String
  -- | name of the underfined tuple positional schema and the schema that references it
  | MissingTupleSchemaDefinition String String
  -- | Minimum length specification was more than maximum.
  | MinimumLengthGreaterThanMaximum String -- name of the schema
  -- | A property specifier section has two properties with the same name.
  -- | Arguments are the parent Schema name and the property name.
  | MultiplePropSchemaDefinition String String
  | UnexpectedTypeNodeErr
  -- | Schema has a Property specification but no $object type
  | PropertySpecWithoutObjectType String
  -- | Schema has a List specification but no $arry type
  | ListSpecWithoutArrayType String
  -- | Schema has a Tuple specification but no $array type
  | TupleSpecWithoutArrayType String
  -- | Schema has a String specification but no $string type
  | StringSpecWithoutStringType String

derive instance genericLoaderError :: Generic LoaderError _

instance showLoaderError :: Show LoaderError where
  show x = genericShow x

-- | Attempt to produce a schema from Binary data in memory.
buildSchema :: forall m. MonadError LoaderError m => String -> m Schema
buildSchema utf8 = do
  -- parseUtf8 not necessary since JS strings are utf8 by default
  spec <- fromutf8 ":memory:" utf8
  analyze spec

loadSchemaFromFile :: forall m. MonadAff m => MonadError LoaderError m => String -> m Schema
loadSchemaFromFile fp = do
  contents <- liftAff <<< readTextFile UTF8 $ fp
  spec <- fromutf8 fp contents
  analyze spec

-- `loadSchemaFromHandle` is not ported as Purescript's filesystem bindings don't have the notion of a first class file handle
fromutf8 :: forall m. MonadError LoaderError m => String -> String -> m Schemata.Specification
fromutf8 sourceName utf8 = case runParser utf8 Schemata.parseSpecification of
  Left err -> throwError $ ParserError err
  Right scm -> pure scm

analyze :: forall m. MonadError LoaderError m => Schemata.Specification -> m Schema
analyze scm = case runExcept $ compileSchemata scm of
  Left (DuplicateSchemaName ident) -> throwError $ MultipleSchemaDefinition (unwrap ident)
  Left (UnexpectedTypeNode) -> throwError $ UnexpectedTypeNodeErr
  Left NoStartSchema -> throwError StartSchemaMissing
  Left (DanglingTypeReference danglingRef parSchema) -> throwMissing danglingRef parSchema
  Left (DanglingTypeRefProp danglingRef parSchema) -> throwMissing danglingRef parSchema
  Left (DanglingTypeRefList danglingRef parSchema) -> throwMissing danglingRef parSchema
  Left (DanglingTypeRefTuple danglingRef parSchema) -> throwMissing danglingRef parSchema
  Left TypeRelationIsCyclic -> throwError SelfTypingSchema
  Left (ReservedDefined ident) -> throwError $ SchemaNameReserved $ unwrap ident
  Left (DefinedButNotUsed ident) -> throwError $ IsolatedSchemata $ unwrap ident
  Left (MinMoreThanMax ident) -> throwError $ MinimumLengthGreaterThanMaximum $ unwrap ident
  Left (DuplicatePropName ident prop) -> throwError $ MultiplePropSchemaDefinition (unwrap ident) (unwrap prop)
  Left (PropertyWithoutObject schema) -> throwError $ PropertySpecWithoutObjectType (unwrap schema)
  Left (ListWithoutArray schema) -> throwError $ ListSpecWithoutArrayType (unwrap schema)
  Left (TupleWithoutArray schema) -> throwError $ TupleSpecWithoutArrayType (unwrap schema)
  Left (StringValsWithoutString schema) -> throwError $ StringSpecWithoutStringType (unwrap schema)
  Right g -> pure $ Schema g
  where
  throwMissing danglingRef parSchema = throwError $ MissingSchemaDefinition (unwrap danglingRef) (unwrap parSchema)
