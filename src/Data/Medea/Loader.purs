module Data.Medea.Loader where

import MedeaPrelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (runExcept)
import Data.Medea.Schema (Schema, mkSchema)
import Data.Medea.Analysis(AnalysisError(..), intoMap, intoEdges, intoAcyclic, checkStartSchema)
import Data.Medea.Parser.Spec.Schemata as Schemata
import Text.Parsing.Parser (ParseError, runParser)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

data LoaderError
  = -- | The data provided wasn't UTF-8.
    NotUtf8
  | -- | An identifier was longer than allowed.
    IdentifierTooLong
  | -- | A natural number had a leading 0.
    LeadingZeroNatural
  | -- | A length specification had no minimum/maximum specification.
    EmptyLengthSpec
  | -- | Parsing failed.
    ParserError ParseError
  | -- | No schema labelled $start was provided.
    StartSchemaMissing
  | -- | A schema was typed in terms of itself.
    SelfTypingSchema
  | -- | A schema was defined more than once.
    MultipleSchemaDefinition String
  | -- | name of the undefined schema
    MissingSchemaDefinition String
  | -- | A schema with non-start reserved naming identifier.
    SchemaNameReserved String -- name of the reserved identifier
  | -- | There is at least one isolated schema.
    IsolatedSchemata
  | -- | Value for `$object-schema` is an undefined schema.
    MissingPropSchemaDefinition String
  | -- | Minimum length specification was more than maximum.
    MinimumLengthGreaterThanMaximum String -- name of the schema
  | -- | A property specifier section has two properties with the same name.
    -- | Arguments are the parent Schema name and the property name.
    MultiplePropSchemaDefinition String String
  | UnexpectedTypeNodeErr

-- | Attempt to produce a schema from Binary data in memory.


buildSchema :: forall m. MonadError LoaderError m => String -> m Schema
buildSchema utf8   = do
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
fromutf8 sourceName utf8 
  = case runParser utf8 Schemata.parseSpecification of
      Left err -> throwError $ ParserError err
      Right scm -> pure scm

analyze :: forall m. MonadError LoaderError m => Schemata.Specification -> m Schema
analyze scm 
  = case runExcept go of
    Left (UnexpectedTypeNode) -> throwError $ UnexpectedTypeNodeErr
    Left (DuplicateSchemaName ident) -> throwError $ MultipleSchemaDefinition $ show ident
    Left NoStartSchema -> throwError StartSchemaMissing
    Left (DanglingTypeReference ident) -> throwError $ MissingSchemaDefinition $ show ident
    Left TypeRelationIsCyclic -> throwError SelfTypingSchema
    Left (ReservedDefined ident) -> throwError $ SchemaNameReserved $ show ident
    Left UnreachableSchemata -> throwError IsolatedSchemata
    Left (DanglingTypeRefProp ident) -> throwError $ MissingPropSchemaDefinition $ show ident
    Left (MinMoreThanMax ident) -> throwError $ MinimumLengthGreaterThanMaximum $ show ident
    Left (DuplicatePropName ident prop) -> throwError $ MultiplePropSchemaDefinition (show ident) (unwrap prop)
    Right g -> pure g
  where
    go = do
      m <- intoMap scm
      startSchema <- checkStartSchema m
      edges <- intoEdges m startSchema
      tg <- intoAcyclic edges
      pure $ mkSchema tg m
