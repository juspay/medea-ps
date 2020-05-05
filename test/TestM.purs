module TestM where

import MedeaPrelude
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Const (Const)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)

import Data.Medea.Loader (LoaderError(..))

import Mote (MoteT)

newtype TestM a = TestM (ExceptT LoaderError Aff a)

derive newtype instance functorTestM :: Functor TestM

derive newtype instance applyTestM :: Apply TestM

derive newtype instance applicativeTestM :: Applicative TestM

derive newtype instance bindTestM :: Bind TestM

derive newtype instance monadTestM :: Monad  TestM

derive newtype instance monadThrowTestM :: MonadThrow LoaderError TestM

derive newtype instance monadErrorTestM :: MonadError LoaderError TestM

derive newtype instance monadEffectTestM :: MonadEffect TestM

derive newtype instance monadAffTestM :: MonadAff TestM

type TestPlanM a = MoteT (Const Void) (Aff Unit) Aff Unit

runTestM :: forall  a. TestM a -> Aff (Either LoaderError a)
runTestM (TestM comp) = runExceptT comp

isParseError :: forall a. Either LoaderError a -> Boolean
isParseError (Left NotUtf8) = true
isParseError (Left IdentifierTooLong) = true
isParseError (Left (ParserError _)) = true
isParseError _  = false


isSchemaError :: forall a. Either LoaderError a -> Boolean
isSchemaError (Left StartSchemaMissing) = true
isSchemaError (Left SelfTypingSchema) = true
isSchemaError (Left (MultipleSchemaDefinition _)) = true
isSchemaError (Left (MissingSchemaDefinition _)) = true
isSchemaError (Left (SchemaNameReserved _)) = true
isSchemaError (Left IsolatedSchemata) = true
isSchemaError (Left (MissingPropSchemaDefinition _)) = true
isSchemaError (Left (MinimumLengthGreaterThanMaximum _)) = true
isSchemaError (Left (MultiplePropSchemaDefinition _ _)) = true
isSchemaError _ = false
