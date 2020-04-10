module Data.Medea.Analysis where

import MedeaPrelude
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.State.Class (gets, modify)
import Control.Monad.State.Trans (evalStateT)
import Data.HashMap as HM
import Data.Medea.JSONType (JSONType)
import Data.Medea.Parser.Primitive (Identifier, MedeaString, startIdentifier, isReserved, isStartIdent, tryPrimType, typeOf)
import Data.Medea.Parser.Spec.Array (minLength, maxLength)
import Data.Medea.Parser.Spec.Object (properties, additionalAllowed)
import Data.Medea.Parser.Spec.Property (propName, propSchema, propOptional)
import Data.Medea.Parser.Spec.Schema as Schema
import Data.Medea.Parser.Spec.Schemata as Schemata
import Data.Medea.Parser.Spec.Type as Type
import Data.Natural (Natural)
import Data.AcyclicAdjacencyMap (AcyclicAdjacencyMap)
import Data.AcyclicAdjacencyMap as Acyclic
import Data.AdjacencyMap as Cyclic
import Data.Set as Set
import Data.Map as Map
import Unsafe.Coerce (unsafeCoerce)

data AnalysisError
  = DuplicateSchemaName Identifier
  | NoStartSchema
  | DanglingTypeReference Identifier
  | TypeRelationIsCyclic
  | ReservedDefined Identifier
  | UnreachableSchemata
  | MinMoreThanMax Identifier
  | DanglingTypeRefProp Identifier
  | DuplicatePropName Identifier MedeaString
  | UnexpectedTypeNode

data TypeNode
  = AnyNode
  | PrimitiveNode JSONType
  | CustomNode Identifier

derive instance eqTypeNode :: Eq TypeNode

derive instance ordTypeNode :: Ord TypeNode

derive instance genericTypeNode :: Generic TypeNode _

instance showTypeNode :: Show TypeNode where
  show x = genericShow x

type ReducedTypeSpec = Array TypeNode
type ReducedArraySpec = Tuple (Maybe Natural) (Maybe Natural)
type ReducedObjectSpec = Tuple (HM.HashMap String (Tuple TypeNode Boolean)) Boolean

data ReducedSchema = ReducedSchema {
  reducedTypes :: ReducedTypeSpec,
  reducedArray :: ReducedArraySpec,
  reducedObject :: ReducedObjectSpec
}

mkReducedSchema :: ReducedTypeSpec -> ReducedArraySpec -> ReducedObjectSpec -> ReducedSchema
mkReducedSchema rt ra ro 
  = ReducedSchema 
  { reducedTypes: rt
  , reducedArray: ra
  , reducedObject: ro
  }
-- simple getters to mimic haskell record getters
reducedObject :: ReducedSchema -> ReducedObjectSpec
reducedObject (ReducedSchema { reducedObject:ro }) = ro

reducedArray :: ReducedSchema -> ReducedArraySpec
reducedArray (ReducedSchema { reducedArray:ra }) = ra

reducedType :: ReducedSchema -> ReducedTypeSpec
reducedType (ReducedSchema { reducedTypes: rt }) = rt

derive instance genericReducedSchema :: Generic ReducedSchema _

instance showReducedSchema :: Show ReducedSchema where
  show x = genericShow x

intoAcyclic :: forall m. MonadError AnalysisError m => Array (Tuple TypeNode TypeNode) -> m (AcyclicAdjacencyMap TypeNode)
intoAcyclic = maybe (throwError TypeRelationIsCyclic) pure <<< Acyclic.toAcyclic <<< Cyclic.edges

intoEdges :: forall m. Monad m => MonadError AnalysisError m => Map Identifier ReducedSchema -> ReducedSchema -> m (Array (Tuple TypeNode TypeNode))
intoEdges mir redScm = evalStateT (go [] redScm startNode <* checkUnusedSchema <* checkUndefinedPropSchema) Set.empty
  where
    startNode = CustomNode startIdentifier 
    checkUnusedSchema = do
       reachableSchemas <- gets Set.size
       when (reachableSchemas < Map.size mir) $ throwError UnreachableSchemata
    checkUndefinedPropSchema =
      let result = filter isUndefinedNode <<< map fst <<< HM.values <<< fst <<< reducedObject $ redScm
      in case uncons result of
        Nothing -> pure unit
        Just { head: (CustomNode ident), tail } -> throwError $ DanglingTypeRefProp ident
        Just _ -> pure unit
    isUndefinedNode (CustomNode ident) = isNothing <<< Map.lookup ident $ mir
    isUndefinedNode _ = false
    go acc scm node = do
      alreadySeen <- gets (Set.member node)
      if alreadySeen
        then pure acc
        else do
           _ <- modify (Set.insert node)
           traverseRefs acc node $ reducedType scm
    traverseRefs acc node [] = pure $ cons (Tuple node AnyNode) acc
    traverseRefs acc node refs =
      (acc <> _) <<< concat <$> traverse (resolveLinks node) refs
      -- Note: t cannot be AnyNode
    resolveLinks u t 
      = case t of
        PrimitiveNode _ -> pure <<< pure $ Tuple u t
        CustomNode ident -> case Map.lookup ident mir of
          Nothing -> throwError <<< DanglingTypeReference $ ident
          Just scm -> cons <$> pure (Tuple u t) <*> go [] scm t
        AnyNode -> throwError $ UnexpectedTypeNode 

-- alterF is not part of the PS Map library
mapAlterF :: forall f k a. Functor f => Ord k => (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)
mapAlterF f k mka = update <$> ma
  where
    update (Nothing) = Map.delete k mka
    update (Just a) = Map.insert k a mka
    ma = f $ Map.lookup k mka

hashMapAlterF :: forall f k a. Functor f => Hashable k => (Maybe a -> f (Maybe a)) -> k -> HashMap k a -> f (HashMap k a)
hashMapAlterF f k mka = update <$> ma
  where
    update (Nothing) = HM.delete k mka
    update (Just a) = HM.insert k a mka
    ma = f $ HM.lookup k mka

-- TODO checkStartSchema, intoMap
intoMap :: forall m. MonadError AnalysisError m => Schemata.Specification -> m (Map Identifier ReducedSchema)
intoMap (Schemata.Specification arr) = foldM go Map.empty arr
  where
    go acc spec = mapAlterF (checkedInsert spec) (Schema.name spec) acc
    checkedInsert spec 
      = case _ of
        Nothing -> do
          when (isReserved ident && (not <<< isStartIdent) ident)
            $ throwError <<< ReservedDefined $ ident
          Just <$> reduceSchema spec
        Just _ -> throwError <<< DuplicateSchemaName $ ident
        where
          ident = Schema.name spec

reduceSchema :: forall m. MonadError AnalysisError m => Schema.Specification -> m ReducedSchema
reduceSchema scm 
  = do
    let reducedArraySpec = unsafeCoerce (Tuple  (minLength arraySpec) (maxLength arraySpec))
        typenodes = map (identToNode <<< Just) types
    reducedProps <- foldM go HM.empty (properties objSpec)
    when (uncurry (>) reducedArraySpec) 
      $ throwError $ MinMoreThanMax schemaName 
    pure $ mkReducedSchema typenodes reducedArraySpec (Tuple reducedProps (additionalAllowed objSpec))
  where
    Schema.Specification { name: schemaName, types: (Type.Specification types), array: arraySpec, object: objSpec } = scm
    go acc prop = hashMapAlterF (checkedInsert prop) (unsafeCoerce propName prop) acc
    checkedInsert prop 
      = case _ of
        Nothing -> pure $ Just $ (Tuple (identToNode (propSchema prop)) (propOptional prop))
        Just _ -> throwError $ DuplicatePropName schemaName (propName prop)
                              

identToNode :: Maybe Identifier -> TypeNode
identToNode = case _ of
  Nothing -> AnyNode
  Just t -> maybe (CustomNode t) (PrimitiveNode <<< typeOf) $ tryPrimType t

checkStartSchema :: forall m. MonadError AnalysisError m => Map Identifier ReducedSchema -> m ReducedSchema
checkStartSchema mir = case Map.lookup startIdentifier mir of
  Nothing -> throwError NoStartSchema
  Just scm -> pure scm
