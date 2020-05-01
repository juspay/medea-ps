module Data.Medea.Analysis where

import MedeaPrelude
import Control.Alternative ((<|>))
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.HashMap as HM
import Data.Medea.JSONType (JSONType(..))
import Data.Medea.Parser.Primitive (Identifier, MedeaString, startIdentifier, isReserved, isStartIdent, tryPrimType, typeOf)
import Data.Medea.Parser.Spec.Array as Array
import Data.Medea.Parser.Spec.Object (properties, additionalAllowed)
import Data.Medea.Parser.Spec.Property (propName, propSchema, propOptional)
import Data.Medea.Parser.Spec.Schema as Schema
import Data.Medea.Parser.Spec.Schemata as Schemata
import Data.Medea.Parser.Spec.Type as Type
import Data.Medea.Parser.Spec.String as String
import Data.Natural (Natural, intToNat)
import Data.NonEmpty (NonEmpty, fromNonEmpty, (:|))
import Data.AcyclicAdjacencyMap as Acyclic
import Data.AdjacencyMap as Cyclic
import Data.Set as Set
import Data.Map as Map
import Unsafe.Coerce (unsafeCoerce)


data AnalysisError
  = DuplicateSchemaName Identifier
  | NoStartSchema
  | DanglingTypeReference Identifier Identifier
  | TypeRelationIsCyclic
  | ReservedDefined Identifier
  | DefinedButNotUsed Identifier
  | MinMoreThanMax Identifier
  | DanglingTypeRefProp Identifier Identifier
  | DanglingTypeRefList Identifier Identifier
  | DanglingTypeRefTuple Identifier Identifier
  | PropertyWithoutObject Identifier
  | ListWithoutArray Identifier
  | TupleWithoutArray Identifier
  | StringValsWithoutString Identifier 
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

data CompiledSchema 
  = CompiledSchema 
    { schemaNode :: TypeNode
    , typesAs :: NonEmpty Set TypeNode
    , minListLen :: Maybe Natural
    , maxListLen :: Maybe Natural
    , props :: HashMap String (Tuple TypeNode Boolean)
    , additionalProps :: Boolean
    , arrayTypes :: Maybe ArrayType
    , stringVals :: Array String
    }

derive instance genericCompiledSchema :: Generic CompiledSchema _

instance showCompiledSchema :: Show CompiledSchema where
  show x = genericShow x

data ArrayType = ListType TypeNode | TupleType (Array TypeNode)

derive instance genericArrayType :: Generic ArrayType _

instance showArrayType :: Show ArrayType where
  show x = genericShow x

checkAcyclic :: forall m. MonadError AnalysisError m => Map Identifier CompiledSchema -> m Unit
checkAcyclic m = when (isNothing <<< Acyclic.toAcyclic <<< getTypesAsGraph $ m) 
  $ throwError TypeRelationIsCyclic

compileSchemata :: forall m. MonadError AnalysisError m => Schemata.Specification-> m (Map Identifier CompiledSchema)
compileSchemata (Schemata.Specification v) = do
  m <- foldM go Map.empty v
  checkStartSchema m
  checkDanglingReferences getTypeRefs DanglingTypeReference m
  checkDanglingReferences getPropertyTypeRefs DanglingTypeRefProp m
  checkDanglingReferences getListTypeRefs DanglingTypeRefList m
  checkDanglingReferences getTupleTypeRefs DanglingTypeRefTuple m
  checkUnusedSchemata m
  checkAcyclic m
  pure m
  where
    go acc spec = mapAlterF (checkedInsert spec) (Schema.name spec) acc
    checkedInsert spec = case _ of
      Nothing -> Just <$> compileSchema spec
      Just _ -> throwError $ DuplicateSchemaName $ (Schema.name spec)


compileSchema :: forall m. MonadError AnalysisError m => Schema.Specification -> m CompiledSchema
compileSchema scm@(Schema.Specification { name: schemaName,  types: (Type.Specification types),  stringVals: stringValsSpec, array: (Array.Specification arraySpec), object: objSpec }) 
  = do
    when (isReserved schemaName && (not <<< isStartIdent) schemaName)
      $ throwError $ ReservedDefined $ schemaName
    let minListLen = arraySpec.minLength
        maxListLen = arraySpec.maxLength
    when (minListLen > maxListLen && (not $ isNothing maxListLen))
      $ throwError $ MinMoreThanMax schemaName
    propMap <- foldM go HM.empty (maybe [] properties objSpec)
    let arrType = getArrayTypes (arraySpec.elementType) (arraySpec.tupleSpec)
        tupleLen = getTupleTypeLen arrType
        hasPropSpec = isJust objSpec
        compiledScm = CompiledSchema $
          { schemaNode: identToNode <<< Just $ schemaName
          , typesAs: defaultToAny <<< map (identToNode <<< Just) $ types
          , minListLen: minListLen <|> tupleLen
          , maxListLen: maxListLen <|> tupleLen
          , arrayTypes: arrType
          , props: propMap
          , additionalProps: maybe true additionalAllowed objSpec
          , stringVals: String.toReducedSpec stringValsSpec
          }
    when (shouldNotHavePropertySpec compiledScm hasPropSpec) $
      throwError $ PropertyWithoutObject $ schemaName
    when (shouldNotHaveListSpec compiledScm) $
      throwError $ ListWithoutArray $ schemaName
    when (shouldNotHaveTupleSpec compiledScm) $
      throwError $ TupleWithoutArray $ schemaName
    when (shouldNotHaveStringSpec compiledScm) $ 
      throwError $ StringValsWithoutString $ schemaName
    pure compiledScm
  where
    go acc prop = hashMapAlterF (checkedInsert prop) (unsafeCoerce $ propName prop) acc
    checkedInsert prop = case _ of
      Nothing -> pure <<< Just $ (Tuple (identToNode (propSchema prop)) (propOptional prop))
      Just _ -> throwError $ DuplicatePropName schemaName (propName prop)
    defaultToAny :: Array TypeNode -> NonEmpty Set TypeNode
    defaultToAny xs = case uncons xs of
      Nothing -> AnyNode :| Set.empty
      Just {head, tail} -> head :| Set.fromFoldable tail

checkStartSchema :: forall m. MonadError AnalysisError m => Map Identifier CompiledSchema -> m Unit
checkStartSchema mic = case Map.lookup startIdentifier mic of
  Nothing -> throwError NoStartSchema
  Just scm -> pure unit

-- we need a `getRefs` argument here so that we can differentiate between
-- different kinds of dangling references (types/props/lists/tuples)
checkDanglingReferences :: forall m. MonadError AnalysisError m => (CompiledSchema -> Array TypeNode) -> (Identifier -> Identifier -> AnalysisError) -> Map Identifier CompiledSchema -> m Unit
checkDanglingReferences getRefs err m = traverse_ go <<< array <<< Map.toUnfoldable $ m
  where
    go (Tuple schemaName scm) 
      = case uncons $ getDanglingRefs scm of
        Nothing -> pure unit
        Just { head: danglingRef } -> throwError $ err danglingRef schemaName
    getDanglingRefs = filter isUndefined <<< mapMaybe fromCustomNode <<< getRefs
    isUndefined ident = isNothing <<< Map.lookup ident $ m
    fromCustomNode (CustomNode ident) = Just ident
    fromCustomNode _ = Nothing
    -- toUnfoldable can't infer Array
    array :: forall a. Array a -> Array a
    array = identity

checkUnusedSchemata :: forall m. MonadError AnalysisError m => Map Identifier CompiledSchema -> m Unit
checkUnusedSchemata m = traverse_ checkUnused <<< Map.keys $ m
  where
    checkUnused ident
      | Set.member (CustomNode ident) allReferences = pure unit
      | isStartIdent ident = pure unit
      | otherwise = throwError $ DefinedButNotUsed ident
    allReferences = Set.unions <<< map getReferences <<< Map.values $ m
    getReferences scm  = Set.fromFoldable $ 
      getTypeRefs scm <> getPropertyTypeRefs scm <> getListTypeRefs scm <> getTupleTypeRefs scm

-- Helpers
identToNode :: Maybe Identifier -> TypeNode
identToNode = case _ of
  Nothing -> AnyNode
  Just t -> maybe (CustomNode t) (PrimitiveNode <<< typeOf) $ tryPrimType t

getTypeRefs :: CompiledSchema -> Array TypeNode
getTypeRefs (CompiledSchema cs) = fromFoldable $ fromNonEmpty Set.insert $ cs.typesAs

getPropertyTypeRefs :: CompiledSchema -> Array TypeNode
getPropertyTypeRefs (CompiledSchema cs) = map fst $ HM.values $ cs.props

getTypesAsGraph :: Map Identifier CompiledSchema -> Cyclic.AdjacencyMap TypeNode
getTypesAsGraph = Cyclic.edges <<< concatMap intoTypesAsEdges <<< fromFoldable <<< Map.values

intoTypesAsEdges :: CompiledSchema -> Array (Tuple TypeNode TypeNode)
intoTypesAsEdges (CompiledSchema cs) = map (Tuple (cs.schemaNode)) <<< fromFoldable <<< fromNonEmpty Set.insert $ cs.typesAs

getListTypeRefs :: CompiledSchema -> Array TypeNode
getListTypeRefs (CompiledSchema cs) =  case cs.arrayTypes of
  Just (ListType typeNode) -> [typeNode]
  _ -> []

getTupleTypeRefs :: CompiledSchema -> Array TypeNode
getTupleTypeRefs scm@(CompiledSchema cs) = case cs.arrayTypes of
  Just (TupleType ts) -> ts
  _ -> []

getArrayTypes :: Maybe Identifier -> Maybe (Array Identifier) -> Maybe ArrayType
getArrayTypes Nothing Nothing = Nothing
getArrayTypes (Just ident) _ = Just <<< ListType <<< identToNode <<< Just $ ident
getArrayTypes _ (Just idents) = Just <<< TupleType $ identToNode <<< Just <$> idents

getTupleTypeLen :: Maybe ArrayType -> Maybe Natural
getTupleTypeLen (Just (TupleType types)) = Just <<< intToNat <<< length $ types
getTupleTypeLen _ = Nothing

mapAlterF :: forall f a k. Functor f => Ord k => (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)
mapAlterF f k m = 
  let mv = Map.lookup k m 
  in (_ <$> f mv) 
    $ \fres -> case fres of
      Nothing -> Map.delete k m
      Just v' -> Map.insert k v' m

hashMapAlterF :: forall f k v. Functor f => Eq k => Hashable k => (Maybe v -> f (Maybe v)) -> k -> HashMap k v -> f (HashMap k v)
hashMapAlterF f k m =
  let mv = HM.lookup k m
  in (_ <$> f mv) 
    $ \fres -> case fres of
      Nothing -> HM.delete k m
      Just v' -> HM.insert k v' m

arrayNode :: TypeNode 
arrayNode = PrimitiveNode JSONArray

objectNode :: TypeNode
objectNode = PrimitiveNode JSONObject

stringNode :: TypeNode
stringNode = PrimitiveNode JSONString

hasListSpec :: CompiledSchema -> Boolean
hasListSpec (CompiledSchema cs) 
  = case cs.arrayTypes of
    Just (ListType _) -> true
    Just (TupleType _) -> false
    _ -> isJust $ cs.minListLen <|> cs.maxListLen

hasTupleSpec :: CompiledSchema -> Boolean
hasTupleSpec (CompiledSchema cs) 
  = case cs.arrayTypes of
    Just (TupleType _) -> true
    _ -> false

isNodeType :: TypeNode -> CompiledSchema -> Boolean
isNodeType node scm@(CompiledSchema cs) = Set.member node $ fromNonEmpty Set.insert $ cs.typesAs

hasStringSpec :: CompiledSchema -> Boolean
hasStringSpec (CompiledSchema cs) = not $ null $ cs.stringVals

shouldNotHavePropertySpec :: CompiledSchema -> Boolean -> Boolean
shouldNotHavePropertySpec scm hasPropSpec = hasPropSpec && (not $ isNodeType objectNode scm)

shouldNotHaveListSpec :: CompiledSchema -> Boolean
shouldNotHaveListSpec scm = hasListSpec scm && (not $ isNodeType arrayNode scm)

shouldNotHaveTupleSpec :: CompiledSchema -> Boolean
shouldNotHaveTupleSpec scm = hasTupleSpec scm && (not $ isNodeType arrayNode scm)

shouldNotHaveStringSpec :: CompiledSchema -> Boolean
shouldNotHaveStringSpec scm = hasStringSpec scm && (not $ isNodeType stringNode scm)
