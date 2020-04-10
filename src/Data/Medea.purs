module Data.Medea where

import MedeaPrelude
import Control.Alternative (class Alternative, (<|>), empty)
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree as Cofree
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Reader.Class (class MonadReader, asks)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.State.Class (class MonadState, gets, put)
import Control.Monad.State.Trans (evalStateT)
import Control.MonadPlus (class MonadPlus)
import Data.AcyclicAdjacencyMap (postSet)
import Data.AdjacencyMap (catMaybeHashMap)
import Data.Argonaut (Json, encodeJson, jsonNull, jsonFalse, jsonTrue, jsonEmptyObject, caseJsonObject, fromObject, jsonParser, caseJson)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Hashable (class Hashable, hash)
import Data.HashMap as HM
import Data.Map as Map
import Data.Medea.Analysis (TypeNode (..), ReducedSchema(..), reducedArray, reducedObject)
import Data.Medea.JSONType (JSONType(..), typeOf)
import Data.Medea.Loader (LoaderError(..), buildSchema) --, loadSchemaFromFile, loadSchemaFromHandle)
import Data.Medea.MedeaJSON (MJSON(..))
import Data.Medea.Parser.Primitive (Identifier(..), startIdentifier)
import Data.Medea.Schema (Schema(..), reducedSpec, typeGraph)
import Data.Medea.ValidJSON (ValidJSONF(..), objectToHashMap, hashmapToObject)
import Data.Natural (natToInt)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NonEmpty
import Data.Set as Set
import Data.These (These(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

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

newtype ValidatedJSON = ValidatedJSON (Cofree ValidJSONF SchemaInformation)

derive newtype instance eqValidatedJSON :: Eq ValidatedJSON 

derive instance genericValidatedJSON :: Generic ValidatedJSON _

instance showValidatedJSON :: Show ValidatedJSON where
  show (ValidatedJSON cof) = "Cofree ( " <> showed <> " )"
    where showed = show $ extract cof

instance hashableValidatedJSON :: Hashable ValidatedJSON where
  hash (ValidatedJSON cof) = hash $ extract cof

toJSONValue :: ValidatedJSON -> Json
toJSONValue (ValidatedJSON cof) 
  = let f = Cofree.tail cof
    in case f of
      AnythingF v -> unwrap v
      NullF -> jsonNull
      BooleanF b -> if b then jsonTrue else jsonFalse
      NumberF n -> encodeJson n
      StringF s -> encodeJson s
      ArrayF a -> encodeJson $ map (toJSONValue <<< unsafeCoerce) $ a
      ObjectF hm -> hashmapEncodeJson $ map (toJSONValue <<< unsafeCoerce) $ hm

hashmapEncodeJson :: HashMap String Json -> Json
hashmapEncodeJson hsj 
  = foldlWithIndex go jsonEmptyObject hsj
  where
    go :: String -> Json -> Json -> Json
    go k v acc 
      = fromObject $ caseJsonObject Object.empty ins acc
      where
        ins o = Object.insert k v o

validAgainst :: ValidatedJSON -> SchemaInformation
validAgainst (ValidatedJSON cof) = extract cof 

data ValidationError
  = EmptyError
  |  -- | We could not parse JSON out of what we were provided.
    NotJSON
  | -- | We got a type different to what we expected.
    WrongType Json JSONType
  | -- | We expected one of several possibilities, but got something that fits
    -- none.
    NotOneOfOptions Json
  | AdditionalPropFoundButBanned String String
  | RequiredPropertyIsMissing String String
  | OutOfBoundsArrayLength String Json

derive instance eqValidationError :: Eq ValidationError

instance semigroupValidationError :: Semigroup ValidationError where
  append EmptyError x = x
  append x _ = x

instance monoidValidationError :: Monoid ValidationError where
  mempty = EmptyError

decode = hush <<< jsonParser

validate :: forall m. MonadPlus m => MonadError ValidationError m => Schema -> String -> m ValidatedJSON
validate scm str 
  = case decode str of
    Nothing -> throwError $ NotJSON
    Just v -> ValidatedJSON <$> go v
  where
    go v = runReaderT (evalStateT (checkTypes v) (Tuple initialSet Nothing)) scm
    initialSet = (_ :| Set.empty) <<< CustomNode $ startIdentifier

validateFromFile :: forall m. MonadPlus m => MonadError ValidationError m => MonadAff m => Schema -> String -> m ValidatedJSON
validateFromFile scm fp = do
  contents <- liftAff $ readTextFile UTF8 fp
  validate scm fp

-- validateFromHandle not implmented here because the ps node filesystem implementation doesn't really deal with raw handles

checkTypes :: forall m. Alternative m => MonadReader Schema m => MonadState (Tuple (NonEmpty Set TypeNode) (Maybe Identifier)) m => MonadError ValidationError m => Json -> m (Cofree ValidJSONF SchemaInformation)
checkTypes v = checkAny v <|> checkPrim v <|> checkCustoms v

checkAny :: forall m. Alternative m => MonadState (Tuple (NonEmpty Set TypeNode) (Maybe Identifier)) m => MonadError ValidationError m => Json -> m (Cofree ValidJSONF SchemaInformation)
checkAny v = do
  minNode <- gets $ Set.findMin <<< (NonEmpty.fromNonEmpty Set.insert)<<< fst -- AnyNode is the smallest possible TypeNode
  case minNode of
    Just AnyNode -> pure $ AnySchema :< (AnythingF $ MJSON v)
    _ -> throwError EmptyError

checkPrim :: forall  m.  Alternative m => MonadReader Schema m => MonadState (Tuple (NonEmpty Set TypeNode) (Maybe Identifier)) m => MonadError ValidationError m => Json -> m (Cofree ValidJSONF SchemaInformation)
checkPrim v = do
  (Tuple neNodes par) <- gets identity
  let nodes = NonEmpty.fromNonEmpty (Set.insert) neNodes
  unless (Set.member (PrimitiveNode <<< typeOf $ v) nodes) $ throwError $ NotOneOfOptions $ v
  caseJson
    (\null -> pure $ NullSchema :< NullF)
    (\bool -> pure $ BooleanSchema :< BooleanF bool)
    (\num -> pure $ NumberSchema :< NumberF num)
    (\str -> pure $ StringSchema :< StringF str)
    (\arr -> do
      case par of 
        Nothing -> pure unit
        Just parIdent -> checkArray arr parIdent
          -- we currently don't check array contents, 
          -- therefore we punt to AnyNode before we carry on
      put (Tuple anySet Nothing)
      (ArraySchema :< _ ) <<< ArrayF <$> traverse checkTypes arr
    )
    (\o -> 
      let obj = objectToHashMap o in
      case par of
      -- Fast Path (no object spec)
      Nothing -> do
        put (Tuple anySet Nothing)
        (ObjectSchema :< _ ) <<< ObjectF <$> traverse checkTypes obj
      Just parIdent -> checkObject obj parIdent
    )
    v
  where
    lookupSchema ident = asks $ Map.lookup ident <<< reducedSpec
    checkArray arr parIdent = do
      maybeSchema <- lookupSchema parIdent
      case maybeSchema of 
        Nothing -> throwError $ NotOneOfOptions $ encodeJson arr
        Just schema -> do
          let (Tuple minLen maxLen) = reducedArray schema
          when (invalidLen (<) minLen || invalidLen (>) maxLen) $
            throwError <<< OutOfBoundsArrayLength (textify parIdent) <<< encodeJson $ arr 
      where
        invalidLen f Nothing = false
        invalidLen f (Just len) = length arr `f` (natToInt $ len)
    checkObject obj parIdent 
      = do
        maybeSchema <- lookupSchema parIdent
        case maybeSchema of
          Nothing -> throwError $ NotOneOfOptions $ encodeJson $ hashmapToObject obj
          Just schema -> do
            let (Tuple propsSpec additionalAllowed) = reducedObject schema
            valsAndTypes <- catMaybeHashMap <$> (mergeHashMapsWithKeyM (combine additionalAllowed) propsSpec obj)
            checkedObj <- traverse assess valsAndTypes
            pure $ ObjectSchema :< ObjectF checkedObj
      where
        assess (Tuple val typeNode) = do
                put (Tuple (typeNode :| Set.empty) Nothing) 
                checkTypes val
        -- combine is used to merge propertySpec with the actual object's property in a monadic context.
        -- it returns Just (value, the type it must match against) inside the monadic context
        -- returns nothing when the property must be removed
        -- 1. only property spec found
        combine :: forall m. MonadError ValidationError m => Boolean -> String -> These (Tuple TypeNode Boolean) (Json) -> m (Maybe (Tuple Json TypeNode))
        combine _ propName (This (Tuple _ isOptional)) = do
          unless isOptional $
            throwError <<< RequiredPropertyIsMissing (textify parIdent) $ propName
          pure Nothing
        -- 2. No property spec found - ie this is an additional property
        combine additionalAllowed propName (That val) = do
          unless additionalAllowed $
            throwError $ AdditionalPropFoundButBanned (textify parIdent) $ propName
          pure $ Just (Tuple val AnyNode)
        -- 3. we found a property spec
        combine _ _ (Both (Tuple typeNode _) val) = 
          pure $ Just (Tuple val typeNode)

mergeHashMapsWithKeyM :: forall m k v1 v2 v3. Monad m => Eq k => Hashable k => (k -> These v1 v2 -> m v3) -> HashMap k v1 -> HashMap k v2 -> m (HashMap k v3)
mergeHashMapsWithKeyM f hm1 hm2 = traverse (uncurry f) $ pairKeyVal $ merged
  where
    merged = HM.unionWith joinThisThat (This <$> hm1) (That <$> hm2)

pairKeyVal :: forall k v. HashMap k v -> HashMap k (Tuple k v)
pairKeyVal = mapWithIndex Tuple

joinThisThat :: forall a b. These a b -> These a b -> These a b
joinThisThat = unsafePartial joinThisThat'
joinThisThat' :: forall a b. Partial => These a b -> These a b -> These a b
joinThisThat' (This x) (That y) = Both x y

-- checkCustoms removes all nonCustom nodes from the typenode set and 
-- checks the value against each until one succeeds
checkCustoms :: forall m. Alternative m => MonadReader Schema m => MonadState (Tuple (NonEmpty Set TypeNode) (Maybe Identifier)) m => MonadError ValidationError m => Json -> m (Cofree ValidJSONF SchemaInformation)
checkCustoms v 
  = do
    -- here we drop all non-custom nodes
    nodes <- (gets $ fst :: m (NonEmpty Set TypeNode))
    let customNodes = NonEmpty.fromNonEmpty Set.insert nodes
    let customNodeArr = (Set.toUnfoldable customNodes :: Array TypeNode)
    -- we need to turn the set into an array here as Sets are not functors in PS
    asum <<< map checkCustom $ customNodeArr
  where
    isCustom (CustomNode _) = true
    isCustom _   = false
    -- check value against successfors of a custom node
    checkCustom = unsafePartial checkCustom'
    checkCustom' :: Partial => TypeNode -> m (Cofree ValidJSONF SchemaInformation)
    checkCustom' node@(CustomNode ident) = do
      ne <- asks (forceNonEmptyFromSet <<< postSet node <<< typeGraph)
      put (Tuple ne (Just ident))
      (_ $> (UserDefined <<< textify $ ident)) <$> checkTypes v

asum :: forall t f a. Foldable t => Alternative f => t (f a) -> f a
asum = foldr (<|>) empty


forceNonEmptyFromSet :: forall a. Ord a => Set a -> NonEmpty Set a
forceNonEmptyFromSet = unsafePartial forceNonEmptyFromSet'

forceNonEmptyFromSet' :: forall a. Partial => Ord a => Set a -> NonEmpty Set a
forceNonEmptyFromSet' sa = head' :| tail'
  where
    head' = fromJust $ Set.findMin sa
    tail' = Set.delete head' sa
    fromJust (Just a) = a

anySet :: NonEmpty Set TypeNode
anySet = AnyNode :| Set.empty

textify :: Identifier -> String
textify (Identifier s) = s
