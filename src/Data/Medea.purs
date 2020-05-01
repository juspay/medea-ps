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
import Data.Argonaut (Json, encodeJson, jsonNull, jsonFalse, jsonTrue, jsonEmptyObject, caseJsonObject, fromObject, jsonParser, caseJson, stringify)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.HashMap as HM
import Data.Map as Map
import Data.Medea.Analysis (TypeNode(..), CompiledSchema(..), ArrayType(..))
import Data.Medea.JSONType (JSONType, typeOf)
--, loadSchemaFromFile, loadSchemaFromHandle)
import Data.Medea.MedeaJSON (MJSON(..))
import Data.Medea.Parser.Primitive (Identifier(..), ReservedIdentifier(..), identFromReserved)
import Data.Medea.Schema (Schema)
import Data.Medea.ValidJSON (ValidJSONF(..), objectToHashMap)
import Data.Natural (Natural, natToInt)
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
newtype ValidatedJSON
  = ValidatedJSON (Cofree ValidJSONF SchemaInformation)

derive newtype instance eqValidatedJSON :: Eq ValidatedJSON

derive instance genericValidatedJSON :: Generic ValidatedJSON _

instance showValidatedJSON :: Show ValidatedJSON where
  show (ValidatedJSON cof) = "Cofree ( " <> showed <> " )"
    where
    showed = show $ extract cof

instance hashableValidatedJSON :: Hashable ValidatedJSON where
  hash (ValidatedJSON cof) = hash $ extract cof

toJSONValue :: ValidatedJSON -> Json
toJSONValue (ValidatedJSON cof) =
  let
    f = Cofree.tail cof
  in
    case f of
      AnythingF v -> unwrap v
      NullF -> jsonNull
      BooleanF b -> if b then jsonTrue else jsonFalse
      NumberF n -> encodeJson n
      StringF s -> encodeJson s
      ArrayF a -> encodeJson $ map (toJSONValue <<< unsafeCoerce) $ a
      ObjectF hm -> hashmapEncodeJson $ map (toJSONValue <<< unsafeCoerce) $ hm

hashmapEncodeJson :: HashMap String Json -> Json
hashmapEncodeJson hsj = foldlWithIndex go jsonEmptyObject hsj
  where
  go :: String -> Json -> Json -> Json
  go k v acc = fromObject $ caseJsonObject Object.empty ins acc
    where
    ins o = Object.insert k v o

validAgainst :: ValidatedJSON -> SchemaInformation
validAgainst (ValidatedJSON cof) = extract cof

data ValidationError
  = EmptyError
  -- | We could not parse JSON out of what we were provided.
  | NotJSON
  -- | We got a type different to what we expected. 
  | WrongType Json JSONType
  -- | We expected one of several possibilities, but got something that fits -- none.
  | NotOneOfOptions Json
  | AdditionalPropFoundButBanned String String
  | RequiredPropertyIsMissing String String
  | OutOfBoundsArrayLength String Json
  | ImplementationError String

derive instance eqValidationError :: Eq ValidationError

derive instance genericValidationError :: Generic ValidationError _

instance showValidationError :: Show ValidationError where
  -- Json doesn't have a show instance
  show (WrongType j jt) = "WrongType " <> stringify j <> " type: " <> show jt
  show (EmptyError) = "EmptyError"
  show (NotJSON) = "NotJSON"
  show (NotOneOfOptions j) = "NotOneOfOptions " <> stringify j
  show (AdditionalPropFoundButBanned s1 s2) = "AdditionalPropFoundButBanned " <> s1 <> "  " <> s2
  show (RequiredPropertyIsMissing s1 s2) = "RequiredPropertyIsMissing " <> s1 <> s2
  show (OutOfBoundsArrayLength s j) = "OutOfBoundsArrayLength " <> s <> " " <> stringify j
  show (ImplementationError s) = "ImplementationError " <> s

instance semigroupValidationError :: Semigroup ValidationError where
  append EmptyError x = x
  append x _ = x

instance monoidValidationError :: Monoid ValidationError where
  mempty = EmptyError

decode :: String -> Maybe Json
decode = hush <<< jsonParser

validate :: forall m. MonadPlus m => MonadError ValidationError m => Schema -> String -> m ValidatedJSON
validate scm str = case decode str of
  Nothing -> throwError $ NotJSON
  Just v -> ValidatedJSON <$> go v
  where
  go v = runReaderT (evalStateT (checkTypes v) (Tuple initialSet Nothing)) scm

  initialSet = (_ :| Set.empty) <<< CustomNode <<< identFromReserved $ RStart

validateFromFile :: forall m. MonadPlus m => MonadError ValidationError m => MonadAff m => Schema -> String -> m ValidatedJSON
validateFromFile scm fp = do
  contents <- liftAff $ readTextFile UTF8 fp
  validate scm fp

-- validateFromHandle not implmented here because the ps node filesystem implementation doesn't really deal with raw handles
checkTypes :: forall m. Alternative m => MonadReader Schema m => MonadState (Tuple (NonEmpty Set TypeNode) (Maybe Identifier)) m => MonadError ValidationError m => Json -> m (Cofree ValidJSONF SchemaInformation)
checkTypes v = checkAny v <|> checkPrim v <|> checkCustoms v

checkAny :: forall m. Alternative m => MonadState (Tuple (NonEmpty Set TypeNode) (Maybe Identifier)) m => MonadError ValidationError m => Json -> m (Cofree ValidJSONF SchemaInformation)
checkAny v = do
  minNode <- gets $ Set.findMin <<< (NonEmpty.fromNonEmpty Set.insert) <<< fst -- AnyNode is the smallest possible TypeNode
  case minNode of
    Just AnyNode -> do
      pure $ AnySchema :< (AnythingF $ MJSON v)
    _ -> do
      throwError EmptyError

checkPrim :: forall m. Alternative m => MonadReader Schema m => MonadState (Tuple (NonEmpty Set TypeNode) (Maybe Identifier)) m => MonadError ValidationError m => Json -> m (Cofree ValidJSONF SchemaInformation)
checkPrim v = do
  (Tuple neNodes par) <- gets identity
  let
    nodes = NonEmpty.fromNonEmpty (Set.insert) neNodes
  unless (Set.member (PrimitiveNode <<< typeOf $ v) nodes)
    $ do
        throwError $ NotOneOfOptions $ v
  caseJson
    (\null -> pure $ NullSchema :< NullF)
    (\bool -> pure $ BooleanSchema :< BooleanF bool)
    (\num -> pure $ NumberSchema :< NumberF num)
    ( \str -> do
        -- if we are dealing with a dependent string, we match against supplied values
        let
          validated = pure $ StringSchema :< StringF str
        case par of
          Nothing -> validated
          Just parIdent -> do
            scm@(CompiledSchema cs) <- lookupSchema parIdent
            let
              validVals = cs.stringVals
            if (str `elem` validVals || null validVals) then do
              pure $ StringSchema :< StringF str
            else do
              throwError $ NotOneOfOptions v
    )
    ( \arr -> do
        case par of
          Nothing -> do
            put (Tuple anySet Nothing)
            (ArraySchema :< _) <<< ArrayF <$> traverse checkTypes arr
          Just parIdent -> checkArray arr parIdent
    )
    ( \o ->
        let
          obj = objectToHashMap o
        in
          case par of
            -- Fast Path (no object spec)
            Nothing -> do
              put (Tuple anySet Nothing)
              (ObjectSchema :< _) <<< ObjectF <$> traverse checkTypes obj
            Just parIdent -> checkObject obj parIdent
    )
    v

lookupSchema :: forall m. MonadReader Schema m => MonadError ValidationError m => Identifier -> m CompiledSchema
lookupSchema ident = do
  mscm <- asks $ Map.lookup ident <<< unwrap
  case mscm of
    Nothing -> throwError $ ImplementationError "Unreachable state: we should not be able to find this schema"
    Just scm -> pure scm

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

checkArray :: forall m. Alternative m => MonadReader Schema m => MonadState (Tuple (NonEmpty Set TypeNode) (Maybe Identifier)) m => MonadError ValidationError m => Array Json -> Identifier -> m (Cofree ValidJSONF SchemaInformation)
checkArray arr parIdent = do
  (CompiledSchema cs) <- lookupSchema parIdent
  let
    arrLen = length arr

    outOfBounds = OutOfBoundsArrayLength (textify parIdent) <<< encodeJson $ arr
  when
    ( compareMaybe (<) arrLen cs.minListLen
        || compareMaybe (>) arrLen cs.maxListLen
    )
    $ do
        throwError $ outOfBounds
  maybe (pure unit) (checkTupleLength arr outOfBounds) cs.arrayTypes
  let
    valsAndTypes = pairValsWithTypes $ cs.arrayTypes
  checkedArray <- traverse assess valsAndTypes
  pure $ ArraySchema :< ArrayF checkedArray
  where
  assess (Tuple val typeNode) = do
    put (Tuple (typeNode :| Set.empty) Nothing)
    checkTypes val

  pairValsWithTypes Nothing = map (_ /\ AnyNode) arr

  pairValsWithTypes (Just (ListType node)) = map (_ /\ node) arr

  pairValsWithTypes (Just (TupleType nodes)) = zip arr nodes

compareMaybe :: (Int -> Int -> Boolean) -> Int -> Maybe Natural -> Boolean
compareMaybe f i mn = maybe false (\n -> f i $ natToInt n) mn

checkTupleLength :: forall m a e. MonadError e m => Array a -> e -> ArrayType -> m Unit
checkTupleLength arr err (ListType _) = pure unit

checkTupleLength arr err (TupleType nodes) =
  if length nodes == length arr then
    pure unit
  else
    throwError $ err

checkObject :: forall m. Alternative m => MonadReader Schema m => MonadState (Tuple (NonEmpty Set TypeNode) (Maybe Identifier)) m => MonadError ValidationError m => HashMap String Json -> Identifier -> m (Cofree ValidJSONF SchemaInformation)
checkObject obj parIdent = do
  schema@(CompiledSchema cs) <- lookupSchema parIdent
  valsAndTypes <- pairPropertySchemaAndVal obj (cs.props) (cs.additionalProps) parIdent
  checkedObj <- traverse assess valsAndTypes
  pure $ ObjectSchema :< ObjectF checkedObj
  where
  assess (Tuple val typeNode) = do
    put (Tuple (typeNode :| Set.empty) Nothing)
    checkTypes val

pairPropertySchemaAndVal :: forall m. Alternative m => MonadReader Schema m => MonadError ValidationError m => HashMap String Json -> HashMap String (Tuple TypeNode Boolean) -> Boolean -> Identifier -> m (HashMap String (Tuple Json TypeNode))
pairPropertySchemaAndVal obj properties extraAllowed parIdent = do
  mappedObj <- traverse pairProperty $ mapWithIndex Tuple obj
  traverse_ isMatched $ (mapWithIndex Tuple (properties :: HashMap String (Tuple TypeNode Boolean)) :: HashMap String (Tuple String (Tuple TypeNode Boolean)))
  pure mappedObj
  where
  pairProperty (Tuple propName j) = case HM.lookup propName properties of
    Just (Tuple typeNode _) -> pure (Tuple j typeNode)
    Nothing
      | extraAllowed -> do
        pure (Tuple j AnyNode)
      | otherwise -> do
        throwError $ AdditionalPropFoundButBanned (textify parIdent) $ propName

  isMatched :: String /\ TypeNode /\ Boolean -> m Unit
  isMatched (propName /\ _ /\ optional) = do
    when (isNothing (HM.lookup propName obj) && not optional)
      $ do
          throwError <<< RequiredPropertyIsMissing (textify parIdent) $ propName

-- checkCustoms removes all nonCustom nodes from the typenode set and 
-- checks the value against each until one succeeds
checkCustoms :: forall m. Alternative m => MonadReader Schema m => MonadState (Tuple (NonEmpty Set TypeNode) (Maybe Identifier)) m => MonadError ValidationError m => Json -> m (Cofree ValidJSONF SchemaInformation)
checkCustoms v = do
  -- here we drop all non-custom nodes
  nodes <- (gets $ fst :: m (NonEmpty Set TypeNode))
  let
    nodeSet = NonEmpty.fromNonEmpty Set.insert nodes

    customNodeSet = Set.filter (isCustom) nodeSet

    customNodeArr = (Set.toUnfoldable customNodeSet :: Array TypeNode)
  -- we need to turn the set into an array here as Sets are not functors in PS
  asum <<< map checkCustom $ customNodeArr
  where
  checkCustom node@(CustomNode ident) = do
    (CompiledSchema cs) <- lookupSchema ident
    put (Tuple cs.typesAs (Just ident))
    (_ $> (UserDefined <<< textify $ ident)) <$> checkTypes v

  checkCustom _ = throwError $ ImplementationError "checkCustom received non-custom as input"

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

isCustom :: TypeNode -> Boolean
isCustom (CustomNode _) = true

isCustom _ = false
