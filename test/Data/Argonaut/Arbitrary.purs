module Data.Argonaut.Arbitrary
  ( RandomJson(..)
  , RandomJsonArray(..)
  , ObjGenOpts(..)
  , arbitraryObj
  , genJson
  , genArrayJson
  , arbitraryJsonArray
  ) where

import MedeaPrelude
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import Data.Argonaut (Json)
import Data.Argonaut as Arg
import Data.Array as Array
import Data.List.Lazy as List
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader.Trans (ReaderT, runReaderT, asks, local)
import Foreign.Object as Obj

newtype RandomJson
  = RandomJson Json

instance showJson :: Show RandomJson where
  show x = Arg.stringify <<< unwrap $ x

derive instance newtypeRandomJson :: Newtype RandomJson _

instance arbitraryRandomJson :: Arbitrary RandomJson where
  arbitrary = RandomJson <$> runReaderT makeRandomJson 5 --- recursionDepth

genJson :: Gen Json
genJson = unwrap <$> (arbitrary :: Gen RandomJson)

toJson :: RandomJson -> Json
toJson = unwrap

-- the haskell implementation of this module includes a number of helper predicates available directly from argonaut
number :: Int -> Int
number = identity

string :: String -> String
string = identity

array :: forall a. Array a -> Array a
array = identity

-- Takes 4 fields:
-- required properties,
-- optional properties,
-- minimum additional properties &
-- maximum additional properties.
data ObjGenOpts
  = ObjGenOpts (Array String) (Array String) Int Int

arbitraryObj :: ObjGenOpts -> Gen Json
arbitraryObj opts = runReaderT (makeRandomObject opts) 2

makeRandomObject :: ObjGenOpts -> ReaderT Int Gen Json
makeRandomObject (ObjGenOpts props optionalProps minAdditional maxAdditional) = do
  entryCount <- lift $ Gen.chooseInt minAdditional maxAdditional
  genKeys <- List.replicateM entryCount $ lift arbitrary
  someOptionalProps <- List.filterM (\_ -> lift arbitrary) (List.fromFoldable optionalProps)
  let
    keys = genKeys <> List.fromFoldable props <> someOptionalProps
  keyVals <- traverse (\x -> (Tuple x) <$> local dec makeRandomJson) keys
  pure $ Arg.encodeJson $ Obj.fromFoldable $ keyVals

makeRandomJson :: ReaderT Int Gen Json
makeRandomJson = do
  reachedMaxDepth <- asks (_ == 0)
  choice <- lift $ Gen.chooseInt 0 (if reachedMaxDepth then 3 else 5)
  case choice of
    0 -> pure Arg.jsonNull
    1 -> lift $ Arg.encodeJson <$> (arbitrary :: Gen Boolean)
    2 -> lift $ Arg.encodeJson <$> (arbitrary :: Gen Int)
    3 -> lift $ Arg.encodeJson <$> (arbitrary :: Gen String)
    4 -> Arg.encodeJson <$> makeRandomArray 0 5
    _ -> makeRandomObject (ObjGenOpts [] [] 0 5)

dec :: Int -> Int
dec n = sub 1 n

newtype RandomJsonArray
  = RandomJsonArray (Array Json)

instance arbitraryRandomJsonArray :: Arbitrary RandomJsonArray where
  arbitrary = arbitraryJsonArray 0 5

arbitraryJsonArray :: Int -> Int -> Gen RandomJsonArray
arbitraryJsonArray r1 r2 = RandomJsonArray <$> runReaderT (makeRandomArray r1 r2) 2

genArrayJson :: Gen (Array Json)
genArrayJson = runReaderT (makeRandomArray 0 5) 2

makeRandomArray :: Int -> Int -> ReaderT Int Gen (Array Json)
makeRandomArray r1 r2 = do
  len <- lift $ Gen.chooseInt r1 r2
  list <- List.replicateM len (local dec makeRandomJson)
  pure $ Array.fromFoldable list
