module Data.Argonaut.Arbitrary where

import MedeaPrelude
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import Data.Argonaut (Json)
import Data.Argonaut as Arg
import Data.Array as Array
import Data.List.Lazy as List
import Data.NonEmpty ((:|))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader.Trans (ReaderT, runReaderT, asks, local)
import Foreign.Object as Obj


newtype RandomJson = RandomJson Json

instance showJson :: Show RandomJson where
  show x = Arg.stringify <<< unwrap $ x

derive instance newtypeRandomJson :: Newtype RandomJson _

instance arbitraryRandomJson :: Arbitrary RandomJson where
  arbitrary = RandomJson <$> runReaderT makeRandomJson 5 --- recursionDepth

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

data ObjGenOpts = ObjGenOpts (Array String) (Array String) Int Int

arbitraryObj :: ObjGenOpts -> Gen Json
arbitraryObj opts = runReaderT (makeRandomObject opts) 2

makeRandomObject :: ObjGenOpts -> ReaderT Int Gen Json
makeRandomObject (ObjGenOpts props optionalProps minAdditional maxAdditional) = do
  entryCount <- lift $ Gen.chooseInt minAdditional maxAdditional
  genKeys <- List.replicateM entryCount $ lift arbitrary
  someOptionalProps <- List.filterM (\_ -> lift arbitrary) (List.fromFoldable optionalProps)
  let keys = genKeys <> List.fromFoldable props <> someOptionalProps
  keyVals <- traverse (\x -> (Tuple x) <$> local dec makeRandomJson) keys
  pure $ Arg.encodeJson $ Obj.fromFoldable $ keyVals

makeRandomJson :: ReaderT Int Gen Json
makeRandomJson = do
  reachedMaxDepth <- asks (_ == 0)
  choice <- lift $ Gen.chooseInt 0 (if reachedMaxDepth then 3 else 5)
  case choice of
    0 -> pure Arg.jsonNull
    1 -> lift $ Gen.elements $ Arg.jsonTrue :| [Arg.jsonFalse]
    2 -> lift $ Arg.encodeJson <$> (arbitrary :: Gen Int)
    3 -> lift $ Arg.encodeJson <$> (arbitrary :: Gen String)
    4 -> do
      len <- lift $ Gen.chooseInt 0 5
      list <- List.replicateM len (local dec makeRandomJson)
      pure $ Arg.encodeJson $ Array.fromFoldable list
    _ -> makeRandomObject (ObjGenOpts [] [] 0 5)

dec :: Int -> Int
dec n = sub 1 n
