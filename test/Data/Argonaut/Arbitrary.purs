module Data.Argonaut.Arbitrary where

import MedeaPrelude
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import Data.Argonaut (Json)
import Data.Argonaut as Arg
import Data.Array as Array
import Data.List.Lazy (replicateM)
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
      list <- replicateM len (local dec makeRandomJson)
      pure $ Arg.encodeJson $ Array.fromFoldable list
    _ -> do
      entryCount <- lift $ Gen.chooseInt 0 5
      keyVals <- replicateM entryCount (Tuple <$> lift arbitrary <*> local dec makeRandomJson)
      pure $ Arg.encodeJson $ Obj.fromFoldable keyVals
    where dec n = sub 1 n


