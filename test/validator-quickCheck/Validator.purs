module Test.Validator where

import MedeaPrelude hiding (group)
import Control.Monad.Except (runExcept)
import Control.Monad.Trans.Class (lift)
import Data.Array.NonEmpty as NEA
import Data.Argonaut (class EncodeJson, Json)
import Data.Argonaut as Arg
import Data.Argonaut.Arbitrary (ObjGenOpts(..), RandomJson(..), arbitraryObj, genArrayJson, genJson)
import Data.Medea (validate)
import Data.Medea.Loader (loadSchemaFromFile, LoaderError)
import Data.Medea.Schema (Schema)
import Data.Newtype (wrap)
import Data.NonEmpty (NonEmpty, (:|), fromNonEmpty)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Object as Obj
import Mote (group, test)
import Test.QuickCheck.Combinators ((==>))
import Test.QuickCheck (class Testable, Result(..), arbitrary, quickCheckGen, withHelp)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import TestM (TestPlanM, isParseError, isSchemaError, runTestM, appendPath)
import Unsafe.Coerce (unsafeCoerce)
import Test.Spec.Assertions (shouldNotSatisfy, fail)

arrayFromNonEmptyArray :: NonEmpty Array String -> Array String
arrayFromNonEmptyArray (a :| as) = a:as

suite :: TestPlanM Unit
suite = do
  testAny
    "any.medea"
    "Any Schema"
  testSingular
    "null.medea"
    "Null Schema"
    Arg.isNull
  testSingular
    "boolean.medea"
    "Boolean Schema"
    Arg.isBoolean
  testSingular
    "number.medea"
    "Number Schema"
    Arg.isNumber
  testSingular
    "string.medea"
    "String Schema"
    Arg.isString
  testSingular
    "array.medea"
    "Array Schema"
    Arg.isArray
  testSingular
    "object.medea"
    "Object Schema"
    Arg.isObject
  testSingular
    "nullable-boolean.medea"
    "Boolean/null Schema"
    (isNullOr Arg.isBoolean)
  testSingular
    "nullable-number.medea"
    "Number Schema"
    (isNullOr Arg.isNumber)
  testSingular
    "nullable-string.medea"
    "String Schema"
    (isNullOr Arg.isString)
  testSingular
    "nullable-array.medea"
    "Array Schema"
    (isNullOr Arg.isArray)
  testSingular
    "nullable-object.medea"
    "Object Schema"
    (isNullOr Arg.isObject)
  testStringVals
    "stringVals.medea"
    ("bar" :| [ "baz" ])
  testStringVals
    "stringVals2.medea"
    ("accountant" :| [ "barber", "bishop", "baker" ])
  -- Tests for object property checks.
  -- Object schema with 1 property and no additional allowed
  testObject
    $ ObjTestParams
        { objTestOpts: ObjGenOpts [ "foo" ] [] 0 0
        , objTestPath: "1-property-no-additional-1.medea"
        , objTestPred: hasProperty "foo" Arg.isBoolean
        , objAdditionalPred: const false
        }
  testObject
    $ ObjTestParams
        { objTestOpts: ObjGenOpts [ "foo" ] [] 0 0
        , objTestPath: "1-property-no-additional-2.medea"
        , objTestPred: hasProperty "foo" Arg.isNull
        , objAdditionalPred: const false
        }
  testObject
    $ ObjTestParams
        { objTestOpts: ObjGenOpts [ "foo" ] [] 0 0
        , objTestPath: "1-property-no-additional-3.medea"
        , objTestPred: hasProperty "foo" Arg.isArray
        , objAdditionalPred: const false
        }
  testInvalidObject
    $ ObjTestParams
        { objTestOpts: ObjGenOpts [ "foo" ] [] 1 3
        , objTestPath: "1-property-no-additional-1.medea"
        , objTestPred: const true
        , objAdditionalPred: const false
        }
  testInvalidObject
    $ ObjTestParams
        { objTestOpts: ObjGenOpts [ "foo" ] [] 1 3
        , objTestPath: "1-property-no-additional-2.medea"
        , objTestPred: const true
        , objAdditionalPred: const false
        }
  testInvalidObject
    $ ObjTestParams
        { objTestOpts: ObjGenOpts [] [ "foo" ] 1 3
        , objTestPath: "1-property-no-additional-3.medea"
        , objTestPred: const true
        , objAdditionalPred: const false
        }
  -- Object schema with 1 property and additional allowed
  testObject
    $ ObjTestParams
        { objTestOpts: ObjGenOpts [ "foo" ] [] 0 3
        , objTestPath: "1-property-additional-1.medea"
        , objTestPred: hasProperty "foo" Arg.isString
        , objAdditionalPred: const true
        }
  testObject
    $ ObjTestParams
        { objTestOpts: ObjGenOpts [ "foo" ] [] 0 3
        , objTestPath: "1-property-additional-2.medea"
        , objTestPred: hasProperty "foo" Arg.isNumber
        , objAdditionalPred: const true
        }
  testObject
    $ ObjTestParams
        { objTestOpts: ObjGenOpts [ "foo" ] [] 0 3
        , objTestPath: "1-property-additional-3.medea"
        , objTestPred: hasProperty "foo" Arg.isObject
        , objAdditionalPred: const true
        }
  -- Object schema with 3 properties and no additional allowed
  testObject
    $ ObjTestParams
        { objTestOpts: ObjGenOpts [ "foo", "bar", "bazz" ] [] 0 0
        , objTestPath: "3-property-no-additional-1.medea"
        , objTestPred: hasProperty "foo" (Arg.isNumber || Arg.isArray) && hasProperty "bazz" (Arg.isNull || Arg.isBoolean) && hasProperty "bar" (const true)
        , objAdditionalPred: const false
        }
  testObject
    $ ObjTestParams
        { objTestOpts: ObjGenOpts [ "bar", "bazz" ] [ "foo" ] 0 0
        , objTestPath: "3-property-no-additional-2.medea"

        , objTestPred: (hasOptionalProperty "foo" (Arg.isNumber || Arg.isArray) && hasProperty "bazz" (Arg.isNull || Arg.isBoolean) && hasProperty "bar" (const true))
        , objAdditionalPred: const false
        }
  testInvalidObject
    $ ObjTestParams
        { objTestOpts: ObjGenOpts [ "foo", "bar", "bazz" ] [] 1 3
        , objTestPath: "3-property-no-additional-1.medea"
        , objTestPred: const true
        , objAdditionalPred: const false
        }
  testInvalidObject
    $ ObjTestParams
        { objTestOpts: ObjGenOpts [ "bar", "bazz" ] [ "foo" ] 1 3
        , objTestPath: "3-property-no-additional-2.medea"
        , objTestPred: const true
        , objAdditionalPred: const false
        }
  -- Object schema with 3 properties and additional allowed
  testObject
    $ ObjTestParams
        { objTestOpts: ObjGenOpts [ "foo", "bar", "bazz" ] [] 0 3
        , objTestPath: "3-property-additional-allowed-1.medea"
        , objTestPred: hasProperty "foo" Arg.isBoolean && hasProperty "bazz" Arg.isString
        , objAdditionalPred: const true
        }
  testObject
    $ ObjTestParams
        { objTestOpts: ObjGenOpts [ "bar", "bazz" ] [ "foo" ] 0 3
        , objTestPath: "3-property-additional-allowed-2.medea"
        , objTestPred: (hasOptionalProperty "foo" Arg.isNumber) && (hasProperty "bazz" Arg.isNull)
        , objAdditionalPred: const true
        }
  -- Object schema with additional property schema"
  testObject
    $ ObjTestParams
      { objTestOpts: ObjGenOpts [] [] 0 3,
        objTestPath: "map-number-bool.medea",
        objTestPred: const true,
        objAdditionalPred: Arg.isNumber || Arg.isBoolean
      }
  testObject
    $ ObjTestParams
      { objTestOpts: ObjGenOpts ["foo"] [] 0 3,
        objTestPath: "map-with-1-specified.medea",
        objTestPred: hasProperty "foo" (Arg.isArray || Arg.isObject),
        objAdditionalPred: Arg.isNumber || Arg.isBoolean
      }
  testObject
    $ ObjTestParams
      { objTestOpts: ObjGenOpts ["foo"] ["bazz"] 0 3,
        objTestPath: "map-with-2-specified.medea",
        objTestPred: hasProperty "foo" (Arg.isArray || Arg.isObject),
        objAdditionalPred: Arg.isNumber || Arg.isBoolean
       }
  --Array schema with element_type only
  testList
    $ ListTestParams
        { listTestOpts: (Tuple 0 3)
        , listTestPath: "list-1.medea"
        , elementPred: Arg.isNumber || Arg.isBoolean || Arg.isObject
        , lenPred: const true
        }
  testList
    $ ListTestParams
        { listTestOpts: (Tuple 1 3)
        , listTestPath: "list-2.medea"
        , elementPred: Arg.isNumber || Arg.isBoolean || Arg.isObject
        , lenPred: const true
        }
  -- Array schema with length spec only
  testList
    $ ListTestParams
        { listTestOpts: (Tuple 1 6)
        , listTestPath: "list-3.medea"
        , elementPred: const true
        , lenPred: arrayLenGE 2
        }
  testList
    $ ListTestParams
        { listTestOpts: (Tuple 1 6)
        , listTestPath: "list-4.medea"
        , elementPred: const true
        , lenPred: arrayLenLE 5
        }
  testList
    $ ListTestParams
        { listTestOpts: (Tuple 1 6)
        , listTestPath: "list-5.medea"
        , elementPred: const true
        , lenPred: arrayLenLE 5 && arrayLenGE 3
        }
  -- Array spec with length and element type (List Spec)
  testList
    $ ListTestParams
        { listTestOpts: (Tuple 1 4)
        , listTestPath: "list-6.medea"
        , elementPred: Arg.isNull || Arg.isBoolean || Arg.isNumber
        , lenPred: arrayLenGE 2 && arrayLenLE 3
        }
  testList
    $ ListTestParams
        { listTestOpts: (Tuple 1 4)
        , listTestPath: "list-7.medea"
        , elementPred: Arg.isNull || Arg.isBoolean || Arg.isNumber
        , lenPred: arrayLenGE 2 && arrayLenLE 3
        }
  -- tupleSpec
  testTuple
    $ TupleTestParams
        { tupleTestOpts: (Tuple 3 4)
        , tupleTestPath: "3-tuple.medea"
        , tuplePreds: [ Arg.isNumber || Arg.isArray, Arg.isBoolean, const true ]
        }
  testTuple
    $ TupleTestParams
        { tupleTestOpts: (Tuple 1 3)
        , tupleTestPath: "2-tuple.medea"
        , tuplePreds: [ Arg.isObject || Arg.isNull, Arg.isString || Arg.isNumber ]
        }

-- helpers
data ObjTestParams
  = ObjTestParams
    { objTestOpts :: ObjGenOpts
    , objTestPath :: String
    , objTestPred :: Json -> Boolean
    , objAdditionalPred :: Json -> Boolean
    }

data ListTestParams
  = ListTestParams
    { listTestOpts :: (Tuple Int Int)
    , listTestPath :: String
    , elementPred :: Json -> Boolean
    , lenPred :: Array Json -> Boolean
    }

data TupleTestParams
  = TupleTestParams
    { tupleTestOpts :: (Tuple Int Int)
    , tupleTestPath :: String
    , tuplePreds :: Array (Json -> Boolean)
    }

prependTestDir :: String -> String
prependTestDir s = appendPath "./conformance/validation/" s

-- due to Mote and Spec preventing Effects from within a group/describe,
-- we need to execute effects outside the group or inside the test only
testWrap :: String -> String -> (Either LoaderError Schema -> TestPlanM Unit) -> (Schema -> TestPlanM Unit) -> TestPlanM Unit
testWrap name fp eitherTests tests = do
  result <- lift $ runTestM $ loadSchemaFromFile $ prependTestDir $ fp
  group ("Test group: " <> name)
    $ do
        eitherTests result
        case result of
          Left e -> test ("Not Left file: " <> fp) (fail "unexpected Left")
          Right scm -> do
            tests scm

testWrap' :: String -> String -> (Schema -> TestPlanM Unit) -> TestPlanM Unit
testWrap' name fp tests = testWrap name fp (const $ pure unit) tests

testObject :: ObjTestParams -> TestPlanM Unit
testObject (ObjTestParams { objTestOpts, objTestPath, objTestPred, objAdditionalPred }) =
  testWrap' "Object test" objTestPath
    $ \scm -> do
        test ("Should validate valid objects" <> ": " <> objTestPath) (propertyTest $ validationSuccess (arbitraryObj objTestOpts) p' scm)
        test ("Should not validate invalid objects" <> ": " <> objTestPath) (propertyTest $ validationFail (arbitraryObj objTestOpts) (not <<< p') scm)
  where
    p' =  objTestPred && makeMapPred objTestOpts objAdditionalPred

makeMapPred :: ObjGenOpts -> (Json -> Boolean) -> Json -> Boolean
makeMapPred (ObjGenOpts props optProps _ _) p
  = Arg.caseJsonObject
    false
   (all p <<< Obj.filterWithKey (\k _ -> k `notElem` specifiedProps))
  where
    specifiedProps = props <> optProps

-- Tests for Object values that should always get invalidated.
testInvalidObject :: ObjTestParams -> TestPlanM Unit
testInvalidObject (ObjTestParams { objTestOpts, objTestPath, objTestPred }) =
  testWrap' "Invalid Object test" objTestPath
    $ \scm -> do
        test ("Should not validate" <> ": " <> objTestPath) (propertyTest $ validationFail (arbitraryObj objTestOpts) objTestPred scm)

testList :: ListTestParams -> TestPlanM Unit
testList (ListTestParams { listTestOpts: (Tuple r1 r2), listTestPath, elementPred, lenPred }) =
  testWrap' "ListTest" listTestPath
    ( \scm -> do
        test ("should validate lists: " <> listTestPath) (propertyTest $ validationSuccess genArrayJson p scm)
        test ("shouldn't validate lists: " <> listTestPath) (propertyTest $ validationFail genArrayJson (not <<< p) scm)
    )
  where
  p arr = (all elementPred arr) && (lenPred arr)

testTuple :: TupleTestParams -> TestPlanM Unit
testTuple (TupleTestParams { tupleTestOpts: (Tuple r1 r2), tupleTestPath, tuplePreds }) =
  testWrap' "TupleTest" tupleTestPath
    ( \scm -> do
        test ("should validate valid tuples: " <> tupleTestPath) (propertyTest $ validationSuccess genArrayJson p scm)
        test ("shouldn't validate invalid tuples: " <> tupleTestPath) (propertyTest $ validationFail genArrayJson (not <<< p) scm)
    )
  where
  p :: Array Json -> Boolean
  p arr = (and $ zipWith ($) tuplePreds arr) && (length tuplePreds == length arr)

-- "validation succeeded" property
yesProp :: Gen Json -> (Json -> Boolean) -> Schema -> Effect Unit
yesProp gen p scm =
  quickCheckGen
    $ do
        v <- gen
        pure $ prop v
  where
  prop v = toResult p v ==> toResult (isRight <<< runExcept <<< validate scm <<< Arg.stringify) v

-- "validation failed" property
noProp :: Gen Json -> (Json -> Boolean) -> Schema -> Effect Unit
noProp gen p scm =
  quickCheckGen
    $ do
        v <- gen
        pure $ prop v
  where
  prop v = toResult p v ==> toResult (isLeft <<< runExcept <<< validate scm <<< Arg.stringify) v

-- Returns true iff the value is an object with the given property and the
-- property-value satisfies the predicate.
hasProperty :: String -> (Json -> Boolean) -> Json -> Boolean
hasProperty = propTest false

-- Like hasProperty but is also true when the given property is absent.
hasOptionalProperty :: String -> (Json -> Boolean) -> Json -> Boolean
hasOptionalProperty = propTest true

propTest :: Boolean -> String -> (Json -> Boolean) -> Json -> Boolean
propTest haveProp propName p j =
  Arg.caseJsonObject
    false
    (\obj -> maybe haveProp (p) $ Obj.lookup propName obj)
    j

isNullOr :: (Json -> Boolean) -> (Json -> Boolean)
isNullOr f = (||) <$> Arg.isNull <*> f

testAny :: String -> String -> TestPlanM Unit
testAny fp name =
  testWrap name fp
    ( \result -> do
        test ("should parse: " <> fp) (result `shouldNotSatisfy` isParseError)
        test ("should build: " <> fp) (result `shouldNotSatisfy` isSchemaError)
    )
    ( \scm -> do
        test ("should validate anything: " <> fp) (propertyTest $ validationSuccess genJson (const true) scm)
    )
  where
  go scm (RandomJson j) = isRight $ runExcept <<< validate scm <<< Arg.stringify $ j

testSingular :: String -> String -> (Json -> Boolean) -> TestPlanM Unit
testSingular fp name p =
  testWrap name fp
    ( \result -> do
        test ("should parse: " <> fp) (result `shouldNotSatisfy` isParseError)
        test ("should build: " <> fp) (result `shouldNotSatisfy` isSchemaError)
    )
    ( \scm -> do
        test ("should validate " <> name <> "s: " <> fp) (liftEffect $ yesProp genJson p scm)
        test ("should not validate non-" <> name <> "s: " <> fp) (liftEffect $ noProp genJson (not <<< p) scm)
    )
  where
  genJson = unsafeCoerce (arbitrary :: Gen RandomJson)

testStringVals :: String -> NonEmpty Array String -> TestPlanM Unit
testStringVals fp validStrings =
  let
    name = "string is one of " <> show validStrings
  in
    testWrap' name fp
      ( \scm -> do
          test ("Should Validate " <> name <> fp) (propertyTest $ validationSuccess genString p scm)
          test ("Shouldn't Validate " <> name <> fp) (propertyTest $ validationFail genString (not <<< p) scm)
      )
  where
  genString :: Gen.Gen String
  genString = Gen.oneOf $ NEA.fromNonEmpty $ (Gen.elements $ NEA.fromNonEmpty validStrings) :| [ arbitrary ]
  p :: String -> Boolean
  p = (_ `elem` (arrayFromNonEmptyArray validStrings))

propertyTest :: forall m a. MonadEffect m => Testable a => Gen a -> m Unit
propertyTest g = liftEffect $ quickCheckGen $ g

validationSuccess :: forall a. EncodeJson a => Gen a -> (a -> Boolean) -> Schema -> Gen Result
validationSuccess = validationTest isRight

validationFail :: forall a. EncodeJson a => Gen a -> (a -> Boolean) -> Schema -> Gen Result
validationFail = validationTest isLeft

validationTest :: forall a. EncodeJson a => (forall b c. Either b c -> Boolean) -> Gen a -> (a -> Boolean) -> Schema -> Gen Result
validationTest eitherPred gen p scm = do
  a <- gen
  let result = prop a
  pure $ withHelp
    (resultBool result)
    ("validation failure: " <> (Arg.stringify $ Arg.encodeJson a) <> " result: " <> (show $ action a))
  where
  action = runExcept <<< validate scm <<< Arg.stringify <<< Arg.encodeJson
  prop v =
    toResult  p v ==> toResult (eitherPred <<< action) v

resultBool :: Result -> Boolean
resultBool Success = true
resultBool _ = false

toResult' :: Boolean -> Result
toResult' a = withHelp a "failed predicate"

toResult :: forall a. (a -> Boolean) -> a -> Result
toResult f a = withHelp (f a) "failed predicate"

arrayLenGE :: forall a. Int -> Array a -> Boolean
arrayLenGE len arr = length arr >= len

arrayLenLE :: forall a. Int -> Array a -> Boolean
arrayLenLE len arr = length arr <= len
