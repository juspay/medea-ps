module Test.Validator where 

import MedeaPrelude hiding (group)
import Control.Monad.Except (runExcept)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (Json)
import Data.Argonaut as Arg
import Data.Argonaut.Arbitrary (RandomJson(..), ObjGenOpts(..), arbitraryObj)
import Data.Medea (validate)
import Data.Medea.Loader (loadSchemaFromFile, LoaderError)
import Data.Medea.Schema (Schema)
import Data.NonEmpty (NonEmpty, (:|))
import Effect (Effect)
import Foreign.Object as Obj
import Mote (group, test)
import Test.QuickCheck.Combinators ((==>))
import Test.QuickCheck (Result, arbitrary, quickCheck, quickCheckGen, withHelp, (<?>))
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import TestM (TestPlanM, isParseError, isSchemaError, runTestM)

import Unsafe.Coerce (unsafeCoerce)

import Test.Spec.Assertions (shouldNotSatisfy, fail)

import Effect.Class (liftEffect)

suite :: TestPlanM Unit
suite = do
  testAny 
    "any.medea" "Any Schema"
  testSingular 
    "null.medea" "Null Schema" Arg.isNull
  testSingular 
    "boolean.medea" "Boolean Schema" Arg.isBoolean
  testSingular 
    "number.medea" "Number Schema" Arg.isNumber
  testSingular 
    "string.medea" "String Schema" Arg.isString
  testSingular 
    "array.medea" "Array Schema" Arg.isArray
  testSingular 
    "object.medea" "Object Schema" Arg.isObject
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
    ("bar" :| ["baz"])
  testStringVals
    "stringVals2.medea"
    ("accountant" :| ["barber", "bishop", "baker"])  
    -- Tests for object property checks.
  testObject
    (arbitraryObj $ ObjGenOpts ["foo"] [] 0 0)
    "Object schema with 1 property"
    "1-property-no-additional-1.medea"
    (hasProperty "foo" Arg.isBoolean)
  testObject 
    (arbitraryObj $ ObjGenOpts ["foo"] [] 0 0)
    "Object schema with 1 property"
    "1-property-no-additional-2.medea"
    (hasProperty "foo" Arg.isNull)
  testObject 
    (arbitraryObj $ ObjGenOpts [] ["foo"] 0 0)
    "Object schema with 1 property"
    "1-property-no-additional-3.medea"
    (hasOptionalProperty "foo" Arg.isArray)
  testObject 
    (arbitraryObj $ ObjGenOpts ["foo"] [] 0 3)
    "Object schema with 1 property and additional allowed"
    "1-property-additional-1.medea"
    (hasProperty "foo" Arg.isString)
  testObject
    (arbitraryObj $ ObjGenOpts ["foo"] [] 0 3)
    "Object schema with 1 property and additional allowed"
    "1-property-additional-2.medea"
    (hasProperty "foo" Arg.isNumber)
  testObject
    (arbitraryObj $ ObjGenOpts ["foo"] [] 0 3)
    "Object schema with 1 property and additional allowed"
    "1-property-additional-3.medea"
    (hasOptionalProperty "foo" Arg.isObject)
  testObject
    (arbitraryObj $ ObjGenOpts ["foo", "bar", "bazz"] [] 0 0)
    "Object schema with 3 properties"
    "3-property-no-additional-1.medea"
    (\x -> hasProperty "foo" Arg.isBoolean x && hasProperty "bazz" Arg.isString x)
  testObject
    (arbitraryObj $ ObjGenOpts ["bar", "bazz"] ["foo"] 0 0)
    "Object schema with 3 properties"
     "3-property-no-additional-2.medea"
    (\x -> hasOptionalProperty "foo" Arg.isNumber x && hasProperty "bazz" Arg.isNull x)
  testObject
    (arbitraryObj $ ObjGenOpts ["foo", "bar", "bazz"] [] 0 3)
    "Object schema with 3 properties and additional allowed"
    "3-property-additional-allowed-1.medea"
    (\x -> hasProperty "foo" Arg.isBoolean x && hasProperty "bazz" Arg.isString x)
  testObject
    (arbitraryObj $ ObjGenOpts ["bar", "bazz"] ["foo"] 0 3)
    "Object schema with 3 properties and additional allowed"
    "3-property-additional-allowed-2.medea"
    (\x -> hasOptionalProperty "foo" Arg.isNumber x && hasProperty "bazz" Arg.isNull x)
     -- These tests are for objects where additional are not allowed but are still found.
  -- The generator is such that additional properties always exists.
  testInvalidObject 
    (arbitraryObj $ ObjGenOpts ["foo"] [] 1 3)
    "Object schema with 1 property and no additional allowed"
    "1-property-no-additional-1.medea"
  testInvalidObject 
    (arbitraryObj $ ObjGenOpts ["foo"] [] 1 3) 
    "Object schema with 1 property and no additional allowed"
    "1-property-no-additional-2.medea"
  testInvalidObject 
    (arbitraryObj $ ObjGenOpts [] ["foo"] 1 3)
    "Object schema with 1 property and no additional allowed"
     "1-property-no-additional-3.medea"
  testInvalidObject 
    (arbitraryObj $ ObjGenOpts ["foo", "bar", "bazz"] [] 1 3)
    "Object schema with 3 properties and no additional allowed"
    "3-property-no-additional-1.medea"
  testInvalidObject 
    (arbitraryObj $ ObjGenOpts ["bar", "bazz"] ["foo"] 1 3)
    "Object schema with 3 properties and no additional allowed"
     "3-property-no-additional-2.medea"


-- helpers

prependTestDir :: String -> String
prependTestDir s = "./conformance/validation/" <> s

  -- due to Mote and Spec preventing Effects from within a group/describe, 
  -- we need to execute effects outside the group or inside the test only
testWrap :: String -> String -> (Either LoaderError Schema -> TestPlanM Unit) -> (Schema -> TestPlanM Unit) -> TestPlanM Unit
testWrap name fp eitherTests tests = do
  result <- lift $ runTestM $ loadSchemaFromFile $ prependTestDir $ fp
  group ("Test group: " <> name) $ do
    eitherTests result
    case result of 
      Left _ -> test ("Not Left file: " <> fp) (fail "unexpected Left")
      Right scm -> do
         tests scm

testWrap' :: String -> String -> (Schema -> TestPlanM Unit) -> TestPlanM Unit
testWrap' name fp tests = testWrap name fp (const $ pure unit) tests

testObject :: Gen Json -> String -> String -> (Json -> Boolean) -> TestPlanM Unit
testObject gen name fp p = testWrap' name fp $ \scm -> do
        test ("Should validate valid objects" <> ": " <> fp) (liftEffect $ yesProp gen p scm)
        test ("Should not validate invalid objects" <> ": " <> fp) (liftEffect $ noProp gen (not <<< p) scm)

-- Tests for Object values that should always get invalidated.
testInvalidObject :: Gen Json -> String -> String -> TestPlanM Unit
testInvalidObject gen name fp = testWrap' name fp $ \scm -> do
  test ("Should not validate" <> ": " <> fp) (liftEffect $ noProp gen (const true) scm)


-- "validation succeeded" property
yesProp :: Gen Json -> (Json -> Boolean) -> Schema -> Effect Unit
yesProp gen p scm = quickCheckGen $ do
  v <- gen
  pure $ prop v
  where
    prop v = toResult p v ==> toResult (isRight <<< runExcept <<< validate scm <<< Arg.stringify) v

-- "validation failed" property
noProp :: Gen Json -> (Json -> Boolean) -> Schema -> Effect Unit
noProp gen p scm = quickCheckGen $ do 
  v <- gen
  pure $ prop v
  where
    prop v = toResult p v ==> toResult (isLeft <<< runExcept <<< validate scm <<< Arg.stringify) v

-- Returns true iff the value is an object with the given property and the
-- property-value satisfies the predicate.
hasProperty :: String -> (Json -> Boolean) -> Json -> Boolean
hasProperty propName p j 
  = Arg.caseJsonObject 
    false  
    (\obj -> maybe false p $ Obj.lookup propName obj)
    j

-- Like hasProperty but is also true when the given property is absent.
hasOptionalProperty :: String -> (Json -> Boolean) -> Json -> Boolean
hasOptionalProperty propName p j
  = Arg.caseJsonObject 
    false
    (\obj -> maybe true p $ Obj.lookup propName obj)
    j

isNullOr :: (Json -> Boolean) -> (Json -> Boolean)
isNullOr f = (||) <$> Arg.isNull <*> f

testAny :: String -> String -> TestPlanM Unit
testAny fp name 
  = testWrap name fp
    (\result -> do
      test ("should parse: " <> fp) (result `shouldNotSatisfy` isParseError)
      test ("should build: " <> fp) (result `shouldNotSatisfy` isSchemaError)
    )
    (\scm -> do
         test ("should validate anything: " <> fp) ( liftEffect $ quickCheck (\rj -> (go scm $ rj) <?> "Test Failed for input" <> show rj))
    )
  where
    go scm (RandomJson j) = isRight $ runExcept <<< validate scm <<< Arg.stringify $ j 

testSingular :: String -> String -> (Json -> Boolean) -> TestPlanM Unit
testSingular fp name p = testWrap name fp
  (\result -> do
    test ("should parse: " <> fp) (result `shouldNotSatisfy` isParseError)
    test ("should build: " <> fp) (result `shouldNotSatisfy` isSchemaError)
  )
  (\scm -> do
    test ("should validate " <> name <> "s: " <> fp) (liftEffect $ yesProp genJson p scm )
    test ("should not validate non-" <> name <> "s: " <> fp) (liftEffect $ noProp genJson (not <<< p) scm)
  )
  where
    genJson = unsafeCoerce (arbitrary :: Gen RandomJson)

testStringVals :: String -> NonEmpty Array String -> TestPlanM Unit
testStringVals fp validStrings = 
  let name = "string is one of " <> show validStrings in
  testWrap' name fp
    (\scm -> do
      test ("Should Validate " <> name <> "s: " <> fp) (liftEffect $ quickCheckGen $ validationIsCorrect $ scm)
      test ("Shouldn't Validate " <> name <> "s: " <> fp) (liftEffect $ quickCheckGen $ invalidationIsCorrect $ scm)
    )
  where
    validationIsCorrect = validationTest identity isRight
    invalidationIsCorrect = validationTest not isLeft
    validationTest :: (Boolean -> Boolean) -> (forall a b. Either a b -> Boolean) -> Schema -> Gen Result
    validationTest resultPredicate eitherPredicate scm = do
      str <- genString
      let 
        stringArray = foldr cons [] validStrings
        isMember = str `elem` stringArray
        successPredicate = eitherPredicate <<< runExcept <<< validate scm <<< Arg.stringify <<< Arg.encodeJson
      pure $ toResult resultPredicate isMember ==> toResult (successPredicate) str
    genString :: Gen.Gen String
    genString = Gen.oneOf $ (Gen.elements validStrings) :| [ arbitrary ]


toResult :: forall a. (a -> Boolean) -> a -> Result
toResult f a = withHelp (f a) "failed predicate"
