module Test.Validator where 

import MedeaPrelude hiding (group)
import Control.Monad.Except (runExcept)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (Json)
import Data.Argonaut as Arg
import Data.Argonaut.Arbitrary (RandomJson(..))
import Data.Medea (validate)
import Data.Medea.Loader (loadSchemaFromFile)
import Mote (group, test)
import Test.QuickCheck.Combinators ((==>))
import Test.QuickCheck (Result, quickCheck, withHelp, (<?>))
import TestM (TestPlanM, isParseError, isSchemaError, runTestM)

import Test.Spec.Assertions (shouldNotSatisfy, fail)

import Effect.Class (liftEffect)

suite :: TestPlanM Unit
suite = do
  testAny 
    "./conformance/validation/any.medea" "Any Schema"
  testSingular 
    "./conformance/validation/null.medea" "Null Schema" Arg.isNull
  testSingular 
    "./conformance/validation/boolean.medea" "Boolean Schema" Arg.isBoolean
  testSingular 
    "./conformance/validation/number.medea" "Number Schema" Arg.isNumber
  testSingular 
    "./conformance/validation/string.medea" "String Schema" Arg.isString
  testSingular 
    "./conformance/validation/array.medea" "Array Schema" Arg.isArray
  testSingular 
    "./conformance/validation/object.medea" "Object Schema" Arg.isObject
  testSingular 
    "./conformance/validation/nullable-boolean.medea" 
    "Boolean/null Schema" 
    (isNullOr Arg.isBoolean)
  testSingular 
    "./conformance/validation/nullable-number.medea" 
    "Number Schema" 
    (isNullOr Arg.isNumber)
  testSingular 
    "./conformance/validation/nullable-string.medea" 
    "String Schema" 
    (isNullOr Arg.isString)
  testSingular 
    "./conformance/validation/nullable-array.medea" 
    "Array Schema" 
    (isNullOr Arg.isArray)
  testSingular 
    "./conformance/validation/nullable-object.medea" 
    "Object Schema" 
    (isNullOr Arg.isObject)


-- helpers
isNullOr :: (Json -> Boolean) -> (Json -> Boolean)
isNullOr f = (||) <$> Arg.isNull <*> f

testAny :: String -> String -> TestPlanM Unit
testAny fp  name = do
  result <- lift $ runTestM $ loadSchemaFromFile fp
  -- due to Mote and Spec preventing Effects from within a group/describe, 
  -- we need to execute effects outside the group or inside the test only
  group name $ do
    test ("should parse: " <> fp) (result `shouldNotSatisfy` isParseError)
    test ("should build: " <> fp) (result `shouldNotSatisfy` isSchemaError)
    case result of 
      Left _ -> test ("Not Left " <> name <> " file: " <> fp) (fail "unexpected Left")
      Right scm -> do
         test ("should validate anything: " <> fp) ( liftEffect $ quickCheck (\rj -> (go scm $ rj) <?> "Test Failed for input" <> show rj))
  where
    go scm (RandomJson j) = isRight $ runExcept <<< validate scm <<< Arg.stringify $ j 

testSingular :: String -> String -> (Json -> Boolean) -> TestPlanM Unit
testSingular fp name p = do
  result <- lift $ runTestM $ loadSchemaFromFile fp
  group name $ do
    test ("should parse: " <> fp) (result `shouldNotSatisfy` isParseError)
    test ("should build: " <> fp) (result `shouldNotSatisfy` isSchemaError)
    case result of 
      Left _ -> test ("Not Left " <> name <> " file: " <> fp) (fail "unexpected Left")
      Right scm -> do
        test ("should validate " <> name <> "s: " <> fp) (liftEffect $ quickCheck (\rj -> yesProp scm rj))
        test ("should not validate non-" <> name <> "s: " <> fp) (liftEffect $ quickCheck (\rj -> noProp scm rj))
  where
    yesProp scm (RandomJson j) = toResult p j ==> toResult (isRight <<< runExcept <<< validate scm <<< Arg.stringify) j
    noProp scm (RandomJson j) = toResult (not <<< p) j ==> toResult (isLeft <<< runExcept <<< validate scm <<< Arg.stringify) j


toResult :: forall a. (a -> Boolean) -> a -> Result
toResult f a = withHelp (f a) "failed predicate"
