module Test.SchemaBuilder where

import MedeaPrelude hiding (group)
import Control.Monad.Trans.Class (lift)
import Data.Medea.Loader (loadSchemaFromFile)
import Test.Spec.Assertions (shouldSatisfy)
import TestM.Util (listMedeaFiles)
import TestM (TestPlanM, isSchemaError, runTestM)

import Mote (group, test)

suite :: TestPlanM Unit
suite = do
  let prefix = "./conformance/schema-builder"
  testFilesPass <- lift $ listMedeaFiles (prefix <> "/pass")
  testFilesFail <- lift $ listMedeaFiles (prefix <> "/fail")
  group "invalid schemata cases" $ do
    traverse_ makeFailTest $ testFilesFail
  group "Valid schemata cases" $ do
    traverse_ makePassTest $ testFilesPass

makeFailTest :: String -> TestPlanM Unit
makeFailTest fp = do
  test (" Schema case (shouldn't build) :" <> fp) do
    result <- runTestM <<< loadSchemaFromFile $ fp
    result `shouldSatisfy` isSchemaError

makePassTest :: String -> TestPlanM Unit
makePassTest fp = do
  test (" Schema case (should build) :" <> fp) do
    result <- runTestM <<< loadSchemaFromFile $ fp
    result `shouldSatisfy` isRight
