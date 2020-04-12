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
  testFiles <- lift $ listMedeaFiles prefix
  group "invalid schemata cases" $ do
    traverse_ makeSchemaTest $ testFiles

makeSchemaTest :: String -> TestPlanM Unit
makeSchemaTest fp = do
  test (" Schema case (shouldn't build) :" <> fp) do
    result <- runTestM <<< loadSchemaFromFile $ fp
    result `shouldSatisfy` isSchemaError


