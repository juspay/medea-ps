module Test.Parser where

import MedeaPrelude hiding (group)
import Control.Monad.Trans.Class (lift)
import Test.Spec.Assertions (shouldSatisfy, shouldNotSatisfy)
import TestM (isParseError, runTestM, TestPlanM, listMedeaFiles)

import Data.Medea.Loader(LoaderError, loadSchemaFromFile)
import Data.Medea.Schema (Schema)

import Mote (group, test)


suite :: TestPlanM Unit
suite = do
  let 
    failDir = "./conformance/parser/fail"
    passDir = "./conformance/parser/pass"
  failTestFiles <- lift $ listMedeaFiles failDir
  passTestFiles <- lift $ listMedeaFiles passDir
  group "invalid parse cases" $ do
    traverse_ makeParseTestsFail failTestFiles
  group "valid parse cases" $ do
    traverse_ makeParseTestsPass passTestFiles

type ParseResult = Either LoaderError Schema

makeParseTestsFail ::  String -> TestPlanM Unit
makeParseTestsFail fp = do
  test ("parse case: " <> fp) do
     result <- runTestM <<< loadSchemaFromFile $ fp
     result `shouldSatisfy` isParseError


makeParseTestsPass ::  String -> TestPlanM Unit
makeParseTestsPass fp = do
  test ("parse case: " <> fp) do
     result <- runTestM <<< loadSchemaFromFile $ fp
     result `shouldNotSatisfy` isParseError

