module Test.Main where

import MedeaPrelude
import Data.Const (Const)
import Data.Foldable (sequence_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter (consoleReporter)
import Test.Parser as ParseTest
import Test.SchemaBuilder as SchemaBuilder
import Test.Validator as Validator
import TestM (TestPlanM)
import Mote (Plan, foldPlan, planT)

main :: Effect Unit
main = do
  launchAff_ $ interpret testPlan

interpret :: TestPlanM Unit -> Aff Unit
interpret spif = do
  plan <- planT $ spif
  let
    spec = go plan
  runSpec [ consoleReporter ] spec
  pure unit
  where
  go :: Plan (Const Void) (Aff Unit) -> Spec Unit
  go =
    foldPlan
      (\{ label, value } -> it label $ liftAff value)
      (\label -> pure unit)
      (\{ label, value } -> describe label (go $ value))
      sequence_

testPlan :: TestPlanM Unit
testPlan = do
  ParseTest.suite
  SchemaBuilder.suite
  Validator.suite
