{-# LANGUAGE DataKinds #-}

import qualified CSimpleSpec
import qualified CollisionSpec
import qualified InterpSpec
import qualified ProblemSpec
import qualified ReverseDifferentiationSpec
import qualified SimplifySpec
import qualified SolverSpec
import qualified StructureSpec
import Test.Hspec
import Test.Hspec.Runner
import Prelude hiding ((^))

main :: IO ()
main = do
  hspecWith defaultConfig {configQuickCheckMaxSuccess = Just 2000} $ do
    describe "SimplifySpec" SimplifySpec.spec
  hspecWith defaultConfig {configQuickCheckMaxDiscardRatio = Just 100, configQuickCheckMaxSuccess = Just 100} $ do
    describe "CollisionSpec" CollisionSpec.spec
    describe "ProblemSpec" ProblemSpec.spec
    describe "InterpSpec" InterpSpec.spec
    describe "StructureSpec" StructureSpec.spec
    describe "ReverseDifferentiationSpec" ReverseDifferentiationSpec.spec
  hspecWith defaultConfig {configQuickCheckMaxSuccess = Just 10} $ do
    describe "SolverSpec" SolverSpec.spec
  hspecWith defaultConfig {configQuickCheckMaxSuccess = Just 60} $ do
    describe "CSimpleSpec" CSimpleSpec.spec
