{-# LANGUAGE DataKinds #-}

import CSimpleSpec (evaluateCodeC)
import qualified CSimpleSpec
import qualified CollisionSpec
import qualified Data.IntMap as IM
import Data.Map (fromList, union)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified HashedExpression.Operation
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
  hspecWith defaultConfig {configQuickCheckMaxDiscardRatio = Just 100, configQuickCheckMaxSuccess = Just 100} $ do
    describe "SimplifySpec" SimplifySpec.spec
    describe "CollisionSpec" CollisionSpec.spec
    describe "ProblemSpec" ProblemSpec.spec
    describe "InterpSpec" InterpSpec.spec
    describe "StructureSpec" StructureSpec.spec
    describe "ReverseDifferentiationSpec" ReverseDifferentiationSpec.spec
  hspecWith defaultConfig {configQuickCheckMaxSuccess = Just 10} $ do
    describe "SolverSpec" SolverSpec.spec
  hspecWith defaultConfig {configQuickCheckMaxSuccess = Just 10} $ do
    describe "CSimpleSpec" CSimpleSpec.spec
