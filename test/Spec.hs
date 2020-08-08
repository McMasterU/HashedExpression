{-# LANGUAGE DataKinds #-}

import CSimpleSpec (evaluateCodeC)
import qualified CSimpleSpec
import qualified CollisionSpec
import Commons
import Data.Array.Unboxed as U
import qualified Data.IntMap as IM
import Data.Map (fromList, union)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Utils
import HashedExpression.Interp
import HashedExpression.Operation hiding (product, sum)
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import qualified InterpSpec
import qualified ProblemSpec
import qualified ReverseDifferentiationSpec
import qualified SimplifySpec
import qualified SolverSpec
import qualified StructureSpec
import Test.Hspec
import Test.Hspec.Runner
import Var
import Prelude hiding ((^))

main :: IO ()
main = do
  hspecWith defaultConfig {configQuickCheckMaxSuccess = Just 100} $ do
    describe "SimplifySpec" SimplifySpec.spec
    describe "CollisionSpec" CollisionSpec.spec
    describe "ProblemSpec" ProblemSpec.spec
    describe "HashedInterpSpec" InterpSpec.spec
    describe "StructureSpec" StructureSpec.spec
    describe "ReverseDifferentiationSpec" ReverseDifferentiationSpec.spec
  hspecWith defaultConfig {configQuickCheckMaxSuccess = Just 10} $ do
    describe "SolverSpec" SolverSpec.spec
  hspecWith defaultConfig {configQuickCheckMaxSuccess = Just 20} $ do
    describe "CSimpleSpec" CSimpleSpec.spec
