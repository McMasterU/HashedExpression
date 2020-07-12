{-# LANGUAGE DataKinds #-}

import qualified CSimpleSpec
import CSimpleSpec (evaluateCodeC)
import qualified CollectSpec
import Commons
import Data.Array.Unboxed as U
import qualified Data.IntMap as IM
import Data.Map (fromList, union)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import HashedExpression.Derivative
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Normalize
import HashedExpression.Internal.Utils
import HashedExpression.Internal.CollectDifferential
import HashedExpression.Interp
import HashedExpression.Operation hiding (product, sum)
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import qualified InterpSpec
import qualified NormalizeSpec
import qualified ProblemSpec
import qualified StructureSpec
import Test.Hspec
import Test.Hspec.Runner
import Var

--main :: IO ()
--main = do 
--  let x = variable2D @10 @10 "x"
--  let f = x <.> (constant2D @10 @10 2)
--  showExp f
--  showExp $ derivativeAllVars f

main :: IO ()
main = do
  hspecWith defaultConfig {configQuickCheckMaxSuccess = Just 100} $ do
    describe "ProblemSpec" ProblemSpec.spec
    describe "NormalizeSpec" NormalizeSpec.spec
    describe "HashedInterpSpec" InterpSpec.spec
    describe "HashedCollectSpec" CollectSpec.spec
    describe "StructureSpec" StructureSpec.spec
  hspecWith defaultConfig {configQuickCheckMaxSuccess = Just 10} $ do
    describe "CSimpleSpec" CSimpleSpec.spec
