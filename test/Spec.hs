{-# LANGUAGE DataKinds #-}

import CSimpleSpec (evaluateCodeC)
import qualified CSimpleSpec
import qualified CollectSpec
import qualified CollisionSpec
import Commons
import Data.Array.Unboxed as U
import qualified Data.IntMap as IM
import Data.Map (fromList, union)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import HashedExpression.Differentiation.Exterior.Collect
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Normalize
import HashedExpression.Internal.Utils
import HashedExpression.Interp
import HashedExpression.Operation hiding (product, sum)
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import qualified InterpSpec
import qualified NormalizeSpec
import qualified ProblemSpec
import qualified ReverseDifferentiationSpec
import qualified StructureSpec
import Test.Hspec
import Test.Hspec.Runner
import Var
import Prelude hiding ((^))

--main :: IO ()
--main = do
--  let exp = (Expression {exRootID = 2029289003279401, exMap = IM.fromList [(35219439,([],R,Var "m")),(36934800,([],R,Var "p")),(40937309,([],R,Var "w")),(1434098621427159,([],R,Power 2 40937309)),(2029289003279401,([],R,Scale 1434098621427159 7227349723491681)),(7227349723491681,([],R,Piecewise [24.675817653418964] 36934800 [35219439,40937309]))]}) :: Expression Scalar R
--  showExp $ collectDifferentials . derivativeAllVars $ exp

--  ((((piecewise [24.675817653418964] p [0.0, 1.0])*(w^2))+()+(2.0*(piecewise [24.675817653418964] p [0.0, 1.0])*(w^2)))|*|dw)

main :: IO ()
main = do
  hspecWith defaultConfig {configQuickCheckSeed = Just 2097370412, configQuickCheckMaxSuccess = Just 100} $ do
    describe "CollisionSpec" CollisionSpec.spec
    describe "ProblemSpec" ProblemSpec.spec
    describe "NormalizeSpec" NormalizeSpec.spec
    describe "HashedInterpSpec" InterpSpec.spec
    describe "HashedCollectSpec" CollectSpec.spec
    describe "StructureSpec" StructureSpec.spec
    describe "ReverseDifferentiationSpec" ReverseDifferentiationSpec.spec
  hspecWith defaultConfig {configQuickCheckMaxSuccess = Just 20} $ do
    describe "CSimpleSpec" CSimpleSpec.spec
