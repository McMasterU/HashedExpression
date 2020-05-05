{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

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
import HashedExpression.Interp
import HashedExpression.Operation hiding (product, sum)
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import qualified InterpSpec
import qualified NormalizeEval.OneCSpec as OneCSpec
import qualified NormalizeEval.OneRSpec as OneRSpec
import qualified NormalizeEval.ScalarCSpec as ScalarCSpec
import qualified NormalizeEval.ScalarRSpec as ScalarRSpec
import qualified NormalizeSpec
import qualified ProblemSpec
import qualified StructureSpec
import Test.Hspec
import Test.Hspec.Runner
import Var

--main = do
--  let exp = (Expression {rootID = 89536334, exMap = IM.fromList [(89536334,([10],Var "y1"))]}) ::
--        Expression Default1D R
--  let valMap = fromList [("y1",V1D (array (0,9) [(0,0.0),(1,0.0),(2,0.0),(3,0.0),(4,0.0),(5,0.0),(6,0.0),(7,0.0),(8,0.0),(9,0.0)]))]
--  showExp $ normalize exp
--  print $ eval valMap exp
--  (_, val) <- evaluateCodeC True (normalize exp) valMap
--  print $ val

main :: IO ()
main = hspecWith defaultConfig {configQuickCheckMaxSuccess = Just 50} spec

spec :: Spec
spec = do
  describe "ProblemSpec" ProblemSpec.spec
  describe "NormalizeSpec" NormalizeSpec.spec
  describe "HashedInterpSpec" InterpSpec.spec
  describe "HashedCollectSpec" CollectSpec.spec
  describe "CSimpleSpec" CSimpleSpec.spec
  describe "StructureSpec" StructureSpec.spec
  describe "NormalizeEval.ScalarRSpec" ScalarRSpec.spec
  describe "NormalizeEval.ScalarCSpec" ScalarCSpec.spec
  describe "NormalizeEval.OneRSpec" OneRSpec.spec
  describe "NormalizeEval.OneCSpec" OneCSpec.spec
