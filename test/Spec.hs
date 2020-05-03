{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified SolverSpec
import qualified StructureSpec
import Test.Hspec
import Test.Hspec.Runner
import qualified Test1
import qualified Test2
import qualified ToCSpec
import ToCSpec (evaluateCodeC)
import Var

main1 = do
  let exp = Expression {exIndex = 7082896168417259, exMap = IM.fromList [(77528807, ([10], Var "d1")), (87820973, ([10], Var "v1")), (7082896168417259, ([], InnerProd R 87820973 77528807))]} :: Expression Scalar R
  let valMap =
        fromList
          [ ( "d1",
              V1D
                ( array
                    (0, 9)
                    [ (0, 1.5958172544605354),
                      (1, 1.314548693539582),
                      (2, 1.1786498848347708),
                      (3, 7.20375985180917),
                      (4, 7.1317920688760905),
                      (5, 7.204361565242639),
                      (6, 4.789975032287655),
                      (7, 9.475451912236897),
                      (8, 5.787377921728574),
                      (9, 8.978387621239188)
                    ]
                )
            ),
            ( "v1",
              V1D
                ( array
                    (0, 9)
                    [ (0, 0.16742794997767455),
                      (1, 2.8570668339167566),
                      (2, 6.280186667663374),
                      (3, 3.8329879757437313),
                      (4, 7.89320260393878),
                      (5, 4.602021181179035),
                      (6, 6.022001999697775),
                      (7, 3.158035052947157),
                      (8, 2.2984982879702223),
                      (9, 7.3226890759137095)
                    ]
                )
            )
          ]
  showExp $ normalize exp
  evaluateCodeC True (normalize exp) valMap
  print "DONE"

--main :: IO ()
main = hspecWith defaultConfig {configQuickCheckMaxSuccess = Just 50} spec

spec :: Spec
spec = do
  --  describe "HashedSolverSpec" SolverSpec.spec
  --  describe "NormalizeSpec" NormalizeSpec.spec
  --  describe "Test1" Test1.spec
  --  describe "Test2" Test2.spec
  --  describe "HashedInterpSpec" InterpSpec.spec
  --  describe "HashedCollectSpec" CollectSpec.spec
  describe "HashedToCSpec" ToCSpec.spec
--  describe "StructureSpec" StructureSpec.spec
--  describe "NormalizeEval.ScalarRSpec" ScalarRSpec.spec
--  describe "NormalizeEval.ScalarCSpec" ScalarCSpec.spec
--  describe "NormalizeEval.OneRSpec" OneRSpec.spec
--  describe "NormalizeEval.OneCSpec" OneCSpec.spec
