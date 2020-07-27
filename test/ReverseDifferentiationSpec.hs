module ReverseDifferentiationSpec where

import Commons
import Control.Applicative (liftA2)
import Control.Monad
import Control.Monad (replicateM_, unless)
import qualified Data.IntMap.Strict as IM
import Data.List (group, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Tuple.Extra (thd3)
import Debug.Trace (traceShow)
import HashedExpression.Differentiation.Exterior
import HashedExpression.Differentiation.Exterior.Derivative
import HashedExpression.Differentiation.Reverse
import HashedExpression.Internal (D_, ET_, unwrap)
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Node
import HashedExpression.Internal.Normalize
import HashedExpression.Internal.Structure
import HashedExpression.Internal.Utils
import HashedExpression.Interp
import HashedExpression.Operation hiding (product, sum)
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import Test.HUnit (assertBool)
import Test.Hspec
import Test.QuickCheck
import Var
import Prelude hiding ((^))
import qualified Prelude

prop_reverseMethodAndExteriorShouldBeSameValue :: SuiteScalarR -> Expectation
prop_reverseMethodAndExteriorShouldBeSameValue (Suite exp valMap) = do
  --  print "---------------------"
  --  showExp exp
  let (eMP, eMap) = partialDerivativesMapByExterior exp
  let (rMP, rMap) = partialDerivativesMapByReverse exp
  forM_ (Map.toList $ zipMp eMap rMap) $ \(name, (eID, rID)) -> do
    --    putStrLn $ "for: " ++ name
    --    putStrLn $ debugPrint (eMP, eID)
    --    putStrLn $ debugPrint (rMP, rID)
    retrieveShape eID eMP `shouldBe` retrieveShape rID rMP
    let shape = retrieveShape eID eMP
    case shape of
      [] -> do
        let valE = eval valMap (Expression @Scalar @R eID eMP)
        let valR = eval valMap (Expression @Scalar @R rID rMP)
        valE `shouldApprox` valR
      [sz] -> do
        let valE = evaluate1DReal valMap (eMP, eID)
        let valR = evaluate1DReal valMap (rMP, rID)
        valE `shouldApprox` valR
      [sz1, sz2] -> do
        let valE = evaluate2DReal valMap (eMP, eID)
        let valR = evaluate2DReal valMap (rMP, rID)
        valE `shouldApprox` valR

spec :: Spec
spec =
  describe "Reverse differentiation spec" $ do
    specify "should be the same as exterior method" $ do
      property prop_reverseMethodAndExteriorShouldBeSameValue

--      let f = xRe ((x1 +: y1) <.> (y1 +: z1))
--      showExp f
--      showExp $ collectDifferentials . derivativeAllVars $ f
--      let (mp, pd) = compute f
--      forM_ (Map.toList pd) $ \(name, pID) -> do
--        print $ name ++ ": " ++ debugPrint (mp, pID)
