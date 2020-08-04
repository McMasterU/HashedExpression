module ReverseDifferentiationSpec where

import CSimpleSpec (evaluateCodeC)
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
import HashedExpression.Internal (D_, ET_, removeUnreachable, unwrap)
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
  -- computed by exterior method
  let (eMP, eMap) = partialDerivativesMapByExterior exp
  -- computed by reverse method
  let (rMP, rMap) = partialDerivativesMapByReverse exp
  -- computed by reverse method of the normalized expression
  let (rMPN, rMapN) = partialDerivativesMapByReverse $ normalize exp
  forM_ (Map.toList $ zipMp3 eMap rMap rMapN) $ \(name, (eID, rID, rIDN)) -> do
    retrieveShape eID eMP `shouldBe` retrieveShape rID rMP
    let shape = retrieveShape eID eMP
    case shape of
      [] -> do
        let valE = eval valMap (Expression @Scalar @R eID eMP)
        let valR = eval valMap (Expression @Scalar @R rID rMP)
        let valRN = eval valMap (Expression @Scalar @R rIDN rMPN)
        valRN `shouldApprox` valR
        valR `shouldApprox` valE
      [sz] -> do
        let valE = evaluate1DReal valMap (eMP, eID)
        let valR = evaluate1DReal valMap (rMP, rID)
        let valRN = evaluate1DReal valMap (rMPN, rIDN)
        valRN `shouldApprox` valR
        valE `shouldApprox` valR
      [sz1, sz2] -> do
        let valE = evaluate2DReal valMap (eMP, eID)
        let valR = evaluate2DReal valMap (rMP, rID)
        let valRN = evaluate2DReal valMap (rMPN, rIDN)
        valRN `shouldApprox` valR
        valE `shouldApprox` valR

spec :: Spec
spec =
  describe "Reverse differentiation spec" $ do
    specify "should be the same as exterior method" $ do
      property prop_reverseMethodAndExteriorShouldBeSameValue
