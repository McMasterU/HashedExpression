module ReverseDifferentiationSpec where

import CSimpleSpec (evaluateCodeC)
import Commons
import Control.Applicative (liftA2)
import Control.Monad (forM_)
import qualified Data.IntMap.Strict as IM
import Data.List (group, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Tuple.Extra (thd3)
import Debug.Trace (traceShow)
import HashedExpression.Differentiation.Reverse
import HashedExpression.Internal (removeUnreachable, unwrap)
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Node
import HashedExpression.Utils
import HashedExpression.Interp
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import HashedExpression.Value
import Test.HUnit (assertBool)
import Test.Hspec
import Test.QuickCheck
import Var
import Prelude hiding ((^))
import qualified Prelude

--sharedVariables :: ValMap -> ValMap -> [String]
--sharedVariables mp1 mp2 = filter isVar $ Set.toList (Map.keysSet mp1 `Set.intersection` Map.keysSet mp2)
--  where
--    isVar name = 'p' `notElem` name
--
--prop_Linearity :: SuiteScalarR -> SuiteScalarR -> Property
--prop_Linearity (Suite f valMap1) (Suite g valMap2) =
--  sharedVariables valMap1 valMap2 /= [] ==> do
--    let svs = sharedVariables valMap1 valMap2
--    let valMap = Map.union valMap1 valMap2
--    let (mpSum, pdSum) = partialDerivativesMap (f + g)
--    let (mpF, pdF) = partialDerivativesMap f
--    let (mpG, pdG) = partialDerivativesMap g
--    forM_ svs $ \name -> do
--      let pIDSum = fromJust $ Map.lookup name pdSum
--      let pIDF = fromJust $ Map.lookup name pdF
--      let pIDG = fromJust $ Map.lookup name pdG
--      case retrieveShape pIDSum mpSum of
--        [] -> do
--          let val1 = eval valMap (Expression @Scalar @R pIDF mpF)
--          let val2 = eval valMap (Expression @Scalar @R pIDG mpG)
--          let valSum = eval valMap (Expression @Scalar @R pIDSum mpSum)
--          val1 + val2 `shouldApprox` valSum
--        [sz] -> do
--          let val1 = evaluate1DReal valMap (mpF, pIDF)
--          let val2 = evaluate1DReal valMap (mpG, pIDG)
--          let valSum = evaluate1DReal valMap (mpSum, pIDSum)
--          val1 + val2 `shouldApprox` valSum
--        [sz1, sz2] -> do
--          let val1 = evaluate2DReal valMap (mpF, pIDF)
--          let val2 = evaluate2DReal valMap (mpG, pIDG)
--          let valSum = evaluate2DReal valMap (mpSum, pIDSum)
--          val1 + val2 `shouldApprox` valSum

spec :: Spec
spec =
  describe "Reverse differentiation spec" $ do
    specify "linearity" $ pending

--    specify "linearity" $ property prop_Linearity
