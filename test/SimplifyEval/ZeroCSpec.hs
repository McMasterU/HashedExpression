module SimplifyEval.ZeroCSpec where

import Commons
import Data.Map.Strict
import Data.Maybe (fromJust)
import Data.Typeable (Typeable)
import Debug.Trace (traceShowId)
import GHC.IO.Unsafe (unsafePerformIO)
import HashedExpression
import HashedInterp
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
import HashedSimplify
import HashedUtils
import Prelude hiding
    ( (*)
    , (+)
    , (-)
    , (/)
    , acos
    , acosh
    , asin
    , asinh
    , atan
    , atanh
    , const
    , cos
    , cosh
    , exp
    , log
    , negate
    , product
    , sin
    , sinh
    , sqrt
    , sum
    , sum
    , tan
    , tanh
    )
import Test.Hspec
import Test.QuickCheck


-- |
--
prop_SimplifyThenEval :: SuiteZeroC -> Bool
prop_SimplifyThenEval (SuiteZeroC exp valMap) =
    eval (emptyVms |> withVm0 valMap) exp ~=
    eval (emptyVms |> withVm0 valMap) (simplify exp)

-- |
--
prop_Add :: SuiteZeroC -> SuiteZeroC -> (Bool, Bool, Bool) -> Bool
prop_Add (SuiteZeroC exp1 valMap1) (SuiteZeroC exp2 valMap2) (simplify1, simplify2, simplifySum) =
    eval (emptyVms |> withVm0 valMap) exp1' +
    eval (emptyVms |> withVm0 valMap) exp2' ~=
    eval (emptyVms |> withVm0 (valMap1 `union` valMap)) expSum'
  where
    valMap = valMap1 `union` valMap2
    exp1' =
        if simplify1
            then simplify exp1
            else exp1
    exp2' =
        if simplify2
            then simplify exp2
            else exp2
    expSum' =
        if simplifySum
            then simplify (exp1 + exp2)
            else exp1 + exp2

prop_Multiply :: SuiteZeroC -> SuiteZeroC -> (Bool, Bool, Bool) -> Bool
prop_Multiply (SuiteZeroC exp1 valMap1) (SuiteZeroC exp2 valMap2) (simplify1, simplify2, simplifyMul) =
    eval (emptyVms |> withVm0 valMap) exp1' *
     eval (emptyVms |> withVm0 valMap) exp2' ~=
    eval (emptyVms |> withVm0 (valMap1 `union` valMap)) expMul'
  where
    valMap = valMap1 `union` valMap2
    exp1' =
        if simplify1
            then simplify exp1
            else exp1
    exp2' =
        if simplify2
            then simplify exp2
            else exp2
    expMul' =
        if simplifyMul
            then simplify (exp1 * exp2)
            else exp1 * exp2

prop_AddMultiply :: SuiteZeroC -> Bool
prop_AddMultiply (SuiteZeroC exp valMap) =
    eval (emptyVms |> withVm0 valMap) (simplify (exp + exp)) ~=
    eval (emptyVms |> withVm0 valMap) (simplify (const 2 *. exp))

spec :: Spec
spec =
    describe "simplify & eval property for Zero C" $ do
        specify "evaluate must equals simplify then evaluate " $
            property prop_SimplifyThenEval
        specify "prop_Add" $ property prop_Add
        specify "prop_Multiply" $ property prop_Multiply
        specify "prop_AddMultiply" $ property prop_AddMultiply
