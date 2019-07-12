module SimplifyEval.ZeroCSpec where

import Commons
import Data.Map.Strict
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromJust)
import Data.Typeable (Typeable)
import Debug.Trace (traceShowId, traceShow)
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
prop_SimplifyThenEval (SuiteZeroC exp valMaps) =
    eval valMaps exp ~= eval valMaps (simplify exp)

-- |
--
prop_Add :: SuiteZeroC -> SuiteZeroC -> (Bool, Bool, Bool) -> Bool
prop_Add (SuiteZeroC exp1 valMaps1) (SuiteZeroC exp2 valMaps2) (simplify1, simplify2, simplifySum) =
    eval valMaps exp1' + eval valMaps exp2' ~= eval valMaps expSum'
  where
    valMaps = mergeValMaps valMaps1 valMaps2
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
prop_Multiply (SuiteZeroC exp1 valMaps1) (SuiteZeroC exp2 valMaps2) x@(simplify1, simplify2, simplifyMul) =
    if eval valMaps exp1' * eval valMaps exp2' ~= eval valMaps expMul'
        then True
        else error $
             prettifyDebug exp1' ++
             prettifyDebug exp2'
  where
    valMaps = mergeValMaps valMaps1 valMaps2
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
prop_AddMultiply (SuiteZeroC exp valMaps) =
    eval valMaps (simplify (exp + exp)) ~=
    eval valMaps (simplify (const 2 *. exp))

spec :: Spec
spec =
    describe "simplify & eval property for Zero C" $ do
        specify "evaluate must equals simplify then evaluate " $
            property prop_SimplifyThenEval
        specify "prop_Add" $ property prop_Add
        specify "prop_Multiply" $ property prop_Multiply
        specify "prop_AddMultiply" $ property prop_AddMultiply
