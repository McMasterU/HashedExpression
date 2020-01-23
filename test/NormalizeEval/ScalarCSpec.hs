module NormalizeEval.ScalarCSpec where

import Commons
import Control.Monad (replicateM_)
import qualified Data.IntMap.Strict as IM
import Data.Map.Strict
import Data.Maybe (fromJust)
import Data.Typeable (Typeable)
import Debug.Trace (traceShow, traceShowId)
import GHC.IO.Unsafe (unsafePerformIO)
import HashedExpression.Expression

import HashedExpression.Interp
import HashedExpression.Normalize
import HashedExpression.Operation hiding (product, sum)
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import HashedExpression.Utils
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
prop_NormalizeThenEval :: SuiteScalarC -> Bool
prop_NormalizeThenEval (SuiteScalarC exp valMaps) =
    eval valMaps exp ~= eval valMaps (normalize exp)

-- |
--
prop_Add :: SuiteScalarC -> SuiteScalarC -> (Bool, Bool, Bool) -> Bool
prop_Add (SuiteScalarC exp1 valMaps1) (SuiteScalarC exp2 valMaps2) (normalize1, normalize2, normalizeSum) =
    eval valMaps exp1' + eval valMaps exp2' ~= eval valMaps expSum'
  where
    valMaps = union valMaps1 valMaps2
    exp1'
        | normalize1 = normalize exp1
        | otherwise = exp1
    exp2'
        | normalize2 = normalize exp2
        | otherwise = exp2
    expSum'
        | normalizeSum = normalize (exp1 + exp2)
        | otherwise = exp1 + exp2

prop_Multiply :: SuiteScalarC -> SuiteScalarC -> (Bool, Bool, Bool) -> Bool
prop_Multiply (SuiteScalarC exp1 valMaps1) (SuiteScalarC exp2 valMaps2) x@(normalize1, normalize2, normalizeMul) =
    if eval valMaps exp1' * eval valMaps exp2' ~= eval valMaps expMul'
        then True
        else error $
             prettifyDebug exp1' ++
             "\n-----------\n" ++
             prettifyDebug exp2' ++
             "\n-----------\n" ++
             show valMaps ++
             "\n-----------n" ++ show lhs ++ " not equals " ++ show rhs
  where
    lhs = eval valMaps exp1' * eval valMaps exp2'
    rhs = eval valMaps expMul'
    valMaps = union valMaps1 valMaps2
    exp1'
        | normalize1 = normalize exp1
        | otherwise = exp1
    exp2'
        | normalize2 = normalize exp2
        | otherwise = exp2
    expMul'
        | normalizeMul = normalize (exp1 * exp2)
        | otherwise = exp1 * exp2

prop_AddMultiply :: SuiteScalarC -> Bool
prop_AddMultiply (SuiteScalarC exp valMaps) =
    eval valMaps (normalize (exp + exp)) ~=
    eval valMaps (normalize (const 2 *. exp))

spec :: Spec
spec =
    describe "normalize & eval property for Scalar C" $ do
        specify "evaluate must equals normalize then evaluate " $
            property prop_NormalizeThenEval
        specify "prop_Add" $ property prop_Add
        specify "prop_Multiply" $ property prop_Multiply
        specify "prop_AddMultiply" $ property prop_AddMultiply
