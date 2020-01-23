module NormalizeEval.ScalarRSpec where

import Commons
import Data.Map.Strict
import Data.Maybe (fromJust)
import HashedExpression.Expression

import HashedExpression.Interp
import HashedNormalize
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
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
prop_NormalizeThenEval :: SuiteScalarR -> Bool
prop_NormalizeThenEval (SuiteScalarR exp valMaps) =
    eval valMaps exp ~= eval valMaps (normalize exp)

-- |
--
prop_Add :: SuiteScalarR -> SuiteScalarR -> (Bool, Bool, Bool) -> Bool
prop_Add (SuiteScalarR exp1 valMaps1) (SuiteScalarR exp2 valMaps2) (normalize1, normalize2, normalizeSum) =
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

prop_Multiply :: SuiteScalarR -> SuiteScalarR -> (Bool, Bool, Bool) -> Bool
prop_Multiply (SuiteScalarR exp1 valMaps1) (SuiteScalarR exp2 valMaps2) (normalize1, normalize2, normalizeMul) =
    if lhs ~= rhs
        then True
        else error
                 (prettify exp1' ++
                  " * " ++
                  prettify exp2' ++
                  " not ~= " ++
                  prettify expMul' ++
                  " ----- " ++
                  show lhs ++ " " ++ show rhs ++ " " ++ show valMaps)
  where
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
    lhs = eval valMaps exp1' * eval valMaps exp2'
    rhs = eval valMaps expMul'

prop_AddMultiply :: SuiteScalarR -> Bool
prop_AddMultiply (SuiteScalarR exp valMaps) =
    eval valMaps (normalize (exp + exp)) ~=
    eval valMaps (normalize (exp * const 2))

spec :: Spec
spec =
    describe "normalize & eval property for Scalar R" $ do
        specify "evaluate must equals normalize then evaluate " $
            property prop_NormalizeThenEval
        specify "prop_Add" $ property prop_Add
        specify "prop_Multiply" $ property prop_Multiply
        specify "prop_AddMultiply" $ property prop_AddMultiply
