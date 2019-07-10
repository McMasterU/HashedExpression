module SimplifyEval.ZeroRSpec where

import Commons
import Data.Map.Strict
import Data.Maybe (fromJust)
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
prop_SimplifyThenEval :: SuiteZeroR -> Bool
prop_SimplifyThenEval (SuiteZeroR exp valMaps) =
    eval valMaps exp ~=
    eval valMaps (simplify exp)

-- |
--
prop_Add :: SuiteZeroR -> SuiteZeroR -> (Bool, Bool, Bool) -> Bool
prop_Add (SuiteZeroR exp1 valMaps1) (SuiteZeroR exp2 valMaps2) (simplify1, simplify2, simplifySum) =
    eval valMaps exp1' +
    eval valMaps exp2' ~=
    eval valMaps expSum'
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

prop_Multiply :: SuiteZeroR -> SuiteZeroR -> (Bool, Bool, Bool) -> Bool
prop_Multiply (SuiteZeroR exp1 valMaps1) (SuiteZeroR exp2 valMaps2) (simplify1, simplify2, simplifyMul) =
    if lhs ~= rhs
        then True
        else error (show lhs ++ " not equal " ++ show rhs)
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
    lhs =
        eval valMaps exp1' *
        eval valMaps exp2'
    rhs = eval valMaps expMul'

prop_AddMultiply :: SuiteZeroR -> Bool
prop_AddMultiply (SuiteZeroR exp valMaps) =
    eval valMaps (simplify (exp + exp)) ~=
    eval valMaps (simplify (exp * const 2))

spec :: Spec
spec =
    describe "simplify & eval property for Zero R" $ do
        specify "evaluate must equals simplify then evaluate " $
            property prop_SimplifyThenEval
        specify "prop_Add" $ property prop_Add
        specify "prop_Multiply" $ property prop_Multiply
        specify "prop_AddMultiply" $ property prop_AddMultiply
