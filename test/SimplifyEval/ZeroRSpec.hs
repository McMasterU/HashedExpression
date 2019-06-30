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

infix 4 ~=

(~=) :: Double -> Double -> Bool
a ~= b
    | a == b = True
    | otherwise = relativeError a b < 0.001

-- |
--
data SuiteZeroR =
    SuiteZeroR (Expression Zero R) (Map String Double)

instance Show SuiteZeroR where
    show (SuiteZeroR e valMap) =
        format
            [ ("Expr", exp)
            , ("Simplified", simplifiedExp)
            , ("ValMap", show valMap)
            ]
      where
        exp = prettify e
        simplifiedExp = prettify . simplify $ e
        evalExp = eval (emptyVms |> withVm0 valMap) e
        evalSimplified = eval (emptyVms |> withVm0 valMap) $ simplify e

instance Arbitrary SuiteZeroR where
    arbitrary = do
        (exp, names) <- genZeroR
        doubles <- vectorOf (length names) arbitrary
        let valMap = fromList $ zip names doubles
        return $ SuiteZeroR exp valMap

-- |
--
prop_SimplifyThenEval :: SuiteZeroR -> Bool
prop_SimplifyThenEval (SuiteZeroR exp valMap) =
    eval (emptyVms |> withVm0 valMap) exp ~=
    eval (emptyVms |> withVm0 valMap) (simplify exp)

-- |
--
prop_Add :: SuiteZeroR -> SuiteZeroR -> (Bool, Bool, Bool) -> Bool
prop_Add (SuiteZeroR exp1 valMap1) (SuiteZeroR exp2 valMap2) (simplify1, simplify2, simplifySum) =
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

prop_Multiply :: SuiteZeroR -> SuiteZeroR -> (Bool, Bool, Bool) -> Bool
prop_Multiply (SuiteZeroR exp1 valMap1) (SuiteZeroR exp2 valMap2) (simplify1, simplify2, simplifyMul) =
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

prop_AddMultiply :: SuiteZeroR -> Bool
prop_AddMultiply (SuiteZeroR exp valMap) =
    eval (emptyVms |> withVm0 valMap) (simplify (exp + exp)) ~=
    eval (emptyVms |> withVm0 valMap) (simplify (exp * const 2))

spec :: Spec
spec =
    describe "simplify eval 0" $ do
        specify "evaluate must equals simplify then evaluate " $
            property prop_SimplifyThenEval
        specify "prop_Add" $ property prop_Add
        specify "prop_Multiply" $ property prop_Multiply
        specify "prop_AddMultiply" $ property prop_AddMultiply
