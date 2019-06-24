module SimplifyEvalSpec where

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
import TestCommons

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
            , ("Eval", show evalExp)
            , ("EvalSimplified", show evalSimplified)
            , ("ValMap", show valMap)
            , ("Abs", show . abs $ evalExp - evalSimplified)
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

prop_SimplifyThenEval :: SuiteZeroR -> Bool
prop_SimplifyThenEval (SuiteZeroR exp valMap) =
    eval (emptyVms |> withVm0 valMap) exp ~=
    eval (emptyVms |> withVm0 valMap) (simplify exp)

spec :: Spec
spec =
    describe "simplify eval 0" $ do
        specify "evaluate must equals simplify then evaluate " $
            property prop_SimplifyThenEval
--        specify "haha" $ do
