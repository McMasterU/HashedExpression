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

(~=) :: Double -> Double -> Property
a ~= b = (Prelude.abs (a - b) < 0.0001) === True

prop_SimplifyThenEval :: SuiteZeroR -> Property
prop_SimplifyThenEval (SuiteZeroR exp v0s str) =
    let valMaps = emptyVms |> withVm0 (fromList v0s)
     in (eval valMaps exp ~= eval valMaps (simplify exp))

spec :: Spec
spec =
    describe "simplify eval 0" $ do
        specify "evaluate must equals simplify then evaluate " $
            property prop_SimplifyThenEval
--        specify "haha" $ do
