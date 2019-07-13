import Data.Array.Unboxed as U
import Data.Map (fromList, union)
import qualified Data.Set as Set
import HashedDerivative
import HashedExpression
import HashedInterp
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
import HashedSimplify
import qualified HashedSimplifySpec
import qualified HashedToCSpec
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
    , tan
    , tanh
    )
import qualified SimplifyEval.ZeroCSpec as ZeroCSpec
import qualified SimplifyEval.ZeroRSpec as ZeroRSpec
import qualified SimplifyEval.OneRSpec as OneRSpec
import qualified Test1
import qualified Test2

import Commons
import Data.Maybe (fromJust)
import HashedUtils ((|>))
import HashedVar
import Test.Hspec

main :: IO ()
main = do
    hspec spec

spec :: Spec
spec = do
    describe "SimplifyEval.OneRSpec" OneRSpec.spec
    describe "SimplifyEval.ZeroRSpec" ZeroRSpec.spec
    describe "SimplifyEval.ZeroCSpec" ZeroCSpec.spec
    describe "SimplifySpec" HashedSimplifySpec.spec
    describe "Test1" Test1.spec
    describe "Test1" Test2.spec
    describe "HashedToCSpec" HashedToCSpec.spec
