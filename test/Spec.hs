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
import qualified Test1
import qualified Test2
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
import qualified HashedSimplifySpec
import qualified HashedToCSpec

import Commons ((~=))
import Data.Maybe (fromJust)
import HashedUtils ((|>))
import HashedVar
import Test.Hspec

main :: IO ()
main = do
    hspec spec

--    haha
--    haha
spec :: Spec
spec = do
    describe "Test1" Test1.spec
--    describe "SimplifyEval.ZeroRSpec" ZeroRSpec.spec
--    describe "SimplifyEval.ZeroCSpec" ZeroCSpec.spec
--    describe "SimplifySpec" HashedSimplifySpec.spec
--    describe "Test2" Test2.spec
--    describe "HashedToCSpec" HashedToCSpec.spec

--haha = do
--    let f = negate (y +: m) + (y +: g)
--    let g = one +: (const 12000 * d * n * q * m * k * m * q)
--    let valMap =
--            fromList
--                [ ("d", 58.599726238270165)
--                , ("e", -22.479418298271167)
--                , ("g", 36.886364410147706)
--                , ("j", 33.10357980099519)
--                , ("k", -40.9584460427426)
--                , ("m", 32.199218020098456)
--                , ("n", 13.85852371892153)
--                , ("q", -46.90808938878782)
--                , ("y", 48.628302220316655)
--                ]
--    putStrLn "\n"
--    showExp $ simplify f
--    showExp $ simplify g
--    showExp $ simplify (f * g)
--    let simplifiedF = simplify f
--    let fVal = eval (emptyVms |> withVm0 valMap) simplifiedF
--    let gVal = eval (emptyVms |> withVm0 valMap) $ simplify g
--    let prodVal = eval (emptyVms |> withVm0 valMap) (simplify $ f * g)
--    print $ fVal * gVal
--    print $ prodVal
--    print $ fVal * gVal ~= prodVal
