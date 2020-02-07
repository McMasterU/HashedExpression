{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

import qualified CollectSpec
import Data.Array.Unboxed as U
import Data.Map (fromList, union)
import qualified Data.Set as Set
import HashedExpression.Derivative
import HashedExpression.Internal.Expression

import HashedExpression.Internal.Normalize
import HashedExpression.Interp
import HashedExpression.Operation hiding (product, sum)
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import qualified InterpSpec
import qualified NormalizeEval.OneCSpec as OneCSpec
import qualified NormalizeEval.OneRSpec as OneRSpec
import qualified NormalizeEval.ScalarCSpec as ScalarCSpec
import qualified NormalizeEval.ScalarRSpec as ScalarRSpec
import qualified NormalizeSpec
import Prelude hiding
    ( (*)
    , (+)
    , (-)
    , (/)
    , (^)
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
import qualified SolverSpec
import qualified StructureSpec
import qualified Test1
import qualified Test2
import qualified ToCSpec

import Commons
import Data.Maybe (fromJust)
import HashedExpression.Internal.Utils
import Test.Hspec
import Test.Hspec.Runner
import Var

--main :: IO ()
--main = do
--    print ""
--    let reFT = xRe . ft
----    let imFT = xIm . ft
--    let exp = normalize . reFT . reFT $ variable2D @6 @6 "x2"
--        valMap = fromList [("x2", V2D $ listArray ((0, 0), (6 - 1, 6 - 1)) [1 .. ])]
--    (_, vals) <- HashedToCSpec.evaluateCodeC True exp valMap
--    print $ prettifyShow $ eval valMap exp
--    print vals
--    showExp exp
main :: IO ()
main = hspecWith defaultConfig {configQuickCheckMaxSuccess = Just 50} spec

spec :: Spec
spec = do
    describe "HashedSolverSpec" SolverSpec.spec
    describe "NormalizeSpec" NormalizeSpec.spec
    describe "Test1" Test1.spec
    describe "Test2" Test2.spec
    describe "HashedInterpSpec" InterpSpec.spec
    describe "HashedCollectSpec" CollectSpec.spec
    describe "HashedToCSpec" ToCSpec.spec
    describe "StructureSpec" StructureSpec.spec
    describe "NormalizeEval.ScalarRSpec" ScalarRSpec.spec
    describe "NormalizeEval.ScalarCSpec" ScalarCSpec.spec
    describe "NormalizeEval.OneRSpec" OneRSpec.spec
    describe "NormalizeEval.OneCSpec" OneCSpec.spec
