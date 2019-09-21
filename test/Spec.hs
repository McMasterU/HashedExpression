import Data.Array.Unboxed as U
import Data.Map (fromList, union)
import qualified Data.Set as Set
import qualified HashedCollectSpec
import HashedDerivative
import HashedExpression
import HashedInterp
import qualified HashedInterpSpec
import HashedNormalize
import qualified HashedNormalizeSpec
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
import qualified HashedToCSpec
import qualified NormalizeEval.OneCSpec as OneCSpec
import qualified NormalizeEval.OneRSpec as OneRSpec
import qualified NormalizeEval.ScalarCSpec as ScalarCSpec
import qualified NormalizeEval.ScalarRSpec as ScalarRSpec
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
import qualified StructureSpec
import qualified Test1
import qualified Test2

import Commons
import Data.Maybe (fromJust)
import HashedUtils
import HashedVar
import Test.Hspec
import Test.Hspec.Runner

main :: IO ()
main = hspecWith defaultConfig {configQuickCheckMaxSuccess = Just 100} spec

spec :: Spec
spec = do
    describe "NormalizeSpec" HashedNormalizeSpec.spec
    describe "Test1" Test1.spec
    describe "Test2" Test2.spec
    describe "HashedInterpSpec" HashedInterpSpec.spec
    describe "HashedCollectSpec" HashedCollectSpec.spec
    describe "HashedToCSpec" HashedToCSpec.spec
    describe "StructureSpec" StructureSpec.spec
    describe "NormalizeEval.ScalarRSpec" ScalarRSpec.spec
    describe "NormalizeEval.ScalarCSpec" ScalarCSpec.spec
    describe "NormalizeEval.OneRSpec" OneRSpec.spec
    describe "NormalizeEval.OneCSpec" OneCSpec.spec
