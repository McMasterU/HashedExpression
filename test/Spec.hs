import qualified SimplifyEval.ZeroCSpec as ZeroCSpec
import qualified SimplifyEval.ZeroRSpec as ZeroRSpec
import qualified SimplifySpec
import Test.Hspec

--main = regressionTestAll
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "SimplifySpec" SimplifySpec.spec
    describe "SimplifyEval.ZeroRSpec" ZeroRSpec.spec
    describe "SimplifyEval.ZeroCSpec" ZeroCSpec.spec
