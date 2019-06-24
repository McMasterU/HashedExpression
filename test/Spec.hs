import qualified SimplifySpec
import qualified SimplifyEvalSpec
import Test.Hspec

--main = regressionTestAll
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "SimplifySpec" SimplifySpec.spec
    describe "SimplifyEvalSpec" SimplifyEvalSpec.spec
