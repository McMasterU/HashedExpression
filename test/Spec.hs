import qualified EvalSpec
import qualified GradDiffSpec
import qualified PartDiffSpec
import qualified SimplifySpec
import Test.Hspec

--main = regressionTestAll
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "SimplifySpec" SimplifySpec.spec
    describe "EvalSpec" EvalSpec.spec
    describe "PartDiffSpec" PartDiffSpec.spec
    describe "GradDiffSpec" GradDiffSpec.spec
