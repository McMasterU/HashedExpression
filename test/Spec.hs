import qualified SimplifySpec
import qualified EvalSpec
import qualified PartDiffSpec
import qualified GradDiffSpec
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
