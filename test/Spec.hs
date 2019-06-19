import qualified SimplifySpec
import Test.Hspec

--main = regressionTestAll
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "EvalSpec" SimplifySpec.spec
