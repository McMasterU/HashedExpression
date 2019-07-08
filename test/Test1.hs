module Test1 where
import Commons
import Data.Maybe (fromJust)
import HashedExpression
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
import HashedSimplify
import HashedVar
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
    , sum
    , tan
    , tanh
    )
import Test.Hspec

spec :: Spec
spec = describe "Simplify spec" $ do
          specify "simplify scalar one zero" $ do
              x `shouldBe` x
              --simplify ((x+y)*(x+y))  `shouldBe` (((const 2.0)*.(x*y))+(x^2)+(y^2))
              --simplify (((x+y)*(x+y))*(x+y))  `shouldBe` (((const 3.0)*.(y*(x^2)))+((const 3.0)*.(x*(y^2)))+(x^3)+(y^3))
              simplify (x*x) `shouldBe` (x^2)
              simplify ((x*x)*x) `shouldBe` (x^3)
              simplify (const 1/x) `shouldBe` (x^(-1))
              simplify (x*x/x) `shouldBe` x
              simplify (x/x) `shouldBe` const 1
              simplify (x/x/x) `shouldBe` x^(-1) --FIXME
              simplify (x/x/y) `shouldBe` (y^(-1)) --((x*(x^-1))*(y^-1))

              --simplify (x/x*x) `shouldBe` x^(-1) --FIXME
              --simplify (x/(x*x)) `shouldBe` (x*((x*x)^(-1))) -- FIXME
              --simplify ((x*x)/y) `shouldBe` ((y^(-1))*(x^2))

