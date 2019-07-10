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
spec =
    describe "Simplify spec" $ do
        specify "simplify scalar one zero" $ do
            x `shouldBe` x
            simplify (x * x) `shouldBe` (x ^ 2)
            simplify ((x * x) * x) `shouldBe` (x ^ 3)
            simplify (const 1 / x) `shouldBe` (x ^ (-1))
            simplify (x * x / x) `shouldBe` x
            simplify (x / x) `shouldBe` const 1
            simplify (x / x / x) `shouldBe` (x ^ (-1)) --Expected output
            simplify (x / x / y) `shouldBe` (y ^ (-1)) --((x*(x^-1))*(y^-1))
            simplify ((x * y) ^ 3) `shouldBe` ((x ^ 3) * (y ^ 3))
            simplify (((x * y) ^ 3) * x / y) `shouldBe` ((y ^ 2) * (x ^ 4))
            simplify (((x * y) ^ 3) * x + y) `shouldBe`
                (y + ((y ^ 3) * (x ^ 4))) --Without Paranthesis
            simplify (((x * y) ^ 3) * (x + y)) `shouldBe`
                (((y ^ 3) * (x ^ 4)) + ((x ^ 3) * (y ^ 4))) -- With paranthesis
            simplify (x / x) `shouldBe` const 1
            simplify (x / (x ^ 3)) `shouldBe` (x ^ (-2)) --FIXED
            simplify ((x ^ 2) ^ 2) `shouldBe` (x ^ 4)
            simplify ((x + y) * (x + y)) `shouldBe`
                simplify ((const 2.0 *. (x * y)) + (x ^ 2) + (y ^ 2))
            simplify (((x + y) * (x + y)) * (x + y)) `shouldBe`
                simplify
                    ((const 3.0 *. (y * (x ^ 2))) +
                     ((const 3.0) *. (x * (y ^ 2))) +
                     (x ^ 3) +
                     (y ^ 3))
            simplify ((x - y) ^ 2) `shouldBe`
                simplify
                    ((const 2.0 *. (x * (negate (y)))) + (x ^ 2) +
                     ((negate (y)) ^ 2))
            simplify (((x * y) ^ 3) * (x + y) ^ 2) `shouldBe`
                simplify
                    (((y ^ 3) * (x ^ 5)) + ((x ^ 3) * (y ^ 5)) +
                     (const 2.0 *. ((x ^ 4) * (y ^ 4))))
            simplify ((x + y) ^ 2) `shouldBe`
                simplify ((const 2.0 *. (x * y)) + (x ^ 2) + (y ^ 2))
            simplify (x / (x ^ 2)) `shouldBe` (x ^ (-1))
            simplify (x / (x * x)) `shouldBe` (x ^ (-1)) --with paranthesis
            simplify (x / x * x) `shouldBe` x -- without paranthesis
            simplify (x / (x ^ 3)) `shouldBe` (x ^ (-2)) -- FIXED
            simplify ((x * x) / y) `shouldBe` ((y ^ (-1)) * (x ^ 2))
            simplify ((x ^ 2) ^ 3) `shouldBe` (x ^ 6)
            simplify (((x ^ 3) ^ 3) ^ 2) `shouldBe` (x ^ 18)
            simplify (y * (y ^ 2) ^ 2) `shouldBe` (y ^ 5)
            simplify (x / ((y ^ 2) ^ 3)) `shouldBe` (x * y ^ (-6))
            simplify (((x * y) ^ 2) / x) `shouldBe` (x * y ^ 2)
--              simplify (((x+y)^2)*((x-y)^2)) `shouldBe`
