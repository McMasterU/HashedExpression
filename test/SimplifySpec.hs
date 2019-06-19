module SimplifySpec where

import HashedOperation
import HashedSimplify
import HashedPrettify
import HashedExpression
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
    , sin
    , sinh
    , sqrt
    , sum
    , tan
    , tanh
    )
import Test.Hspec

[x, y, z, u, v, w] = map var ["x", "y", "z", "u", "v", "w"]

[x1, y1, z1, u1, v1, w1] = map (var1d 10) ["X1", "Y1", "Z1", "U1", "V1", "W1"]

[x2, y2, z2, u2, v2, w2] =
    map (var2d (10, 10)) ["X2", "Y2", "Z2", "U2", "V2", "W2"]

[zero, one] = map const [0, 1]
[zero1, one1] = map (const1d 10) [0, 1]
[zero2, one2] = map (const2d (10, 10)) [0, 1]

spec :: Spec
spec = do
    describe "Simplify spec" $ do
        specify "simplify 1" $ do
            simplify (x * one) `shouldBe` x
            simplify (one * x) `shouldBe` x
            simplify (x * zero) `shouldBe` zero
            simplify (zero * x) `shouldBe` zero
            -- TODO: These can be uncommented one makeRecursive :: Simplification -> Simplification is done
--            simplify ((x * zero) * y) `shouldBe` zero
--            simplify (y * (x * zero)) `shouldBe` zero
--            simplify (zero * (x * one)) `shouldBe` zero
--            simplify (zero * x * one) `shouldBe` zero
--            simplify (zero * (x * y)) `shouldBe` zero
--            simplify ((x * y) * zero) `shouldBe` zero
--            simplify ((x * zero) * one) `shouldBe` zero
--            simplify ((x * y) * one) `shouldBe` (x * y)
--            simplify (x * y * z * one) `shouldBe` simplify (x * y * z)
        specify "simplify 0" $ do
            simplify (log (exp (x))) `shouldBe` x
            simplify (exp (log (x))) `shouldBe` x
            simplify (log (exp (x1))) `shouldBe` x1
            simplify (exp (log (x1))) `shouldBe` x1
            simplify (log (exp (x2))) `shouldBe` x2
            simplify (exp (log (x2))) `shouldBe` x2

