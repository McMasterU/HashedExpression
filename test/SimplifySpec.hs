module SimplifySpec where

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
spec = do
    describe "Simplify spec" $ do
        specify "simplify scalar one zero" $ do
            simplify (const 0.0 *. const 9.0) `shouldBe` const 0.0
            simplify (x * one) `shouldBe` x
            simplify (one * x) `shouldBe` x
            simplify (x * zero) `shouldBe` zero
            simplify (zero * x) `shouldBe` zero
            simplify (y * (x * zero)) `shouldBe` zero
            simplify (zero * (x * one)) `shouldBe` zero
            simplify (zero * x * one) `shouldBe` zero
            simplify (zero * (x * y)) `shouldBe` zero
            simplify ((x * y) * zero) `shouldBe` zero
            simplify ((x * zero) * one) `shouldBe` zero
            simplify ((x * y) * one) `shouldBe` (x * y)
            simplify (x * y * z * one) `shouldBe` simplify (x * y * z)
            simplify (product [x, y, z, t, w, zero]) `shouldBe` zero
        specify "simplify log and exponential" $ do
            simplify (log (exp x)) `shouldBe` x
            simplify (exp (log x)) `shouldBe` x
        specify "complex related" $ do
            prettify (simplify ((x +: y) * (z +: w))) `shouldBe`
                prettify ((x * z - y * w) +: (x * w + y * z))
            simplify (xRe (x +: y)) `shouldBe` x
            simplify (xIm (x +: y)) `shouldBe` y
            simplify ((x +: y) + (u +: v)) `shouldBe` (x + u) +: (y + v)
            simplify (s *. (x +: y)) `shouldBe` (s *. x) +: (s *. y) -- does not work for ScalarC, only vectorC; it's also in HashedComplexInstances
            simplify ((x +: y) * (z +: w)) `shouldBe` (x * z - y * w) +:
                (x * w + y * z)
        specify "dot product" $ do
            simplify (x <.> zero) `shouldBe` zero
            simplify (zero <.> x) `shouldBe` zero
            simplify ((s *. x) <.> y) `shouldBe` s * (x <.> y) -- TB,CD,RF: *. --> * (FIX) 27/05/2015.
            simplify (x <.> (s *. y)) `shouldBe` s * (x <.> y) -- TB,CD,RF: *. --> * (FIX) 27/05/2015.
        specify "distributivity" $ do
            simplify (x * (y + z)) `shouldBe` (x * y + x * z)
            simplify ((y + z) * x) `shouldBe` (x * y + x * z)
            simplify (x *. (y + z)) `shouldBe` (x *. y + x *. z)
            simplify (x <.> (y + z)) `shouldBe` ((x <.> y) + (x <.> z))
            simplify ((y + z) <.> x) `shouldBe` ((x <.> y) + (x <.> z))
            simplify (x * sum [y, z, t, u, v]) `shouldBe`
                sum (map (x *) [y, z, t, u, v])
            simplify (sum [y, z, t, u, v] * x) `shouldBe`
                sum (map (x *) [y, z, t, u, v])
            simplify (x *. sum [y, z, t, u, v]) `shouldBe`
                sum (map (x *.) [y, z, t, u, v])
            simplify (x <.> sum [y, z, t, u, v]) `shouldBe`
                sum (map (x <.>) [y, z, t, u, v])
            simplify (sum [y, z, t, u, v] <.> x) `shouldBe`
                sum (map (x <.>) [y, z, t, u, v])
        specify "flatten sum and product" $ do
            simplify (product [x * y, product [z, t, w], one]) `shouldBe`
                product [x, y, z, t, w]
            simplify (sum [x + y, sum [z, t, w + s], zero]) `shouldBe`
                sum [x, y, z, t, w, s]
        specify "group constants together" $ do
            simplify (product [one, one, x, y, one, z]) `shouldBe`
                product [x, y, z]
            simplify (sum [one, one, x, y, one, z]) `shouldBe`
                sum [const 3, x, y, z]
            simplify (product [const 1, const 2, x, y, const 3, z]) `shouldBe`
                product [const 6, x, y, z]
        specify "combine same terms" $ do
            simplify (sum [one *. x, x, x, const 3 *. y, y]) `shouldBe`
                sum [const 3 *. x, const 4 *. y]
            simplify (sum [const (-1) *. x, x, const 3 *. y, y, z]) `shouldBe`
                sum [const 4 *. y, z]
            simplify (x - x) `shouldBe` zero
        specify "scale rules" $ do
            simplify (x *. (y *. v)) `shouldBe` (x * y) *. v
            simplify (xRe (x *. xc)) `shouldBe` simplify (x *. xRe xc)
            simplify (xIm (x *. xc)) `shouldBe` simplify (x *. xIm xc)
        specify "negate rules" $ do
            simplify (negate (negate x)) `shouldBe` x
            simplify (negate (negate (x + y))) `shouldBe` x + y
            simplify (negate zero) `shouldBe` zero
    describe "Simplify spec higher dimension" $ do
        specify "simplify one d one zero" $ do
            simplify (x1 * one1) `shouldBe` x1
            simplify (one1 * x1) `shouldBe` x1
            simplify (x1 * zero1) `shouldBe` zero1
            simplify (zero1 * x1) `shouldBe` zero1
            simplify (y1 * (x1 * zero1)) `shouldBe` zero1
            simplify (zero1 * (x1 * one1)) `shouldBe` zero1
            simplify (zero1 * x1 * one1) `shouldBe` zero1
            simplify (zero1 * (x1 * y1)) `shouldBe` zero1
            simplify ((x1 * y1) * zero1) `shouldBe` zero1
            simplify ((x1 * zero1) * one1) `shouldBe` zero1
            simplify ((x1 * y1) * one1) `shouldBe` (x1 * y1)
            simplify (x1 * y1 * z1 * one1) `shouldBe` simplify (x1 * y1 * z1)
        specify "dot product higher dimension with scaling and point wise" $ do
            simplify (x1 <.> zero1) `shouldBe` zero
            simplify (zero1 <.> x1) `shouldBe` zero
            simplify ((s *. x1) <.> y1) `shouldBe` s * (x1 <.> y1)
            simplify (x1 <.> (s *. y1)) `shouldBe` s * (x1 <.> y1)
            simplify (x1 * (y1 + z1)) `shouldBe` (x1 * y1 + x1 * z1)
            simplify ((y1 + z1) * x1) `shouldBe` (x1 * y1 + x1 * z1)
            simplify (s *. (y1 + z1)) `shouldBe` (s *. y1 + s *. z1)
            simplify (x1 <.> (y1 + z1)) `shouldBe` ((x1 <.> y1) + (x1 <.> z1))
            simplify ((y1 + z1) <.> x1) `shouldBe` ((x1 <.> y1) + (x1 <.> z1))
        specify "log and exp higher" $ do
            simplify (log (exp x1)) `shouldBe` x1
            simplify (exp (log x1)) `shouldBe` x1
            simplify (log (exp x2)) `shouldBe` x2
            simplify (exp (log x2)) `shouldBe` x2
