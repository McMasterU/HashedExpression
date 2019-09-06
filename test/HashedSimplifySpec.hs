module HashedSimplifySpec where

import Commons
import Data.Complex (Complex(..))
import Data.Maybe (fromJust)
import HashedExpression
import HashedInterp ((~=))
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
spec = do
    describe "Simplify spec" $ do
        specify "simplify scalar one zero" $ do
            const 0.0 *. const 9.0 `shouldSimplifyTo` const 0.0
            x * one `shouldSimplifyTo` x
            one * x `shouldSimplifyTo` x
            x * zero `shouldSimplifyTo` zero
            zero * x `shouldSimplifyTo` zero
            y * (x * zero) `shouldSimplifyTo` zero
            zero * (x * one) `shouldSimplifyTo` zero
            zero * x * one `shouldSimplifyTo` zero
            zero * (x * y) `shouldSimplifyTo` zero
            (x * y) * zero `shouldSimplifyTo` zero
            (x * zero) * one `shouldSimplifyTo` zero
            ((x * y) * one) `shouldSimplifyTo` (x * y)
            x * y * z * one `shouldSimplifyTo` x * y * z
            product [x, y, z, t, w, zero] `shouldSimplifyTo` zero
        specify "simplify log and exponential" $ do
            log (exp x) `shouldSimplifyTo` x
            exp (log x) `shouldSimplifyTo` x
        specify "complex related" $ do
            ((x +: y) * (z +: w)) `shouldSimplifyTo`
                ((x * z - y * w) +: (x * w + y * z))
            xRe (x +: y) `shouldSimplifyTo` x
            xIm (x +: y) `shouldSimplifyTo` y
            (x +: y) + (u +: v) `shouldSimplifyTo` (x + u) +: (y + v)
            s *. (x +: y) `shouldSimplifyTo` (s *. x) +: (s *. y)
            (x +: y) *
                (z +: w) `shouldSimplifyTo` (x * z - y * w) +: (x * w + y * z)
        specify "dot product" $ do
            x <.> zero `shouldSimplifyTo` zero
            zero <.> x `shouldSimplifyTo` zero
            ((s *. x) <.> y) `shouldSimplifyTo` (s *. (x <.> y))
            x <.> (s *. y) `shouldSimplifyTo` s *. (x <.> y)
        specify "distributivity" $ do
            x * (y + z) `shouldSimplifyTo` (x * y + x * z)
            (y + z) * x `shouldSimplifyTo` (x * y + x * z)
            (x *. (y + z)) `shouldSimplifyTo` (x *. y + x *. z)
            (x <.> (y + z)) `shouldSimplifyTo` ((x <.> y) + (x <.> z))
            (y + z) <.> x `shouldSimplifyTo` (y <.> x) + (z <.> x)
            x *
                sum [y, z, t, u, v] `shouldSimplifyTo`
                sum (map (x *) [y, z, t, u, v])
            sum [y, z, t, u, v] *
                x `shouldSimplifyTo` sum (map (x *) [y, z, t, u, v])
            x *. sum [y, z, t, u, v] `shouldSimplifyTo`
                sum (map (x *.) [y, z, t, u, v])
            x <.> sum [y, z, t, u, v] `shouldSimplifyTo`
                sum (map (x <.>) [y, z, t, u, v])
            sum [y, z, t, u, v] <.> x `shouldSimplifyTo`
                sum (map (<.> x) [y, z, t, u, v])
            product [a, b, c, sum [x, y, z]] `shouldSimplifyTo`
                sum (map (product . (: [a, b, c])) [x, y, z])
            (x + y) * (z + t) * a *
                b `shouldSimplifyTo`
                (a * b * x * z + a * b * x * t + a * b * y * z + a * b * y * t)
        specify "flatten sum and product" $ do
            product [x * y, product [z, t, w], one] `shouldSimplifyTo`
                product [x, y, z, t, w]
            sum [x + y, sum [z, t, w + s], zero] `shouldSimplifyTo`
                sum [x, y, z, t, w, s]
        specify "group constants together" $ do
            product [one, one, x, y, one, z] `shouldSimplifyTo`
                product [x, y, z]
            sum [one, one, x, y, one, z] `shouldSimplifyTo`
                sum [const 3, x, y, z]
            product [const 1, const 2, x, y, const 3, z] `shouldSimplifyTo`
                product [const 6, x, y, z]
        specify "combine same terms" $
            -- Higher dimension this is correct
         do
            sum [one *. x1, x1, x1, const 3 *. y1, y1] `shouldSimplifyTo`
                sum [const 3 *. x1, const 4 *. y1]
            sum [const (-1) *. x1, x1, const 3 *. y1, y1, z1] `shouldSimplifyTo`
                sum [const 4 *. y1, z1]
            x1 - x1 `shouldSimplifyTo` zero1
            sum [one *. x, x, x, const 3 *. y, y] `shouldSimplifyTo`
                sum [const 3 *. x, const 4 *. y]
            sum [const (-1) *. x, x, const 3 *. y, y, z] `shouldSimplifyTo`
                sum [const 4 *. y, z]
            x - x `shouldSimplifyTo` zero
        specify "scale rules" $ do
            x *. (y *. v) `shouldSimplifyTo` (x * y) *. v
            xRe (x *. xc) `shouldSimplifyTo` x *. xRe xc
            xIm (x *. xc) `shouldSimplifyTo` x *. xIm xc
        specify "negate rules" $ do
            negate (negate x) `shouldSimplifyTo` simplify x
            negate (negate (x + y)) `shouldSimplifyTo` (x + y)
            negate zero `shouldSimplifyTo` zero
    describe "Simplify spec higher dimension" $ do
        specify "simplify one d one zero" $ do
            x1 * one1 `shouldSimplifyTo` x1
            one1 * x1 `shouldSimplifyTo` x1
            x1 * zero1 `shouldSimplifyTo` zero1
            zero1 * x1 `shouldSimplifyTo` zero1
            y1 * (x1 * zero1) `shouldSimplifyTo` zero1
            zero1 * (x1 * one1) `shouldSimplifyTo` zero1
            zero1 * x1 * one1 `shouldSimplifyTo` zero1
            zero1 * (x1 * y1) `shouldSimplifyTo` zero1
            (x1 * y1) * zero1 `shouldSimplifyTo` zero1
            (x1 * zero1) * one1 `shouldSimplifyTo` zero1
            (x1 * y1) * one1 `shouldSimplifyTo` (x1 * y1)
            x1 * y1 * z1 * one1 `shouldSimplifyTo` x1 * y1 * z1
        specify "dot product higher dimension with scaling and point wise" $ do
            x1 <.> zero1 `shouldSimplifyTo` zero
            zero1 <.> x1 `shouldSimplifyTo` zero
            (s *. x1) <.> y1 `shouldSimplifyTo` s *. (x1 <.> y1)
            x1 <.> (s *. y1) `shouldSimplifyTo` s *. (x1 <.> y1)
            x1 * (y1 + z1) `shouldSimplifyTo` x1 * y1 + x1 * z1
            (y1 + z1) * x1 `shouldSimplifyTo` x1 * y1 + x1 * z1
            s *. (y1 + z1) `shouldSimplifyTo` s *. y1 + s *. z1
            x1 <.> (y1 + z1) `shouldSimplifyTo` (x1 <.> y1) + (x1 <.> z1)
            (y1 + z1) <.> x1 `shouldSimplifyTo` (y1 <.> x1) + (z1 <.> x1)
        specify "log and exp higher" $ do
            log (exp x1) `shouldSimplifyTo` x1
            exp (log x1) `shouldSimplifyTo` x1
            log (exp x2) `shouldSimplifyTo` x2
            exp (log x2) `shouldSimplifyTo` x2
        specify "rotate rules" $ do
            rotate 1 (rotate 2 x1) `shouldSimplifyTo` rotate 3 x1
            rotate 2 (rotate 3 x1) `shouldSimplifyTo` rotate 5 x1
            rotate (1, 1) (rotate (2, 3) x2) `shouldSimplifyTo` rotate (3, 4) x2
            rotate (0, 1) (rotate (2, -3) x2) `shouldSimplifyTo`
                rotate (2, -2) x2
            rotate (0, 0, 0) x3 `shouldSimplifyTo` x3
            rotate (2, 3, 4) (s *. y3) `shouldSimplifyTo` s *.
                rotate (2, 3, 4) y3
            rotate (2, 3) (sum [x2, y2, z2]) `shouldSimplifyTo`
                sum (map (rotate (2, 3)) [x2, y2, z2])
            rotate (2, 3) (product [x2, y2, z2]) `shouldSimplifyTo`
                product (map (rotate (2, 3)) [x2, y2, z2])
    describe "Simplify piecewise" $
        specify "some piecewise rules" $ do
            piecewise [1] c [x, y] `shouldSimplifyTo`
                piecewise [1] c [x, zero] + piecewise [1] c [zero, y]
            piecewise [1] c [x + z, y] `shouldSimplifyTo`
                piecewise [1] c [x, zero] + piecewise [1] c [z, zero] + piecewise [1] c [zero, y] 
            piecewise [1] c [x *. y, zero] `shouldSimplifyTo` x *.
                piecewise [1] c [y, zero]
            piecewise [1, 2] c [zero, x *. y, zero] `shouldSimplifyTo` x *.
                piecewise [1, 2] c [zero, y, zero]
            piecewise [4, 5] x1 [zero1, x1 * (s *. y1), zero1] `shouldSimplifyTo`
                s *. piecewise [4, 5] x1 [zero1, x1 * y1, zero1]
            piecewise [1] c1 [c1 +: y1, z1 +: t1] `shouldSimplifyTo`
                piecewise [1] c1 [c1, z1] +: piecewise [1] c1 [y1, t1]
