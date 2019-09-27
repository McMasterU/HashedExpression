{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module HashedNormalizeSpec where

import Commons
import Data.Array (listArray)
import Data.Complex (Complex(..))
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import HashedExpression
import HashedInterp ((~=), eval)
import HashedNormalize
import HashedOperation
import HashedPrettify
import HashedUtils
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

reFT :: (DimensionType d) => Expression d R -> Expression d R
reFT = xRe . ft

imFT :: (DimensionType d) => Expression d R -> Expression d R
imFT = xIm . ft

spec :: Spec
spec = do
    describe "Normalize spec" $ do
        specify "normalize scalar one zero" $ do
            const 0.0 *. const 9.0 `shouldNormalizeTo` const 0.0
            x * one `shouldNormalizeTo` x
            one * x `shouldNormalizeTo` x
            x * zero `shouldNormalizeTo` zero
            zero * x `shouldNormalizeTo` zero
            y * (x * zero) `shouldNormalizeTo` zero
            zero * (x * one) `shouldNormalizeTo` zero
            zero * x * one `shouldNormalizeTo` zero
            zero * (x * y) `shouldNormalizeTo` zero
            (x * y) * zero `shouldNormalizeTo` zero
            (x * zero) * one `shouldNormalizeTo` zero
            ((x * y) * one) `shouldNormalizeTo` (x * y)
            x * y * z * one `shouldNormalizeTo` x * y * z
            product [x, y, z, t, w, zero] `shouldNormalizeTo` zero
        specify "normalize log and exponential" $ do
            log (exp x) `shouldNormalizeTo` x
            exp (log x) `shouldNormalizeTo` x
        specify "complex related" $ do
            ((x +: y) * (z +: w)) `shouldNormalizeTo`
                ((x * z - y * w) +: (x * w + y * z))
            xRe (x +: y) `shouldNormalizeTo` x
            xIm (x +: y) `shouldNormalizeTo` y
            (x +: y) + (u +: v) `shouldNormalizeTo` (x + u) +: (y + v)
            s *. (x +: y) `shouldNormalizeTo` (s *. x) +: (s *. y)
            (x +: y) *
                (z +: w) `shouldNormalizeTo` (x * z - y * w) +: (x * w + y * z)
        specify "dot product" $ do
            x <.> zero `shouldNormalizeTo` zero
            zero <.> x `shouldNormalizeTo` zero
            ((s *. x) <.> y) `shouldNormalizeTo` (s *. (x <.> y))
            x <.> (s *. y) `shouldNormalizeTo` s *. (x <.> y)
        specify "distributivity" $ do
            x * (y + z) `shouldNormalizeTo` (x * y + x * z)
            (y + z) * x `shouldNormalizeTo` (x * y + x * z)
            (x *. (y + z)) `shouldNormalizeTo` (x *. y + x *. z)
            (x <.> (y + z)) `shouldNormalizeTo` ((x <.> y) + (x <.> z))
            (y + z) <.> x `shouldNormalizeTo` (y <.> x) + (z <.> x)
            x *
                sum [y, z, t, u, v] `shouldNormalizeTo`
                sum (map (x *) [y, z, t, u, v])
            sum [y, z, t, u, v] *
                x `shouldNormalizeTo` sum (map (x *) [y, z, t, u, v])
            x *. sum [y, z, t, u, v] `shouldNormalizeTo`
                sum (map (x *.) [y, z, t, u, v])
            x <.> sum [y, z, t, u, v] `shouldNormalizeTo`
                sum (map (x <.>) [y, z, t, u, v])
            sum [y, z, t, u, v] <.> x `shouldNormalizeTo`
                sum (map (<.> x) [y, z, t, u, v])
            product [a, b, c, sum [x, y, z]] `shouldNormalizeTo`
                sum (map (product . (: [a, b, c])) [x, y, z])
            (x + y) * (z + t) * a *
                b `shouldNormalizeTo`
                (a * b * x * z + a * b * x * t + a * b * y * z + a * b * y * t)
        specify "flatten sum and product" $ do
            product [x * y, product [z, t, w], one] `shouldNormalizeTo`
                product [x, y, z, t, w]
            sum [x + y, sum [z, t, w + s], zero] `shouldNormalizeTo`
                sum [x, y, z, t, w, s]
        specify "group constants together" $ do
            product [one, one, x, y, one, z] `shouldNormalizeTo`
                product [x, y, z]
            sum [one, one, x, y, one, z] `shouldNormalizeTo`
                sum [const 3, x, y, z]
            product [const 1, const 2, x, y, const 3, z] `shouldNormalizeTo`
                product [const 6, x, y, z]
        specify "combine same terms" $
            -- Higher dimension this is correct
         do
            sum [one *. x1, x1, x1, const 3 *. y1, y1] `shouldNormalizeTo`
                sum [const 3 *. x1, const 4 *. y1]
            sum [const (-1) *. x1, x1, const 3 *. y1, y1, z1] `shouldNormalizeTo`
                sum [const 4 *. y1, z1]
            x1 - x1 `shouldNormalizeTo` zero1
            sum [one *. x, x, x, const 3 *. y, y] `shouldNormalizeTo`
                sum [const 3 *. x, const 4 *. y]
            sum [const (-1) *. x, x, const 3 *. y, y, z] `shouldNormalizeTo`
                sum [const 4 *. y, z]
            x - x `shouldNormalizeTo` zero
        specify "scale rules" $ do
            x *. (y *. v) `shouldNormalizeTo` (x * y) *. v
        specify "negate rules" $ do
            negate (negate x) `shouldNormalizeTo` normalize x
            negate (negate (x + y)) `shouldNormalizeTo` (x + y)
            negate zero `shouldNormalizeTo` zero
    describe "Normalize spec higher dimension" $ do
        specify "normalize one d one zero" $ do
            x1 * one1 `shouldNormalizeTo` x1
            one1 * x1 `shouldNormalizeTo` x1
            x1 * zero1 `shouldNormalizeTo` zero1
            zero1 * x1 `shouldNormalizeTo` zero1
            y1 * (x1 * zero1) `shouldNormalizeTo` zero1
            zero1 * (x1 * one1) `shouldNormalizeTo` zero1
            zero1 * x1 * one1 `shouldNormalizeTo` zero1
            zero1 * (x1 * y1) `shouldNormalizeTo` zero1
            (x1 * y1) * zero1 `shouldNormalizeTo` zero1
            (x1 * zero1) * one1 `shouldNormalizeTo` zero1
            (x1 * y1) * one1 `shouldNormalizeTo` (x1 * y1)
            x1 * y1 * z1 * one1 `shouldNormalizeTo` x1 * y1 * z1
        specify "dot product higher dimension with scaling and point wise" $ do
            x1 <.> zero1 `shouldNormalizeTo` zero
            zero1 <.> x1 `shouldNormalizeTo` zero
            (s *. x1) <.> y1 `shouldNormalizeTo` s *. (x1 <.> y1)
            x1 <.> (s *. y1) `shouldNormalizeTo` s *. (x1 <.> y1)
            x1 * (y1 + z1) `shouldNormalizeTo` x1 * y1 + x1 * z1
            (y1 + z1) * x1 `shouldNormalizeTo` x1 * y1 + x1 * z1
            s *. (y1 + z1) `shouldNormalizeTo` s *. y1 + s *. z1
            x1 <.> (y1 + z1) `shouldNormalizeTo` (x1 <.> y1) + (x1 <.> z1)
            (y1 + z1) <.> x1 `shouldNormalizeTo` (y1 <.> x1) + (z1 <.> x1)
        specify "log and exp higher" $ do
            log (exp x1) `shouldNormalizeTo` x1
            exp (log x1) `shouldNormalizeTo` x1
            log (exp x2) `shouldNormalizeTo` x2
            exp (log x2) `shouldNormalizeTo` x2
        specify "rotate rules" $ do
            rotate 1 (rotate 2 x1) `shouldNormalizeTo` rotate 3 x1
            rotate 2 (rotate 3 x1) `shouldNormalizeTo` rotate 5 x1
            rotate (1, 1) (rotate (2, 3) x2) `shouldNormalizeTo`
                rotate (3, 4) x2
            rotate (0, 1) (rotate (2, -3) x2) `shouldNormalizeTo`
                rotate (2, -2) x2
            rotate (0, 0, 0) x3 `shouldNormalizeTo` x3
            rotate (2, 3, 4) (s *. y3) `shouldNormalizeTo` s *.
                rotate (2, 3, 4) y3
            rotate (2, 3) (sum [x2, y2, z2]) `shouldNormalizeTo`
                sum (map (rotate (2, 3)) [x2, y2, z2])
            rotate (2, 3) (product [x2, y2, z2]) `shouldNormalizeTo`
                product (map (rotate (2, 3)) [x2, y2, z2])
    describe "Normalize piecewise" $
        specify "some piecewise rules" $ do
            piecewise [1] c [x, y] `shouldNormalizeTo` piecewise [1] c [x, zero] +
                piecewise [1] c [zero, y]
            piecewise [1] c [x + z, y] `shouldNormalizeTo`
                piecewise [1] c [x, zero] +
                piecewise [1] c [z, zero] +
                piecewise [1] c [zero, y]
            piecewise [1] c [x *. y, zero] `shouldNormalizeTo` x *.
                piecewise [1] c [y, zero]
            piecewise [1, 2] c [zero, x *. y, zero] `shouldNormalizeTo` x *.
                piecewise [1, 2] c [zero, y, zero]
            piecewise [4, 5] x1 [zero1, x1 * (s *. y1), zero1] `shouldNormalizeTo`
                s *.
                piecewise [4, 5] x1 [zero1, x1 * y1, zero1]
            piecewise [1] c1 [c1 +: y1, z1 +: t1] `shouldNormalizeTo`
                piecewise [1] c1 [c1, z1] +:
                piecewise [1] c1 [y1, t1]
    describe "Fourier transform" $
        specify "some Ft rules" $ do
            reFT (reFT x1) +
                imFT (imFT x1) `shouldNormalizeTo`
                const (fromIntegral defaultDim1D) *.
                x1
            reFT (reFT x2) +
                imFT (imFT x2) `shouldNormalizeTo`
                const (fromIntegral (default1stDim2D * default2ndDim2D)) *.
                x2
            let x = variable1D @10 "x"
                y = variable1D @10 "y"
                z = variable1D @10 "z"
                t = variable1D @10 "t"
                valMap =
                    Map.fromList
                        [ ("x", V1D $ listArray (0, 9) [1 ..])
                        , ("y", V1D $ listArray (0, 9) [2,5 ..])
                        , ("z", V1D $ listArray (0, 9) [3,8 ..])
                        , ("t", V1D $ listArray (0, 9) [0,-1 ..])
                        ]
            let res1 = eval valMap $ reFT (reFT (x * y + z * t))
                res2 = eval valMap . normalize $ reFT (reFT (x * y + z * t))
            res1 `shouldApprox` res2
            let res1 = eval valMap $ imFT (imFT (x * y + z * t))
                res2 = eval valMap . normalize $ imFT (imFT (x * y + z * t))
            res1 `shouldApprox` res2
            let res1 =
                    eval valMap $
                    reFT (reFT (x * y + z * t)) + imFT (imFT (x * y + z * t))
                res2 =
                    eval valMap . normalize $
                    reFT (reFT (x * y + z * t)) + imFT (imFT (x * y + z * t))
            res1 `shouldApprox` res2
