{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module NormalizeSpec where

import Commons
import Data.Array (listArray)
import Data.Complex (Complex (..))
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Normalize
import HashedExpression.Internal.Utils
import HashedExpression.Interp (eval, (~=))
import HashedExpression.Operation
import HashedExpression.Prettify
import HashedExpression.Value
import Test.Hspec
import Test.QuickCheck (property)
import Var
import Prelude hiding ((^))

reFT :: (DimensionType d) => Expression d R -> Expression d R
reFT = xRe . ft

imFT :: (DimensionType d) => Expression d R -> Expression d R
imFT = xIm . ft

prop_NormalizeIsIdempotent :: ArbitraryExpresion -> Expectation
prop_NormalizeIsIdempotent (ArbitraryExpresion exp) =
  exp `shouldNormalizeTo` normalize exp

spec :: Spec
spec = do
  describe "Normalize spec" $ do
    specify "normalize scalar one zero" $ do
      constant 0.0 *. constant 9.0 `shouldNormalizeTo` constant 0.0
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
      ((x +: y) * (z +: w)) `shouldNormalizeTo` ((x * z - y * w) +: (x * w + y * z))
      xRe (x +: y) `shouldNormalizeTo` x
      xIm (x +: y) `shouldNormalizeTo` y
      (x +: y) + (u +: v) `shouldNormalizeTo` (x + u) +: (y + v)
      s *. (x +: y) `shouldNormalizeTo` (s *. x) +: (s *. y)
      (x +: y) * (z +: w) `shouldNormalizeTo` (x * z - y * w) +: (x * w + y * z)
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
      x * sum [y, z, t, u, v] `shouldNormalizeTo` sum (map (x *) [y, z, t, u, v])
      sum [y, z, t, u, v] * x `shouldNormalizeTo` sum (map (x *) [y, z, t, u, v])
      x *. sum [y, z, t, u, v] `shouldNormalizeTo` sum (map (x *.) [y, z, t, u, v])
      x <.> sum [y, z, t, u, v] `shouldNormalizeTo` sum (map (x <.>) [y, z, t, u, v])
      sum [y, z, t, u, v] <.> x `shouldNormalizeTo` sum (map (<.> x) [y, z, t, u, v])
      product [a, b, c, sum [x, y, z]] `shouldNormalizeTo` sum (map (product . (: [a, b, c])) [x, y, z])
      (x + y) * (z + t) * a * b `shouldNormalizeTo` (a * b * x * z + a * b * x * t + a * b * y * z + a * b * y * t)
    specify "flatten sum and product" $ do
      product [x * y, product [z, t, w], one] `shouldNormalizeTo` product [x, y, z, t, w]
      sum [x + y, sum [z, t, w + s], zero] `shouldNormalizeTo` sum [x, y, z, t, w, s]
    specify "group constants together" $ do
      product [one, one, x, y, one, z] `shouldNormalizeTo` product [x, y, z]
      sum [one, one, x, y, one, z] `shouldNormalizeTo` sum [constant 3, x, y, z]
      product [constant 1, constant 2, x, y, constant 3, z] `shouldNormalizeTo` product [constant 6, x, y, z]
    specify "combine same terms" $ do
      sum [one *. x1, x1, x1, constant 3 *. y1, y1] `shouldNormalizeTo` sum [constant 3 *. x1, constant 4 *. y1]
      sum [constant (-1) *. x1, x1, constant 3 *. y1, y1, z1] `shouldNormalizeTo` sum [constant 4 *. y1, z1]
      x1 - x1 `shouldNormalizeTo` zero1
      sum [one *. x, x, x, constant 3 *. y, y] `shouldNormalizeTo` sum [constant 3 *. x, constant 4 *. y]
      sum [constant (-1) *. x, x, constant 3 *. y, y, z] `shouldNormalizeTo` sum [constant 4 *. y, z]
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
      rotate (1, 1) (rotate (2, 3) x2)
        `shouldNormalizeTo` rotate (3, 4) x2
      rotate (0, 1) (rotate (2, -3) x2)
        `shouldNormalizeTo` rotate (2, -2) x2
      rotate (0, 0, 0) x3 `shouldNormalizeTo` x3
      rotate (2, 3, 4) (s *. y3) `shouldNormalizeTo` s
        *. rotate (2, 3, 4) y3
      rotate (2, 3) (sum [x2, y2, z2])
        `shouldNormalizeTo` sum (map (rotate (2, 3)) [x2, y2, z2])
      rotate (2, 3) (product [x2, y2, z2])
        `shouldNormalizeTo` product (map (rotate (2, 3)) [x2, y2, z2])
  describe "Normalize piecewise" $
    specify "some piecewise rules" $ do
      piecewise [1] c [x, y] `shouldNormalizeTo` piecewise [1] c [x, zero] + piecewise [1] c [zero, y]
      piecewise [1] c [x + z, y] `shouldNormalizeTo` piecewise [1] c [x, zero] + piecewise [1] c [z, zero] + piecewise [1] c [zero, y]
      piecewise [1] c [x *. y, zero] `shouldNormalizeTo` x *. piecewise [1] c [y, zero]
      piecewise [1, 2] c [zero, x *. y, zero] `shouldNormalizeTo` x *. piecewise [1, 2] c [zero, y, zero]
      piecewise [4, 5] x1 [zero1, x1 * (s *. y1), zero1] `shouldNormalizeTo` s *. piecewise [4, 5] x1 [zero1, x1 * y1, zero1]
      piecewise [1] c1 [c1 +: y1, z1 +: t1] `shouldNormalizeTo` piecewise [1] c1 [c1, z1] +: piecewise [1] c1 [y1, t1]
  describe "Fourier transform" $ do
    specify "some Ft rules" $ do
      reFT (reFT x1) + imFT (imFT x1) `shouldNormalizeTo` constant (fromIntegral defaultDim1D) *. x1
      reFT (reFT x2) + imFT (imFT x2) `shouldNormalizeTo` constant (fromIntegral (default1stDim2D * default2ndDim2D)) *. x2
      let x = variable1D @10 "x"
          y = variable1D @10 "y"
          z = variable1D @10 "z"
          t = variable1D @10 "t"
          valMap =
            Map.fromList
              [ ("x", V1D $ listArray (0, 9) [1 ..]),
                ("y", V1D $ listArray (0, 9) [2, 5 ..]),
                ("z", V1D $ listArray (0, 9) [3, 8 ..]),
                ("t", V1D $ listArray (0, 9) [0, -1 ..])
              ]
      let res1 = eval valMap $ reFT (reFT (x * y + z * t))
          res2 = eval valMap . normalize $ reFT (reFT (x * y + z * t))
      res1 `shouldApprox` res2
      let res1 = eval valMap $ imFT (imFT (x * y + z * t))
          res2 = eval valMap . normalize $ imFT (imFT (x * y + z * t))
      res1 `shouldApprox` res2
      let res1 = eval valMap $ reFT (reFT (x * y + z * t)) + imFT (imFT (x * y + z * t))
          res2 = eval valMap . normalize $ reFT (reFT (x * y + z * t)) + imFT (imFT (x * y + z * t))
      res1 `shouldApprox` res2
  specify "normalize is idempotent" $ do
    property prop_NormalizeIsIdempotent
  describe "Unit tests" $
    specify "should properly normalize hard-coded exp" $ do
      x `shouldNormalizeTo` x
      constant 1 / x `shouldNormalizeTo` x ^ (-1)
      x + x `shouldNormalizeTo` constant 2 *. x
      x - x `shouldNormalizeTo` constant 0
      x * x `shouldNormalizeTo` x ^ 2
      x / x `shouldNormalizeTo` constant 1
      x + y `shouldNormalizeTo` x + y
      x - y `shouldNormalizeTo` x - y
      x * y `shouldNormalizeTo` x * y
      x / y `shouldNormalizeTo` x * y ^ (-1)
      (x + y) * (x + y) `shouldNormalizeTo` (constant 2.0 *. (x * y)) + (x ^ 2) + (y ^ 2)
      (x + y) ^ 2 `shouldNormalizeTo` (constant 2.0 *. (x * y)) + (x ^ 2) + (y ^ 2)
      (x - y) ^ 2 `shouldNormalizeTo` (constant (-2.0) *. (x * y)) + (x ^ 2) + (y ^ 2)
      (x - y) * (x - y) `shouldNormalizeTo` (constant (-2.0) *. (x * y)) + (x ^ 2) + (y ^ 2)
      (x * y) ^ 2 `shouldNormalizeTo` (x ^ 2) * (y ^ 2)
      (x * y) * (x * y) `shouldNormalizeTo` (x ^ 2) * (y ^ 2)
      (x / y) * (x / y) `shouldNormalizeTo` (y ^ (-2)) * (x ^ 2)
      (x / y) ^ 2 `shouldNormalizeTo` (y ^ (-2)) * (x ^ 2)
      (constant 1 / x) * (constant 1 / x) `shouldNormalizeTo` x ^ (-2)
      (constant 1 / x) ^ 2 `shouldNormalizeTo` x ^ (-2)
      x * x `shouldNormalizeTo` x ^ 2
      (x * x) * x `shouldNormalizeTo` x ^ 3
      constant 1 / x `shouldNormalizeTo` x ^ (-1)
      x * x / x `shouldNormalizeTo` normalize x
      x / x `shouldNormalizeTo` constant 1
      x / x / x `shouldNormalizeTo` x ^ (-1)
      x / x / y `shouldNormalizeTo` y ^ (-1)
      (x * y) ^ 3 `shouldNormalizeTo` (x ^ 3) * (y ^ 3)
      ((x * y) ^ 3) * x / y `shouldNormalizeTo` (y ^ 2) * (x ^ 4)
      ((x * y) ^ 3) * x + y `shouldNormalizeTo` y + ((y ^ 3) * (x ^ 4))
      ((x * y) ^ 3) * (x + y) `shouldNormalizeTo` ((y ^ 3) * (x ^ 4)) + ((x ^ 3) * (y ^ 4))
      x / (x ^ 3) `shouldNormalizeTo` x ^ (-2)
      (x ^ 2) ^ 2 `shouldNormalizeTo` x ^ 4
      (x + y) * (x + y) `shouldNormalizeTo` (constant 2.0 *. (x * y)) + (x ^ 2) + (y ^ 2)
      ((x + y) * (x + y)) * (x + y) `shouldNormalizeTo` (constant 3.0 *. (y * (x ^ 2))) + (constant 3.0 *. (x * (y ^ 2))) + (x ^ 3) + (y ^ 3)
      ((x * y) ^ 3) * (x + y) ^ 2 `shouldNormalizeTo` ((y ^ 3) * (x ^ 5)) + ((x ^ 3) * (y ^ 5)) + (constant 2.0 *. ((x ^ 4) * (y ^ 4)))
      (x + y) ^ 2 `shouldNormalizeTo` (constant 2.0 *. (x * y)) + (x ^ 2) + (y ^ 2)
      x / (x ^ 2) `shouldNormalizeTo` x ^ (-1)
      x / (x * x) `shouldNormalizeTo` x ^ (-1)
      x / x * x `shouldNormalizeTo` normalize x
      x / (x ^ 3) `shouldNormalizeTo` x ^ (-2)
      (x * x) / y `shouldNormalizeTo` (y ^ (-1)) * (x ^ 2)
      (x ^ 2) ^ 3 `shouldNormalizeTo` x ^ 6
      ((x ^ 3) ^ 3) ^ 2 `shouldNormalizeTo` x ^ 18
      y * (y ^ 2) ^ 2 `shouldNormalizeTo` y ^ 5
      x / ((y ^ 2) ^ 3) `shouldNormalizeTo` x * y ^ (-6)
      ((x * y) ^ 2) / x `shouldNormalizeTo` x * y ^ 2
      ((x + y) ^ 2) * ((x - y) ^ 2) `shouldNormalizeTo` (constant (-2.0) *. ((x ^ 2) * (y ^ 2))) + (x ^ 4) + (y ^ 4)
      ((x + y) ^ 2) + ((x - y) ^ 2) `shouldNormalizeTo` (constant 2.0 *. (x ^ 2)) + (constant 2.0 *. (y ^ 2))
      ((x + y) ^ 2) - ((x - y) ^ 2) `shouldNormalizeTo` constant 4.0 *. (x * y)
      (x + y) * (x - y) `shouldNormalizeTo` (constant (-1.0) *. (y ^ 2)) + (x ^ 2)
      (x + y) + (x - y) `shouldNormalizeTo` constant 2.0 *. x
      (x + y) - (x - y) `shouldNormalizeTo` constant 2.0 *. y
      (x + y) / (x - y) `shouldNormalizeTo` (x * ((x + (constant (-1.0) *. y)) ^ (-1))) + (y * ((x + (constant (-1.0) *. y)) ^ (-1)))
      x <.> y `shouldNormalizeTo` x * y
      x <.> constant 1 `shouldNormalizeTo` normalize x
      x <.> constant (-1) `shouldNormalizeTo` constant (-1) * x
      x <.> constant 0 `shouldNormalizeTo` constant 0
      x <.> x `shouldNormalizeTo` normalize x ^ 2
      x <.> x * y `shouldNormalizeTo` y * (x ^ 2)
      x <.> x + x `shouldNormalizeTo` x + (x ^ 2)
      x <.> (x + x) `shouldNormalizeTo` constant 2.0 *. (x ^ 2)
      x <.> (constant 1 / x) `shouldNormalizeTo` constant 1
      x <.> (y / x) `shouldNormalizeTo` normalize y
      (x <.> (x + y)) ^ 2 `shouldNormalizeTo` ((x ^ 2) * (y ^ 2)) + (constant 2.0 *. (y * (x ^ 3))) + (x ^ 4)
