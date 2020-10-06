module SimplifySpec where

import Commons
import HashedExpression.Internal.Base
import HashedExpression.Internal.Simplify
import HashedExpression.Interp hiding
  ( product,
    sum,
  )
import HashedExpression.Modeling.Typed
import HashedExpression.Prettify
import Test.Hspec
import Test.QuickCheck (property)
import Var
import Prelude hiding ((^))

infix 1 `shouldSimplifyTo`

shouldSimplifyTo ::
  (HasCallStack, Dimension d) =>
  Expression d et ->
  Expression d et ->
  IO ()
shouldSimplifyTo exp1 exp2 = do
  prettify (simplify (asExpression exp1)) `shouldBe` prettify (simplify (asExpression exp2))
  simplify (asExpression exp1)`shouldBe` simplify (asExpression exp2)

prop_sameValueInterp :: XSuite -> Expectation
prop_sameValueInterp (XSuite expr valMap) =
  eval valMap expr `shouldApprox` eval valMap (simplify expr)

spec :: Spec
spec =
  describe "simplify spec" $ do
    specify "same value interp" $ property prop_sameValueInterp
    specify "simplify scalar one zero" $ do
      constant 0.0 *. constant 9.0 `shouldSimplifyTo` constant 0.0
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
      xRe (x +: y) `shouldSimplifyTo` x
      xIm (x +: y) `shouldSimplifyTo` y
    specify "dot product" $ do
      x <.> zero `shouldSimplifyTo` zero
      zero <.> x `shouldSimplifyTo` zero
      ((s *. x) <.> y) `shouldSimplifyTo` (s *. (x <.> y))
      x <.> (s *. y) `shouldSimplifyTo` s *. (x <.> y)
    specify "flatten sum and product" $ do
      product [x * y, product [z, t, w], one] `shouldSimplifyTo` product [x, y, z, t, w]
      sum [x + y, sum [z, t, w + s], zero] `shouldSimplifyTo` sum [x, y, z, t, w, s]
    specify "group constants together" $ do
      product [one, one, x, y, one, z] `shouldSimplifyTo` product [x, y, z]
      sum [one, one, x, y, one, z] `shouldSimplifyTo` sum [constant 3, x, y, z]
      product [constant 1, constant 2, x, y, constant 3, z] `shouldSimplifyTo` product [constant 6, x, y, z]
    specify "combine same terms" $ do
      sum [one *. x1, x1, x1, constant 3 *. y1, y1] `shouldSimplifyTo` sum [constant 3 *. x1, constant 4 *. y1]
      sum [constant (-1) *. x1, x1, constant 3 *. y1, y1, z1] `shouldSimplifyTo` sum [constant 4 *. y1, z1]
      x1 - x1 `shouldSimplifyTo` zero1
      sum [one *. x, x, x, constant 3 *. y, y] `shouldSimplifyTo` sum [constant 3 *. x, constant 4 *. y]
      sum [constant (-1) *. x, x, constant 3 *. y, y, z] `shouldSimplifyTo` sum [constant 4 *. y, z]
      x - x `shouldSimplifyTo` zero
    specify "scale rules" $ do
      x *. (y *. v) `shouldSimplifyTo` (x * y) *. v
    specify "negate rules" $ do
      negate (negate x) `shouldSimplifyTo` x
      negate (negate (x + y)) `shouldSimplifyTo` (x + y)
      negate zero `shouldSimplifyTo` zero
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
    specify "log and exp higher" $ do
      log (exp x1) `shouldSimplifyTo` x1
      exp (log x1) `shouldSimplifyTo` x1
      log (exp x2) `shouldSimplifyTo` x2
      exp (log x2) `shouldSimplifyTo` x2
    specify "rotate rules" $ do
      rotate 1 (rotate 2 x1) `shouldSimplifyTo` rotate 3 x1
      rotate 2 (rotate 3 x1) `shouldSimplifyTo` rotate 5 x1
      rotate (1, 1) (rotate (2, 3) x2) `shouldSimplifyTo` rotate (3, 4) x2
      rotate (0, 1) (rotate (2, -3) x2) `shouldSimplifyTo` rotate (2, -2) x2
      rotate (0, 0, 0) x3 `shouldSimplifyTo` x3
      rotate (2, 3, 4) (s *. y3) `shouldSimplifyTo` s *. rotate (2, 3, 4) y3
    specify "more unit tests" $ do
      x `shouldSimplifyTo` x
      constant 1 / x `shouldSimplifyTo` x ^ (-1)
      x + x `shouldSimplifyTo` constant 2 *. x
      x - x `shouldSimplifyTo` constant 0
      x * x `shouldSimplifyTo` x ^ 2
      x / x `shouldSimplifyTo` constant 1
      x + y `shouldSimplifyTo` x + y
      x - y `shouldSimplifyTo` x - y
      x * y `shouldSimplifyTo` x * y
      x / y `shouldSimplifyTo` x * y ^ (-1)
      (x * y) * (x * y) `shouldSimplifyTo` (x ^ 2) * (y ^ 2)
      (x / y) * (x / y) `shouldSimplifyTo` (y ^ (-2)) * (x ^ 2)
