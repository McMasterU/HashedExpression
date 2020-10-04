module SimplifySpec where

import Commons
import Data.Complex (Complex (..))
import Data.Map.Strict (union)
import Data.Maybe (fromJust)
import Debug.Trace (traceShowId)
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Simplify
import HashedExpression.Utils
import HashedExpression.Interp
import HashedExpression.Operation hiding (product, sum)
import qualified HashedExpression.Operation
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
  prettify (simplify exp1) `shouldBe` prettify (simplify exp2)
  simplify exp1 `shouldBe` simplify exp2

-- |
prop_sameValueInterpScalarR :: SuiteScalarR -> Expectation
prop_sameValueInterpScalarR (Suite exp valMap) =
  eval valMap exp `shouldApprox` eval valMap (simplify exp)

prop_sameValueInterpScalarC :: SuiteScalarC -> Expectation
prop_sameValueInterpScalarC (Suite exp valMap) =
  eval valMap exp `shouldApprox` eval valMap (simplify exp)

prop_sameValueInterpOneR :: SuiteOneR -> Expectation
prop_sameValueInterpOneR (Suite exp valMap) =
  eval valMap exp `shouldApprox` eval valMap (simplify exp)

prop_sameValueInterpOneC :: SuiteOneC -> Expectation
prop_sameValueInterpOneC (Suite exp valMap) =
  eval valMap exp `shouldApprox` eval valMap (simplify exp)

prop_sameValueInterpTwoR :: SuiteTwoR -> Expectation
prop_sameValueInterpTwoR (Suite exp valMap) =
  eval valMap exp `shouldApprox` eval valMap (simplify exp)

prop_sameValueInterpTwoC :: SuiteTwoC -> Expectation
prop_sameValueInterpTwoC (Suite exp valMap) =
  eval valMap exp `shouldApprox` eval valMap (simplify exp)

spec :: Spec
spec =
  describe "simplify spec" $ do
    specify "same value interp scalar R" $ property prop_sameValueInterpScalarR
    specify "same value interp scalar C" $ property prop_sameValueInterpScalarC
    specify "same value interp one R" $ property prop_sameValueInterpOneR
    specify "same value interp one C" $ property prop_sameValueInterpOneC
    specify "same value interp two R" $ property prop_sameValueInterpTwoR
    specify "same value interp two C" $ property prop_sameValueInterpTwoC
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
      negate (negate x) `shouldSimplifyTo` simplify x
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
