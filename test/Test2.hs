module Test2 where

import Commons
import Data.Maybe (fromJust)
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Normalize
import HashedExpression.Operation hiding (product, sum)
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import Test.Hspec
import Var
import Prelude hiding ((^))

spec :: Spec
spec =
  describe "More normalize spec"
    $ specify "Mixed normalized spec 1"
    $ do
      negate x `shouldNormalizeTo` constant (-1) *. x
      negate x1 `shouldNormalizeTo` constant (-1) *. x1
      x ^ 2 ^ 3 `shouldNormalizeTo` x ^ 6
      x ^ 3 / x ^ 2 `shouldNormalizeTo` x
      (a *. x) * (b *. y)
        * (c *. z) `shouldNormalizeTo` (a * b * c) *. (x * y * z)
