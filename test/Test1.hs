module Test1 where

import Commons
import Data.Maybe (fromJust)
import HashedExpression.Internal.Expression

import HashedExpression.Internal.Normalize
import HashedExpression.Operation hiding (product, sum)
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import Prelude hiding ((^))
import Test.Hspec
import Var

spec :: Spec
spec =
    describe "Normalize spec" $
    specify "normalize scalar one zero" $ do
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
        (x + y) * (x + y) `shouldNormalizeTo` (constant 2.0 *. (x * y)) +
            (x ^ 2) +
            (y ^ 2)
        (x + y) ^ 2 `shouldNormalizeTo` (constant 2.0 *. (x * y)) + (x ^ 2) +
            (y ^ 2)
        (x - y) ^ 2 `shouldNormalizeTo` (constant (-2.0) *. (x * y)) + (x ^ 2) +
            (y ^ 2)
        (x - y) * (x - y) `shouldNormalizeTo` (constant (-2.0) *. (x * y)) +
            (x ^ 2) +
            (y ^ 2)
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
        ((x * y) ^ 3) * (x + y) `shouldNormalizeTo` ((y ^ 3) * (x ^ 4)) +
            ((x ^ 3) * (y ^ 4)) -- With paranthesis
        x / (x ^ 3) `shouldNormalizeTo` x ^ (-2) --FIXED
        (x ^ 2) ^ 2 `shouldNormalizeTo` x ^ 4
        (x + y) * (x + y) `shouldNormalizeTo` (constant 2.0 *. (x * y)) +
            (x ^ 2) +
            (y ^ 2)
        ((x + y) * (x + y)) *
            (x + y) `shouldNormalizeTo` (constant 3.0 *. (y * (x ^ 2))) +
            (constant 3.0 *. (x * (y ^ 2))) +
            (x ^ 3) +
            (y ^ 3)
        ((x * y) ^ 3) * (x + y) ^ 2 `shouldNormalizeTo` ((y ^ 3) * (x ^ 5)) +
            ((x ^ 3) * (y ^ 5)) +
            (constant 2.0 *. ((x ^ 4) * (y ^ 4)))
        (x + y) ^ 2 `shouldNormalizeTo` (constant 2.0 *. (x * y)) + (x ^ 2) +
            (y ^ 2)
        x / (x ^ 2) `shouldNormalizeTo` x ^ (-1)
        x / (x * x) `shouldNormalizeTo` x ^ (-1) --with paranthesis
        x / x * x `shouldNormalizeTo` normalize x -- without paranthesis
        x / (x ^ 3) `shouldNormalizeTo` x ^ (-2) -- FIXED
        (x * x) / y `shouldNormalizeTo` (y ^ (-1)) * (x ^ 2)
        (x ^ 2) ^ 3 `shouldNormalizeTo` x ^ 6
        ((x ^ 3) ^ 3) ^ 2 `shouldNormalizeTo` x ^ 18
        y * (y ^ 2) ^ 2 `shouldNormalizeTo` y ^ 5
        x / ((y ^ 2) ^ 3) `shouldNormalizeTo` x * y ^ (-6)
        ((x * y) ^ 2) / x `shouldNormalizeTo` x * y ^ 2
        ((x + y) ^ 2) *
            ((x - y) ^ 2) `shouldNormalizeTo`
            (constant (-2.0) *. ((x ^ 2) * (y ^ 2))) +
            (x ^ 4) +
            (y ^ 4)
        ((x + y) ^ 2) +
            ((x - y) ^ 2) `shouldNormalizeTo` (constant 2.0 *. (x ^ 2)) +
            (constant 2.0 *. (y ^ 2))
        ((x + y) ^ 2) -
            ((x - y) ^ 2) `shouldNormalizeTo` constant 4.0 *. (x * y)
        (x + y) * (x - y) `shouldNormalizeTo` (constant (-1.0) *. (y ^ 2)) +
            (x ^ 2)
        (x + y) + (x - y) `shouldNormalizeTo` constant 2.0 *. x
        (x + y) - (x - y) `shouldNormalizeTo` constant 2.0 *. y
        (x + y) /
            (x - y) `shouldNormalizeTo`
            (x * ((x + (constant (-1.0) *. y)) ^ (-1))) +
            (y * ((x + (constant (-1.0) *. y)) ^ (-1)))
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
        (x <.> (x + y)) ^ 2 `shouldNormalizeTo` ((x ^ 2) * (y ^ 2)) +
            (constant 2.0 *. (y * (x ^ 3))) +
            (x ^ 4)
