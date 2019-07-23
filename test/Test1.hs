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
    describe "Simplify spec" $
    specify "simplify scalar one zero" $ do
        x `shouldSimplifyTo` x
        const 1 / x `shouldSimplifyTo` x ^ (-1)
        x + x `shouldSimplifyTo` const 2 *. x
        x - x `shouldSimplifyTo` const 0
        x * x `shouldSimplifyTo` x ^ 2
        x / x `shouldSimplifyTo` const 1
        x + y `shouldSimplifyTo` x + y
        x - y `shouldSimplifyTo` x - y
        x * y `shouldSimplifyTo` x * y
        x / y `shouldSimplifyTo` x * y ^ (-1)
        (x + y) * (x + y) `shouldSimplifyTo` (const 2.0 *. (x * y)) + (x ^ 2) +
            (y ^ 2)
        (x + y) ^ 2 `shouldSimplifyTo` (const 2.0 *. (x * y)) + (x ^ 2) +
            (y ^ 2)
        (x - y) ^ 2 `shouldSimplifyTo` (const (-2.0) *. (x * y)) + (x ^ 2) +
            (y ^ 2)
        (x - y) * (x - y) `shouldSimplifyTo` (const (-2.0) *. (x * y)) + (x ^ 2) +
            (y ^ 2)
        (x * y) ^ 2 `shouldSimplifyTo` (x ^ 2) * (y ^ 2)
        (x * y) * (x * y) `shouldSimplifyTo` (x ^ 2) * (y ^ 2)
        (x / y) * (x / y) `shouldSimplifyTo` (y ^ (-2)) * (x ^ 2)
        (x / y) ^ 2 `shouldSimplifyTo` (y ^ (-2)) * (x ^ 2)
        (const 1 / x) * (const 1 / x) `shouldSimplifyTo` x ^ (-2)
        (const 1 / x) ^ 2 `shouldSimplifyTo` x ^ (-2)
        x * x `shouldSimplifyTo` x ^ 2
        (x * x) * x `shouldSimplifyTo` x ^ 3
        const 1 / x `shouldSimplifyTo` x ^ (-1)
        x * x / x `shouldSimplifyTo` simplify x
        x / x `shouldSimplifyTo` const 1
        x / x / x `shouldSimplifyTo` x ^ (-1)
        x / x / y `shouldSimplifyTo` y ^ (-1)
        (x * y) ^ 3 `shouldSimplifyTo` (x ^ 3) * (y ^ 3)
        ((x * y) ^ 3) * x / y `shouldSimplifyTo` (y ^ 2) * (x ^ 4)
        ((x * y) ^ 3) * x + y `shouldSimplifyTo` y + ((y ^ 3) * (x ^ 4)) --Without Paranthesis
        ((x * y) ^ 3) * (x + y) `shouldSimplifyTo` ((y ^ 3) * (x ^ 4)) +
            ((x ^ 3) * (y ^ 4)) -- With paranthesis
        x / (x ^ 3) `shouldSimplifyTo` x ^ (-2) --FIXED
        (x ^ 2) ^ 2 `shouldSimplifyTo` x ^ 4
        (x + y) * (x + y) `shouldSimplifyTo` (const 2.0 *. (x * y)) + (x ^ 2) +
            (y ^ 2)
        ((x + y) * (x + y)) *
            (x + y) `shouldSimplifyTo` (const 3.0 *. (y * (x ^ 2))) +
            (const 3.0 *. (x * (y ^ 2))) +
            (x ^ 3) +
            (y ^ 3)
        ((x * y) ^ 3) * (x + y) ^ 2 `shouldSimplifyTo` ((y ^ 3) * (x ^ 5)) +
            ((x ^ 3) * (y ^ 5)) +
            (const 2.0 *. ((x ^ 4) * (y ^ 4)))
        (x + y) ^ 2 `shouldSimplifyTo` (const 2.0 *. (x * y)) + (x ^ 2) +
            (y ^ 2)
        x / (x ^ 2) `shouldSimplifyTo` x ^ (-1)
        x / (x * x) `shouldSimplifyTo` x ^ (-1) --with paranthesis
        x / x * x `shouldSimplifyTo` simplify x -- without paranthesis
        x / (x ^ 3) `shouldSimplifyTo` x ^ (-2) -- FIXED
        (x * x) / y `shouldSimplifyTo` (y ^ (-1)) * (x ^ 2)
        (x ^ 2) ^ 3 `shouldSimplifyTo` x ^ 6
        ((x ^ 3) ^ 3) ^ 2 `shouldSimplifyTo` x ^ 18
        y * (y ^ 2) ^ 2 `shouldSimplifyTo` y ^ 5
        x / ((y ^ 2) ^ 3) `shouldSimplifyTo` x * y ^ (-6)
        ((x * y) ^ 2) / x `shouldSimplifyTo` x * y ^ 2
        ((x + y) ^ 2) *
            ((x - y) ^ 2) `shouldSimplifyTo`
            (const (-2.0) *. ((x ^ 2) * (y ^ 2))) +
            (x ^ 4) +
            (y ^ 4)
        ((x + y) ^ 2) + ((x - y) ^ 2) `shouldSimplifyTo` (const 2.0 *. (x ^ 2)) +
            (const 2.0 *. (y ^ 2))
        ((x + y) ^ 2) - ((x - y) ^ 2) `shouldSimplifyTo` const 4.0 *. (x * y)
        (x + y) * (x - y) `shouldSimplifyTo` (const (-1.0) *. (y ^ 2)) + (x ^ 2)
        (x + y) + (x - y) `shouldSimplifyTo` const 2.0 *. x
        (x + y) - (x - y) `shouldSimplifyTo` const 2.0 *. y
        (x + y) /
            (x - y) `shouldSimplifyTo` (x * ((x + (const (-1.0) *. y)) ^ (-1))) +
            (y * ((x + (const (-1.0) *. y)) ^ (-1)))
        x <.> y `shouldSimplifyTo` x * y
        x <.> const 1 `shouldSimplifyTo` simplify x
        x <.> const (-1) `shouldSimplifyTo` const (-1) * x
        x <.> const 0 `shouldSimplifyTo` const 0
        x <.> x `shouldSimplifyTo` simplify x ^ 2
        x <.> x * y `shouldSimplifyTo` y * (x ^ 2)
        x <.> x + x `shouldSimplifyTo` x + (x ^ 2)
        x <.> (x + x) `shouldSimplifyTo` const 2.0 *. (x ^ 2)
        x <.> (const 1 / x) `shouldSimplifyTo` const 1
        x <.> (y / x) `shouldSimplifyTo` simplify y
        (x <.> (x + y)) ^ 2 `shouldSimplifyTo` ((x ^ 2) * (y ^ 2)) +
            (const 2.0 *. (y * (x ^ 3))) +
            (x ^ 4)
