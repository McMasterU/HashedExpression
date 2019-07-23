module Test2 where

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
    describe "More simplify spec" $
    specify "Mixed simplified spec 1" $ do
        negate x `shouldSimplifyTo` const (-1) *. x
        negate x1 `shouldSimplifyTo` const (-1) *. x1
        x ^ 2 ^ 3 `shouldSimplifyTo` x ^ 6
        x ^ 3 / x ^ 2 `shouldSimplifyTo` x
        (a *. x) * (b *. y) *
            (c *. z) `shouldSimplifyTo` (a * b * c) *. (x * y * z)
