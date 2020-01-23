module Test2 where

import Commons
import Data.Maybe (fromJust)
import HashedExpression.Expression

import HashedNormalize
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
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
    describe "More normalize spec" $
    specify "Mixed normalized spec 1" $ do
        negate x `shouldNormalizeTo` const (-1) *. x
        negate x1 `shouldNormalizeTo` const (-1) *. x1
        x ^ 2 ^ 3 `shouldNormalizeTo` x ^ 6
        x ^ 3 / x ^ 2 `shouldNormalizeTo` x
        (a *. x) * (b *. y) *
            (c *. z) `shouldNormalizeTo` (a * b * c) *. (x * y * z)
