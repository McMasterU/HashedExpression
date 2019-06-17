module Main where

import Data.Array.Unboxed as U
import HashedDerivative
import HashedExpression
import HashedFactor
import HashedInterp
import HashedOperation
import HashedPrettify
import HashedSimplify
import Prelude hiding
    ( (*)
    , (+)
    , (-)
    , (/)
    , acos
    , acosh
    , asin
    , asinh
    , atan
    , atanh
    , cos
    , cosh
    , exp
    , log
    , sin
    , sinh
    , sqrt
    , tan
    , tanh
    )

import Test.Hspec
import Test.QuickCheck hiding (scale)

main = do
    let x = var1d 10 "x"
    let y = var1d 10 "y"
    let z = var1d 10 "z"
    let s = var "s"
--    let f = s * (x + y + z) + x + y
    let f = log $ exp $ sqrt $ cos $ s `scale` (x * y) / y
    print $ prettify f
    print $ prettify $ exteriorDerivative f
--    print $ prettify . exteriorDerivative $ f
