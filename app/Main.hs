module Main where

import Data.Array.Unboxed as U
import HashedDerivative
import HashedExpression
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
    , const
    )

import Test.Hspec
import Test.QuickCheck hiding (scale)

main = do
    let x = var1d 10 "x"
        y = var1d 10 "y"
        z = var1d 10 "z"
        s = var "s"
        f = log $ exp $ sqrt $ cos $ s *. (x * y) / y
        anotherF = const 0 *. f
        fImg = xRe $ f +: z
    print $ prettify anotherF
    print $ prettify $ exteriorDerivative anotherF
--    print $ prettify $ exteriorDerivative fImg
--    print $ prettify . exteriorDerivative $ f
