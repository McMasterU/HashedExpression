module Main where

import Data.Array.Unboxed as U
import qualified Data.Set as Set
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
    , const
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
        y = var1d 10 "y"
        z = var1d 10 "z"
        s = var "s"
        f = log $ exp $ sqrt $ cos $ s *. (x * y) / y
        anotherF = const 0 *. f
        fImg = const 0 *. xRe (f +: z)
--    print $ prettify anotherF
    print $ prettify fImg
    print $ prettify $ exteriorDerivative (Set.fromList ["x", "y", "z"]) fImg
    print $
        prettify $
        exteriorDerivative (Set.fromList ["x", "y", "z"]) $ simplify fImg
--    print $ prettify $ exteriorDerivative fImg
--    print $ prettify . exteriorDerivative $ f
