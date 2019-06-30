{-# LANGUAGE DeriveFunctor #-}

module Main where

import Data.Array.Unboxed as U
import Data.Map (fromList, union)
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
    , negate
    , sin
    , sinh
    , sqrt
    , tan
    , tanh
    )

import Test.Hspec
import Test.QuickCheck hiding (scale)
import HashedUtils ((|>))

[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z] =
    map var
        [ "a"
        , "b"
        , "c"
        , "d"
        , "e"
        , "f"
        , "g"
        , "h"
        , "i"
        , "j"
        , "k"
        , "l"
        , "m"
        , "n"
        , "o"
        , "p"
        , "q"
        , "r"
        , "s"
        , "t"
        , "u"
        , "v"
        , "w"
        , "x"
        , "y"
        , "z"
        ]

zero = const 0

main
--    let f = huber 3 (x + y)
--    showExp $ f
--    showExp $ exteriorDerivative (Set.fromList ["x", "y"]) $ f
--    showExp $ simplify $ exteriorDerivative (Set.fromList ["x", "y"]) $ f
 = do
    print "Hello World"
    let exp1 = ((zero+negate(zero))+zero+zero+zero+(zero<.>t))
        exp2 = (zero*zero*c*c*((zero+c+u+a+(zero<.>i))<.>(((a*.u)*s*((b+negate((n*zero*f*zero*(zero*.j))))<.>e)*(zero+r+zero)*zero)*.zero))*b)
    let exp = simplify $ exp1 + exp2
    showExp $ exp
