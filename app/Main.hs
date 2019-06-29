{-# LANGUAGE DeriveFunctor #-}

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
    , negate
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

main = do
    let f = huber 3 (x + y)
    showExp $ f
    showExp $ exteriorDerivative (Set.fromList ["x", "y"]) $ f
    showExp $ simplify $ exteriorDerivative (Set.fromList ["x", "y"]) $ f
