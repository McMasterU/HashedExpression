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

import HashedUtils ((|>))
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

zero = const 0

--f =
--    Expression
--        { exIndex = -3680676852144642292
--        , exMap =
--              fromList
--                  [ (-9181962717066196048, ([], Neg C 3381285164071837012))
--                  , (-8121137239010252842, ([], Const 2.2761298784609183))
--                  , ( -3680676852144642292 , ([], Sum R [-9181962717066196048, 58471637580]))
--                  , (112, ([], Var "p"))
--                  , (119, ([], Var "w"))
--                  , (822568, ([], RealImag 119 112))
--                  , (58471637580, ([], Neg C 822568))
--                  , (79088992115, ([], Const zero))
--                  , ( 3381285164071837012 , ([], RealImag (-8121137239010252842) 79088992115))
--                  ]
--        }
main
--    let f = huber 3 (x + y)
--    showExp f
--    showExp $ exteriorDerivative (Set.fromList ["x", "y"]) f
--    showExp $ simplify $ exteriorDerivative (Set.fromList ["x", "y"]) f
 = do
    let f =
            (((const 1.0327603950665392 +: const 0.0) *. (d +: p)) +
             ((const 1.0327603950665392 +: const 0.0) *. (d +: p)))
--    print $
--        eval (emptyVms |> withVm0 (fromList [("h", 1), ("o", 1)])) $ f + f
    showExp f
    showExp $ simplify f
