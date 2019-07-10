{-# LANGUAGE DeriveFunctor #-}

module Main where

import Data.Array
import Data.Map (fromList, union)
import qualified Data.Set as Set
import HashedDerivative
import HashedExpression
import HashedInterp
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
import HashedSimplify
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
    , tan
    , tanh
    )

import Data.List (intercalate)
import Data.Maybe (fromJust)
import HashedToC
import HashedUtils
import HashedVar
import Test.Hspec
import Test.QuickCheck hiding (scale)

main = do
    let f = (x * y ^ 2 * z + one + x1 <.> y1 + x2 <.> y2) * z
    showExp f
    let valMaps =
            emptyVms |> withVm0 (fromList [("x", 1), ("y", 1.213), ("z", 3)])
--            |> withVm1
--                (fromList
--                     [ ("X1", listArray (0, 9) [1 .. 10])
--                     , ("Y1", listArray (0, 9) [2 .. 11])
--                     ]) |>
--            withVm2
--                (fromList
--                     [ ("X2", listArray ((0, 0), (9, 9)) [1 .. 100])
--                     , ("Y2", listArray ((0, 0), (9, 9)) [2 .. 101])
--                     ])
    let program = generateProgram valMaps f
    writeFile "C/main.c" (intercalate "\n" program)
