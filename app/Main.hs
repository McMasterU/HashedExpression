{-# LANGUAGE DeriveFunctor #-}

module Main where

import Data.Array.Unboxed as U
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

import Data.Maybe (fromJust)
import HashedToC
import HashedUtils
import HashedVar
import Test.Hspec
import Test.QuickCheck hiding (scale)
import Data.List (intercalate)

main = do
    let f = (x * y * z + one + x1 <.> y1) * z
    showExp f
    let program = generateProgram emptyVms f
    writeFile "C/f.c" (intercalate "\n" program)

