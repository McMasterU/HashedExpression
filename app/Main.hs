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
import HashedUtils ((|>))
import HashedVar
import Test.Hspec
import Test.QuickCheck hiding (scale)

main = do
    let a = (const (-2) * x) ^ (-3)
    showExp a
    showExp . exteriorDerivative (Set.fromList ["x", "y"]) $
        simplify $ simplify a
    let b = const 0 * x
    showExp $ simplify b
