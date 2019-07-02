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
import HashedVar
import Test.Hspec
import Test.QuickCheck hiding (scale)

main = do
--    let f = rotate (2, 3) (x2 + y2)
--    showExp f
--    showExp $ exteriorDerivative (Set.fromList ["X2", "Y2"]) f
--    let f = piecewise [-3, 3] x [x + const 3, x + const 3, x + const 3]
--    showExp f
--    showExp . simplify $ f
    let f = huber 3 (x * x + y * y)
--    showExp f
    showExp . simplify . exteriorDerivative (Set.fromList ["x", "y"])$ f

