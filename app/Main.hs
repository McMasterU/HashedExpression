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
    let hbb = huber2 3 (x * x + y * y)
    showExp . simplify . exteriorDerivative (Set.fromList ["x", "y"]) $ hbb

--    showExp . simplify $ sqrt (x * x)
--    let g = x * x
--        f = piecewise [1] g [sqrt g, (one + g) / const 2]
--    let hb = huber 1 x
--    putStrLn "f:"
--    showExp . simplify $ f
--    putStrLn "df:"
--    showExp . simplify . exteriorDerivative (Set.fromList ["x", "y"]) $ f
--    putStrLn "f(x):"
--    print . eval (emptyVms |> withVm0 (fromList [("x", 2)])) $ f
--    showExp . simplify . exteriorDerivative (Set.fromList ["x", "y"] $ hb
