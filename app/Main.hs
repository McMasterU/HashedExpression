{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Array
import Data.Complex
import qualified Data.IntMap.Strict as IM
import Data.Map (empty, fromList, union)
import qualified Data.Set as Set
import HashedDerivative
import HashedExpression
import HashedInterp
import HashedNormalize
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
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
import ToF.ToF

import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.STRef.Strict
import Fruit.Fruit
import Graphics.EasyPlot
import HashedCollect
import HashedPlot
import HashedSolver
import HashedToC (singleExpressionCProgram)
import HashedUtils
import HashedVar
import RecoverKSpace.RecoverKSpace
import Test.Hspec
import ToF.VelocityGenerator
import Problems

reFT :: (DimensionType d) => Expression d R -> Expression d R
reFT = xRe . ft

imFT :: (DimensionType d) => Expression d R -> Expression d R
imFT = xIm . ft

--main = do
--    let exp = const 2 * x
--    showExp $ introduceZeroPartialDerivatives [("y", [2, 3])] . collectDifferentials . exteriorDerivative allVars $ exp
--main = do
--    let [x, y] = map (variable2D @128 @128) ["x", "y"]
--    let objectiveFunction = sumElements (x * x + y)
--    let valMap =
--            fromList
--                [ ("x", V2D $ listArray ((0, 0), (127, 127)) $ repeat 0)
--                , ("y", V2D $ listArray ((0, 0), (127, 127)) $ repeat 0)
--                ]
--    let vars = ["x", "y"]
--    let constraint = IPOPTConstraint [const 2 * (x <.> x) .>= VScalar 1]
--    let (ProblemValid problem) =
--            constructProblem objectiveFunction vars constraint
--    case generateProblemCode valMap problem of
--        Invalid str -> putStrLn str
--        Success proceed -> proceed "algorithms/lbfgs-b"
--    print $ problem
main = bananaFunction
