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
import HashedOperation
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
import Problems
import RecoverKSpace.RecoverKSpace
import Test.Hspec
import ToF.VelocityGenerator

--main = do
--    let exp = const 2 * x
--    showExp $ introduceZeroPartialDerivatives [("y", [2, 3])] . collectDifferentials . exteriorDerivative allVars $ exp
--main = do
--    let [x, y] = map (variable2D @128 @128) ["x", "y"]
--        [zero, one] = map (constant2D @128 @128) [1, 0]
--    let objectiveFunction = piecewise [1] x [zero, one] <.> one
--    let vars = ["x", "y"]
--    let constraint = IPOPTConstraint [const 2 * (x <.> x) .>= VScalar 1]
----    let (ProblemValid problem) =
--    print $ constructProblem objectiveFunction vars constraint --    let valMap =
--            fromList
--                [ ("x", V2D $ listArray ((0, 0), (127, 127)) $ repeat 0)
--                , ("y", V2D $ listArray ((0, 0), (127, 127)) $ repeat 0)
--                ]
--    case generateProblemCode valMap problem of
--        Invalid str -> putStrLn str
--        Success proceed -> proceed "algorithms/lbfgs-b"
--    print $ problem
main = do
    let (vx, vy) = quarterCircleFlow (50, 50) 25 15 0.2
    writeFile "vx.txt" $ unwords . map show . elems $ vx
    writeFile "vy.txt" $ unwords . map show . elems $ vy
--main = do
--    let [x, y] = map (variable2D @128 @128) ["x", "y"]
--        [zero, one] = map (constant2D @128 @128) [1, 0]
--    let objectiveFunction = x <.> y
--    let vars = ["x", "y"]
--    let constraint = Constraint [x .>= VNum 1, y .>= VNum 1]
--    let (ProblemValid problem) =
--            constructProblem objectiveFunction vars constraint
--    let valMap = fromList [("x", VNum 4), ("y", VNum 5)]
--    case generateProblemCode valMap problem of
--        Invalid str -> putStrLn str
--        Success proceed -> proceed "algorithms/lbfgs-b"
