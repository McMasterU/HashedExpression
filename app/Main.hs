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

reFT :: (DimensionType d) => Expression d R -> Expression d R
reFT = xRe . ft

imFT :: (DimensionType d) => Expression d R -> Expression d R
imFT = xIm . ft

--
--main = do
--    let exp = norm2square $ reFT . reFT $ x1
--    showExp . collectDifferentials . exteriorDerivative allVars $ exp--    let x = var "x"
--    let exp = huber 1 x
--        fun = Function exp empty
--    plot1VariableFunction fun "haha"
--main = anotherFruit--main = do
--    let a = const 33
--        b = const 100
--        x = var "x"
--        y = var "y"
--        exp = huber 2 $ (a - x) ^ 2 + b * (y - x ^ 2) ^ 2
--        vars = Set.fromList ["x", "y"]
--        valMap = fromList [("x", VScalar 0), ("y", VScalar 0)]
--    let problem = constructProblem exp vars
--    case generateProblemCode valMap problem of
--        Invalid str -> putStrLn str
--        Success proceed -> proceed "algorithms/lbfgs"
main = smilingFaceProblem--main = do
--    let objectiveFunction = x - const 3
--    let valMap = fromList [("x", VScalar 19999)]
--        vars = ["x"]
--    let constraint = BoxConstraint [("x", LowerBound $ VScalar 0)]
--    let problem = constructProblem objectiveFunction vars constraint
--    case generateProblemCode valMap problem of
--        Invalid str -> putStrLn str
--        Success proceed -> proceed "algorithms/lbfgs-b"
