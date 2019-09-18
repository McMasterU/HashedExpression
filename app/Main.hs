{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Array
import qualified Data.IntMap.Strict as IM
import Data.Map (empty, fromList, union)
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
import ToF.ToF

import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.STRef.Strict
import Graphics.EasyPlot
import HashedCollect
import HashedPlot
import HashedSolver
import HashedToC (singleExpressionCProgram)
import HashedUtils
import HashedVar
import Test.Hspec
import ToF.VelocityGenerator

sum1 :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
sum1 = fromJust . HashedOperation.sum

prod1 :: (DimensionType d, NumType et) => [Expression d et] -> Expression d et
prod1 = fromJust . HashedOperation.product

--
main = do
    let x = variable2D @10 @10 "x"
        y = variable2D @10 @10 "y"
        allOne = constant2D @10 @10 1
        exp = huber 2 (x - y) <.> allOne
        vars = Set.fromList ["x"]
        valMaps = fromList [("y", V2D $ listArray ((0, 0), (9, 9)) [1 ..])]
        problem = constructProblem exp vars
        codes = generateProblemCode valMaps problem
    writeFile "algorithms/lbfgs/problem.c" $ intercalate "\n" codes --main = do
--    let x = var "x"
--    let exp = huber 2 x
--    let values = empty
--    let fn = Function exp values
--    plot1VariableFunction fn "huber2"
