{-# LANGUAGE DataKinds #-}

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
--main = do
--    let x = variable2D "x" :: Expression '( 10, 10) R
--    let y = variable2D "y" :: Expression '( 10, 10) R
--    let exp = (x - y) <.> (x - y)
--    let vars = Set.fromList ["x"]
--    let valMaps = fromList [("y", V2D $ listArray ((0, 0), (9, 9)) [1 ..])]
--    let problem = constructProblem exp vars
--    let codes = generateProblemCode valMaps problem
--    writeFile "algorithms/lbfgs/problem.c" $ intercalate "\n" codes 
main = do
    let x = var "x"
    let exp = huber 2 x
    let values = empty
    let fn = Function exp values
    plot1VariableFunction fn "huber2"
