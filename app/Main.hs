module Main where

import Data.Array
import qualified Data.IntMap.Strict as IM
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
import ToF.ToF

import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.STRef.Strict
import HashedCollect
import HashedSolver
import HashedToC (singleExpressionCProgram)
import HashedUtils
import HashedVar
import Test.Hspec
import ToF.VelocityGenerator

sum1 :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
sum1 = fromJust . HashedOperation.sum

prod1 :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
prod1 = fromJust . HashedOperation.sum

--main = do
--    let (problem, valMaps, (vX, vY)) = tof2DQuarterCircle (50, 50) 30 10 0.15
--    putStrLn $ unwords . map show . elems $ vX
--    putStrLn $ unwords . map show . elems $ vY
--    let code = generateProblemCode valMaps problem
--    let filePath = "algorithms/lbfgs/problem.c"
--    writeFile filePath $ intercalate "\n" code
--main = do
--    let (vX, vY) = quarterCircleFlow (20, 20) 7 6 0.15
--main = do
--    let exp = (x2 - y2) <.> (x2 - y2)
--    let vars = Set.fromList ["x2"]
--    showExp $ collectDifferentials . exteriorDerivative vars $ exp
--    let valMaps =
--            emptyVms |>
--            withVm2 (fromList [("y2", listArray ((0, 0), (9, 9)) [1 .. 100])])
--    let problem = constructProblem exp vars
--    print problem
--    let codes = generateProblemCode valMaps problem
--    writeFile "algorithms/lbfgs/problem.c" $ intercalate "\n" codes
main = do
    let exp = piecewise [1, 2, 3] x [y, z, t, x]
    let valMaps =
            fromList
                [ ("x", VScalar 1)
                , ("y", VScalar 1)
                , ("z", VScalar 2)
                , ("t", VScalar 3)
                ]
    let program = singleExpressionCProgram valMaps exp
    let fileName = "haha"
    let fullFileName = "C/" ++ fileName ++ ".c"
    let program = singleExpressionCProgram valMaps exp
    writeFile fullFileName (intercalate "\n" program)
