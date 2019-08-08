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
import ToF

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

main = do
    let (problem, valMaps, (vX, vY)) = tof2DStraight (15, 15) 5 5 0.15
    print problem
    putStrLn $ unwords . map show . elems $ vX
    putStrLn $ unwords . map show . elems $ vY
    let code = generateProblemCode valMaps problem
    let filePath = "algorithms/gradient_descent/problem.c"
    writeFile filePath $ intercalate "\n" code
    

--main = do
--    let (vX, vY) = quarterCircleFlow (20, 20) 7 6 0.15
