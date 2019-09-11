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
--    let exp = huber 1 x2 <.> one2
--    let vars = Set.fromList ["x2"]
--    showExp $ collectDifferentials . exteriorDerivative vars $ exp
--    let valMaps = fromList []
--    let problem = constructProblem exp vars
--    let codes = generateProblemCode valMaps problem
--    writeFile "algorithms/lbfgs/problem.c" $ intercalate "\n" codes--main = do
--    let exp = piecewise [1, 2, 3] x [y, z, t, x]
--    let valMaps =
--            fromList
--                [ ("x", VScalar 1)
--                , ("y", VScalar 1)
--                , ("z", VScalar 2)
--                , ("t", VScalar 3)
--                ]
--    let program = singleExpressionCProgram valMaps exp
--    let fileName = "haha"
--    let fullFileName = "C/" ++ fileName ++ ".c"
--    let program = singleExpressionCProgram valMaps exp
--    writeFile fullFileName (intercalate "\n" program)
main = do
    let exp =
            piecewise
                [-15.631805560907928, 44.77502422279068]
                i1
                [rotate 8 w1, zero1, zero1]
    let valMaps =
            fromList
                [ ( "i1"
                  , V1D (array
                             (0, 9)
                             [ (0, -40.195765019319865)
                             , (1, 32.73024460317578)
                             , (2, 5.014279519082707)
                             , (3, 8.643093253538066)
                             , (4, -16.615436918501835)
                             , (5, 6.857383705589953)
                             , (6, 12.150806150687048)
                             , (7, -56.97237186263389)
                             , (8, 10.378510423996962)
                             , (9, 25.698093462354326)
                             ]))
                , ( "w1"
                  , V1D (array
                             (0, 9)
                             [ (0, 33.524568575004395)
                             , (1, 16.643983584957276)
                             , (2, -58.027930192704986)
                             , (3, -5.232075657334483)
                             , (4, 61.38815812695504)
                             , (5, 6.0722571973491375)
                             , (6, 69.6439523981028)
                             , (7, 59.76791027804917)
                             , (8, 30.51421673957095)
                             , (9, -16.009449781895622)
                             ]))
                ]
    showExp $ simplify exp
    print $ eval valMaps exp
    print $ eval valMaps $ simplify exp
