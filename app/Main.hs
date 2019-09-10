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
--    let exp1 =
--            Expression
--                { exIndex = 988291336315544
--                , exMap =
--                      IM.fromList
--                          [ (5610, ([], Var "s"))
--                          , (79244168, ([10], Var "g1"))
--                          , (83818464, ([10], Var "o1"))
--                          , (89536334, ([10], Var "y1"))
--                          , (546181000496153, ([10], Const 1.3450711655704364))
--                          , (585729409378185, ([10], Const 10.337048000527076))
--                          , ( 698914407939865
--                            , ([10], Const (-0.44269591460912383)))
--                          , ( 988291336315544
--                            , ( [10]
--                              , Sum R
--                                    [ 546181000496153
--                                    , 698914407939865
--                                    , 7120164784408368
--                                    ]))
--                          , (1931321307050023, ([10], Scale R 5610 89536334))
--                          , ( 7120164784408368
--                            , ( [10]
--                              , Piecewise
--                                    [0.43333585530231195, 7.752029999052966]
--                                    585729409378185
--                                    [83818464, 7532752830571992, 79244168]))
--                          , ( 7532752830571992
--                            , ([10], Rotate [5] 1931321307050023))
--                          ]
--                } :: Expression One R
--    let exp2 =
--            Expression
--                { exIndex = 1256627028655584
--                , exMap =
--                      IM.fromList
--                          [ (76385233, ([10], Var "b1"))
--                          , (83818464, ([10], Var "o1"))
--                          , (508977928577983, ([10], Const 5.402564497746867))
--                          , ( 604554557880923
--                            , ([10], Const (-9.026805492038763)))
--                          , ( 687119126216892
--                            , ([10], Const (-5.640843860509643)))
--                          , (728053771261277, ([10], Const 1.2916479477725504))
--                          , ( 741471461597824
--                            , ([10], Const (-1.0212702582599782)))
--                          , ( 1008606028332833
--                            , ( [10]
--                              , Sum R
--                                    [ 604554557880923
--                                    , 508977928577983
--                                    , 76385233
--                                    , 83818464
--                                    ]))
--                          , ( 1256627028655584
--                            , ( [10]
--                              , Mul R
--                                    [ 687119126216892
--                                    , 728053771261277
--                                    , 1008606028332833
--                                    , 741471461597824
--                                    ]))
--                          ]
--                } :: Expression One R
    let exp1 = one1
    let exp2 = piecewise [1] x1 [rotate 1 (x1 + y1), zero1]
    let exp = exp1 <.> exp2
    showExp $ collectDifferentials . exteriorDerivative allVars $ exp
