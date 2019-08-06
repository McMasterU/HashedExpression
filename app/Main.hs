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

import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.STRef.Strict
import HashedCollect
import HashedSolver
import HashedToC (singleExpressionCProgram)
import HashedUtils
import HashedVar
import Test.Hspec
import Test.QuickCheck hiding (scale)

sum1 :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
sum1 = fromJust . HashedOperation.sum

prod1 :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
prod1 = fromJust . HashedOperation.sum

--main = do
--    let exp = sqrt ((x2 - y2) <.> (x2 - y2))
--    let vars = Set.fromList ["x2"]
--    showExp $ collectDifferentials . exteriorDerivative vars $ exp
--    let valMaps =
--            emptyVms |>
--            withVm2 (fromList [("y2", listArray ((0, 0), (9, 9)) [1 .. 100])])
--    let problem = constructProblem exp vars
--    let codes = generateProblemCode valMaps problem
--    writeFile "algorithms/gradient_descent/problem.c" $ intercalate "\n" codes
--    print "hello world"
main = do
    let exp = rotate 3 (y1 + rotate 2 x1)
    let exp1 =
            Expression
                { exIndex = 27619208921
                , exMap =
                      IM.fromList
                          [ (76957020, ([10], Var "c1"))
                          , (79244168, ([10], Var "g1"))
                          , (82103103, ([10], Var "l1"))
                          , (90108121, ([10], Var "z1"))
                          , (2447702712, ([], Const 37.730079832957344))
                          , (2715335431, ([10], Const 23.700003546553837))
                          , ( 3267288190
                            , ( [10]
                              , Sum R
                                    [ 82103103
                                    , 2715335431
                                    , 3838488649
                                    , 29470537954
                                    ]))
                          , (3838488649, ([10], Sum R [76957020, 79244168]))
                          , ( 27347813370
                            , ([], InnerProd R 3267288190 29700611949))
                          , ( 27619208921
                            , ([], InnerProd R 27347813370 2447702712))
                          , (29470537954, ([10], Rotate [7] 82103103))
                          , (29700611949, ([10], Rotate [0] 90108121))
                          ]
                } :: Expression Zero R
    let exp2 =
            ((sum1 [(l1), (rotate 7 (l1))]) <.> (rotate 0 (z1))) <.>
            (const (37.730079832957344))
    let valMaps =
            emptyVms |>
            withVm1
                (fromList
                     [ ("y1", listArray (0, 9) [2 ..])
                     , ("x1", listArray (0, 9) [20 ..])
                     ])
    showExpDebug exp2
    showExp $ collectDifferentials . exteriorDerivative allVars $ exp2
