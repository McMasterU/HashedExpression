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
    let exp1 = rotate 3 (y1 * rotate 2 x1) <.> x1
--    let exp2 = rotate 3 (y1) * rotate 5 (x1)
    let valMaps =
            emptyVms |>
            withVm1
                (fromList
                     [ ("y1", listArray (0, 9) [2 ..])
                     , ("x1", listArray (0, 9) [20 ..])
                     ])
    showExp $ simplify exp1
    showExp $ exteriorDerivative allVars exp1--    print $ eval valMaps exp1
--    print $ eval valMaps exp2
