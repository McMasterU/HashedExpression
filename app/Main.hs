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
import Test.QuickCheck hiding (scale)

sum1 :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
sum1 = fromJust . HashedOperation.sum

prod1 :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
prod1 = fromJust . HashedOperation.sum

main
--    let exp = norm2 (x2 - y2) + norm1 (x2 - y2) -- + (x2 <.> log x2)
--    let vars = Set.fromList ["x2"]
--    showExp $ collectDifferentials . exteriorDerivative vars $ exp
--    let valMaps =
--            emptyVms |>
--            withVm2 (fromList [("y2", listArray ((0, 0), (9, 9)) [1 .. 100])])
--    let problem = constructProblem exp vars
 = do
    let (problem, valMaps) = tof2DUp (10, 12)
    let code = generateProblemCode valMaps problem
--    print valMaps
    getMinimumGradientDescent code
--    let a = listArray ((0, 0), (1, 2)) [1, 2, 3, 4, 5, 6]
--    print $ a ! (0, 0)
--    print $ a ! (0, 1)
--    print $ a ! (0, 2)
