{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module ToF where

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

tof2DTimeVelocityConstraint ::
       (Int, Int)
    -> (Expression Zero R, Expression Two R, Expression Two R, Expression Two R)
tof2DTimeVelocityConstraint size
        -- velocity n-dim array of vectors of the same dimension
        -- 2d means it is a physics experiment
        -- 3d is real flow
        -- we don't have vector variables, so we need multiple variables
 =
    let vx = var2d size "vx"
        vy = var2d size "vy"
        t = var2d size "t"
        -- up/down neighbours
        tup = rotate (0, -1) t
        vyup = rotate (0, -1) vy
        vxup = rotate (0, -1) vx
        vyuphalf = const 0.5 *. (vy + vyup)
        vxuphalf = const 0.5 *. (vx + vxup)
        vMatchesTud =
            (tup - t) * (vxuphalf * vxuphalf + vyuphalf * vyuphalf) - vxuphalf
        -- left/right neighbours
        tright = rotate (-1, 0) t
        vxright = rotate (-1, 0) vx
        vyright = rotate (-1, 0) vy
        vxrighthalf = const 0.5 *. (vx + vxright)
        vyrighthalf = const 0.5 *. (vy + vyright)
        vMatchesTrl =
            (tright - t) *
            (vxrighthalf * vxrighthalf + vyrighthalf * vyrighthalf) -
            vyrighthalf
     in (vMatchesTud <.> vMatchesTud + vMatchesTrl <.> vMatchesTrl, vx, vy, t)

tof2DUp1 :: (Int, Int) -> [String]
tof2DUp1 size@(sx, sy) =
    let mask = var2d size "mask"
        (vMatchesT, vx, vy, t) = tof2DTimeVelocityConstraint size
        vars = Set.fromList ["t"]
        valMaps =
            emptyVms |>
            withVm2
                (fromList
                     [ ("vx", listArray ((0, 0), (sx - 1, sy - 1)) $ repeat 0)
                     , ("vy", listArray ((0, 0), (sx - 1, sy - 1)) $ repeat 1)
                     , ( "mask"
                       , listArray ((0, 0), (sx - 1, sy - 1)) $
                         (replicate sy 1) ++ repeat 0)
                     ])
        tZeroOnBottom = mask <.> (t * t)
        problem = constructProblem (vMatchesT + tZeroOnBottom) vars
     in generateProblemCode valMaps problem
