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
import HashedCollect
import HashedSolver
import HashedToC (Code, singleExpressionCProgram)
import HashedUtils

vxName :: String
vxName = "vx"

vyName :: String
vyName = "vy"

maskName :: String
maskName = "mask"

tUpMaskName = "tUpMaskName"

tRightMaskName = "tRightMaskName"

tName :: String
tName = "t"

tof2DTimeVelocityConstraint :: (Int, Int) -> (Expression Zero R, ValMaps)
tof2DTimeVelocityConstraint size@(size1, size2) =
    let up, right :: Expression Two R -> Expression Two R
        up = rotate (-1, 0)
        right = rotate (0, 1)
        --
        vx = var2d size vxName
        vy = var2d size vyName
        t = var2d size tName
        -- t
        tUpMask = var2d size tUpMaskName
        tRightMask = var2d size tRightMaskName
        tUp = up t
        tRight = right t
        -- vx
        vxUp = up vx
        vxUpBorder = const 0.5 *. (vx + vxUp)
        vxRight = right vx
        vxRightBorder = const 0.5 *. (vx + vxRight)
        -- vy
        vyUp = up vy
        vyUpBorder = const 0.5 *. (vy + vyUp)
        vyRight = right vy
        vyRightBorder = const 0.5 *. (vy + vyRight)
        -- match up
        matchUp =
            (tUp - t) * (vxUpBorder * vxUpBorder + vyUpBorder * vyUpBorder) -
            vyUpBorder
        -- match right
        matchRight =
            (tRight - t) *
            (vxRightBorder * vxRightBorder + vyRightBorder * vyRightBorder) -
            vxRightBorder
        -- match objective 
        matchObjective =
            tUpMask <.> (matchUp * matchUp) +
            tRightMask <.> (matchRight * matchRight)
        -- necessary values
        valMaps =
            emptyVms
                { vm2 =
                      fromList
                          [ ( tUpMaskName
                            , listArray ((0, 0), (size1 - 1, size2 - 1)) $
                              replicate (size1 * size2 - size2) 1 ++
                              replicate size2 0)
                          , ( tRightMaskName
                            , listArray ((0, 0), (size1 - 1, size2 - 1)) $
                              concat $
                              replicate size1 (0 : replicate (size2 - 1) 1))
                          ]
                }
     in (matchObjective, valMaps)

tof2DUp :: (Int, Int) -> (Problem, ValMaps)
tof2DUp size@(size1, size2) =
    let vx = var2d size vxName
        vy = var2d size vyName
        t = var2d size tName
        mask = var2d size maskName
        (matchObjective, predefinedValMaps) = tof2DTimeVelocityConstraint size
        vars = Set.fromList [tName]
        tZeroOnBottom = mask <.> (t * t)
        objectiveFn = matchObjective + tZeroOnBottom
        valMaps =
            emptyVms
                { vm2 =
                      fromList
                          [ ( vxName
                            , listArray ((0, 0), (size1 - 1, size2 - 1)) $
                              repeat 0)
                          , ( vyName
                            , listArray ((0, 0), (size1 - 1, size2 - 1)) $
                              repeat (-1))
                          , ( maskName
                            , listArray ((0, 0), (size1 - 1, size2 - 1)) $
                              replicate (size1 * size2 - size2) 0 ++
                              replicate size2 1)
                          ]
                }
        finalValMaps = mergeValMaps valMaps predefinedValMaps
        problem = constructProblem objectiveFn vars
     in (problem, finalValMaps)