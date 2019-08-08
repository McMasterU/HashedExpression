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
import ToF.VelocityGenerator

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
tof2DTimeVelocityConstraint size@(row, column) =
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
            ((t - tUp) * (vxUpBorder * vxUpBorder + vyUpBorder * vyUpBorder) -
             vyUpBorder) *
            vyUpBorder
        -- match right
        matchRight =
            ((t - tRight) *
             (vxRightBorder * vxRightBorder + vyRightBorder * vyRightBorder) -
             vxRightBorder) *
            vxRightBorder
        -- match objective
        matchObjective =
            tUpMask <.> (matchUp * matchUp) +
            tRightMask <.> (matchRight * matchRight)
                    -- + regulizer
        -- necessary values
        valMaps =
            emptyVms
                { vm2 =
                      fromList
                          [ ( tUpMaskName
                            , listArray ((0, 0), (row - 1, column - 1)) $
                              replicate ((row - 1) * column) 1 ++
                              replicate column 0)
                          , ( tRightMaskName
                            , listArray ((0, 0), (row - 1, column - 1)) $
                              concat $
                              replicate row (0 : replicate (column - 1) 1))
                          ]
                }
     in (matchObjective, valMaps)

--        regulizer = ((const2d size 1 / (const2d size 1 + const2d size 100 * (vx * vx + vy * vy)))
--                    * ((t - const2d size 10) * (t - const2d size 10))) <.> const2d size 1
tof2DUp :: (Int, Int) -> (Problem, ValMaps)
tof2DUp size@(row, column) =
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
                            , listArray ((0, 0), (row - 1, column - 1)) $
                              repeat 0)
                          , ( vyName
                            , listArray ((0, 0), (row - 1, column - 1)) $
                              repeat 1)
                          , ( maskName
                            , listArray ((0, 0), (row - 1, column - 1)) $
                              replicate ((row - 1) * column) 0 ++
                              replicate column 1)
                          ]
                }
        finalValMaps = mergeValMaps valMaps predefinedValMaps
        problem = constructProblem objectiveFn vars
     in (problem, finalValMaps)

-- |
--
tof2DStraight :: (Problem, ValMaps)
tof2DStraight =
    let size = (10, 10)
        row = 10
        column = 10
        vx = var2d size vxName
        vy = var2d size vyName
        t = var2d size tName
        mask = var2d size maskName
        (vxVal, vyVal) = straightFlow (10, 10) 3 4 0.2
        (matchObjective, predefinedValMaps) = tof2DTimeVelocityConstraint size
        vars = Set.fromList [tName]
        tZeroOnBottom = mask <.> (t * t)
        objectiveFn = matchObjective + tZeroOnBottom
        valMaps =
            emptyVms
                { vm2 =
                      fromList
                          [ ( vxName, vxVal)
                          , ( vyName, vyVal)
                          , ( maskName
                            , listArray ((0, 0), (row - 1, column - 1)) $
                              replicate ((row - 1) * column) 0 ++
                              replicate column 1)
                          ]
                }
        finalValMaps = mergeValMaps valMaps predefinedValMaps
        problem = constructProblem objectiveFn vars
     in (problem, finalValMaps)
