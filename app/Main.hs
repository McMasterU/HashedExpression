{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Array
import Data.Complex
import qualified Data.IntMap.Strict as IM
import Data.Map (empty, fromList, union)
import qualified Data.Set as Set
import HashedDerivative
import HashedExpression
import HashedInterp
import HashedNormalize
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
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
import Fruit.Fruit
import Graphics.EasyPlot
import HashedCollect
import HashedPlot
import HashedSolver
import HashedToC (singleExpressionCProgram)
import HashedUtils
import HashedVar
import RecoverKSpace.RecoverKSpace
import Test.Hspec
import ToF.VelocityGenerator

reFT :: (DimensionType d) => Expression d R -> Expression d R
reFT = xRe . ft

imFT :: (DimensionType d) => Expression d R -> Expression d R
imFT = xIm . ft

--
main = do
    let x = variable1D @10 "x"
        y = variable1D @10 "y"
        z = variable1D @10 "z"
        t = variable1D @10 "t"
        valMap =
            fromList
                [ ("x", V1D $ listArray (0, 9) [1 ..])
                , ("y", V1D $ listArray (0, 9) [2,5 ..])
                , ("z", V1D $ listArray (0, 9) [3,8 ..])
                , ("z", V1D $ listArray (0, 9) [0,-1 ..])
                ]
    let res1 = eval valMap $ reFT (reFT (x + z))
        res2 = eval valMap . normalize $ reFT (reFT (x + z))
    print $ res1 ~= res2
    let res1 = eval valMap $ imFT (imFT (x + z))
        res2 = eval valMap . normalize $ imFT (imFT (x + z))
    print $ res1 ~= res2
    let res1 = eval valMap $ reFT (reFT (x + z)) + imFT (imFT (x + z))
        res2 =
            eval valMap . normalize $ reFT (reFT (x + z)) + imFT (imFT (x + z))
    print $ res1 ~= res2--    let exp =
--            (xRe (ft (x +: y) - (z +: t)) <.> xRe (ft (x +: y) - (z +: t))) +
--            (xIm (ft (x +: y) - (z +: t)) <.> xIm (ft (x +: y) - (z +: t)))
--        vars = Set.fromList ["x", "y"]
--        problem = constructProblem exp vars
--        values =
--            fromList
--                [ ("z", V1DFile HDF5 "z.h5")
--                , ("t", V1DFile HDF5 "t.h5")
--                , ("x", V1DFile HDF5 "x.h5")
--                , ("y", V1DFile HDF5 "y.h5")
--                ]
--    print problem
--    case generateProblemCode values problem of
--        Invalid str -> putStrLn str
--        Success proceed -> proceed "algorithms/lbfgs"
--main = do
--    let x = var "x"
--    let exp = huber 1 x
--        fun = Function exp empty
--    plot1VariableFunction fun "haha"
--main = smilingFaceProblem
