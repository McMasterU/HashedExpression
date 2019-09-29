{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Fruit.Fruit where

import Data.Array
import Data.Complex
import qualified Data.IntMap.Strict as IM
import Data.Map (empty, fromList, union)
import qualified Data.Set as Set
import HashedDerivative
import HashedExpression
import HashedInterp
import HashedNormalize
import HashedOperation
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
import Graphics.EasyPlot
import HashedCollect
import HashedPlot
import HashedSolver
import HashedToC (singleExpressionCProgram)
import HashedUtils
import HashedVar
import Test.Hspec
import ToF.VelocityGenerator

directory :: FilePath
directory = "app/Fruit/data/"

hardOne :: IO ()
hardOne = do
    kspaceMaskValue <- read2DValues (directory ++ "kspaceMask.dat")
    mReValues <-
        sequence
            [ read2DValues (directory ++ "mRe" ++ show i ++ ".dat")
            | i <- [0 .. numCoils]
            ]
    mImValues <-
        sequence
            [ read2DValues (directory ++ "mIm" ++ show i ++ ".dat")
            | i <- [0 .. numCoils]
            ]
    --mReValues <- sequence $ [ read2DValues (directory ++ "sRe"++show i++".dat") | i <- [0..numCoils]]
    --mImValues <- sequence $ [ read2DValues (directory ++ "sIm"++show i++".dat") | i <- [0..numCoils]]
    imageMaskValue <- read2DValues (directory ++ "imageMask.dat")
    let [x, y, kMask, imageMask] =
            map (variable2D @256 @256) ["x", "y", "kmask", "imageMask"]
        mIm i = variable2D @256 @256 $ "mIm" ++ show i
        mRe i = variable2D @256 @256 $ "mRe" ++ show i
        sIm i = variable2D @256 @256 $ "sIm" ++ show i
        sRe i = variable2D @256 @256 $ "sRe" ++ show i
        one = constant2D @256 @256 1
        zero = constant2D @256 @256 0
    let objectiveFunction =
            sum
                [ norm2square
                    ((kMask +: zero) *
                     (ft ((sRe i +: sIm i) * (x +: y)) - (mRe i +: mIm i)))
                | i <- [0 .. numCoils]
                ] +
            huberNorm 2 (x - rotate (0, 1) x) +
            huberNorm 2 (x - rotate (1, 0) x) +
            huberNorm 2 (y - rotate (0, 1) y) +
            huberNorm 2 (y - rotate (1, 0) y) +
            const 10000 * norm2square ((one - imageMask) * (x * x + y * y)) +
            sum
                [ sumElements $
                (sRe i - rotate (0, 1) (sRe i)) ^ 2 +
                (sIm i - rotate (0, 1) (sIm i)) ^ 2 +
                (sRe i - rotate (1, 0) (sRe i)) ^ 2 +
                (sIm i - rotate (1, 0) (sIm i)) ^ 2
                | i <- [0 .. numCoils]
                ]
    let valMap =
            fromList $
            [("kmask", V2D kspaceMaskValue), ("imageMask", V2D imageMaskValue)] ++
            zipWith
                (\i vals -> ("mRe" ++ show i, V2D vals))
                [0 .. numCoils]
                mReValues ++
            zipWith
                (\i vals -> ("mIm" ++ show i, V2D vals))
                [0 .. numCoils]
                mImValues
        vars = ["x"]
    let problem = constructProblem objectiveFunction vars NoConstraint
        codes = generateProblemCode valMap problem
    undefined
    return ()

numCoils :: Int
numCoils = 2

anotherFruiteProblem :: IO ()
anotherFruiteProblem = do
    mReValues <-
        sequence
            [ read2DValues (directory ++ "reCoil" ++ show i ++ ".dat")
            | i <- [0 .. numCoils - 1]
            ]
    mImValues <-
        sequence
            [ read2DValues (directory ++ "imCoil" ++ show i ++ ".dat")
            | i <- [0 .. numCoils - 1]
            ]
    putStrLn "Done reading data"
    let [x, y] = map (variable2D @256 @256) ["x", "y"]
        mIm i = variable2D @256 @256 $ "imCoil" ++ show i
        mRe i = variable2D @256 @256 $ "reCoil" ++ show i
        sIm i = variable2D @256 @256 $ "sIm" ++ show i
        sRe i = variable2D @256 @256 $ "sRe" ++ show i
        one = constant2D @256 @256 1
        zero = constant2D @256 @256 0
    let objectiveFunction =
            sum
                [ norm2square
                    (ft ((sRe i +: sIm i) * (x +: y)) - (mRe i +: mIm i))
                | i <- [0 .. numCoils - 1]
                ] +
            huberNorm 2 (x - rotate (0, 1) x) +
            huberNorm 2 (x - rotate (1, 0) x) +
            huberNorm 2 (y - rotate (0, 1) y) +
            huberNorm 2 (y - rotate (1, 0) y) +
            sumElements (x * x + y * y) +
            sum
                [ sumElements $
                (sRe i - rotate (0, 1) (sRe i)) ^ 2 +
                (sIm i - rotate (0, 1) (sIm i)) ^ 2 +
                (sRe i - rotate (1, 0) (sRe i)) ^ 2 +
                (sIm i - rotate (1, 0) (sIm i)) ^ 2
                | i <- [0 .. numCoils - 1]
                ]
    let valMap =
            fromList $
            zipWith
                (\i vals -> ("reCoil" ++ show i, V2D vals))
                [0 .. numCoils - 1]
                mReValues ++
            zipWith
                (\i vals -> ("imCoil" ++ show i, V2D vals))
                [0 .. numCoils - 1]
                mImValues ++
            map
                (\x ->
                     (\i vals -> ("sIm" ++ show i, V2D vals))
                         x
                         (listArray ((0, 0), (255, 255)) $ repeat 0))
                [0 .. numCoils - 1] ++
            map
                (\x ->
                     (\i vals -> ("sRe" ++ show i, V2D vals))
                         x
                         (listArray ((0, 0), (255, 255)) $ repeat 0))
                [0 .. numCoils - 1] ++
            [ ("x", V2D $ listArray ((0, 0), (255, 255)) $ repeat 0)
            , ("y", V2D $ listArray ((0, 0), (255, 255)) $ repeat 0)
            ]
        vars =
            ["x", "y"] ++
            ["sIm" ++ show i | i <- [0 .. numCoils - 1]] ++
            ["sRe" ++ show i | i <- [0 .. numCoils - 1]]
    let problem = constructProblem objectiveFunction vars NoConstraint
    case generateProblemCode valMap problem of
        Invalid str -> putStrLn str
        Success proceed -> proceed "algorithms/lbfgs"

easyFruitProblem :: IO ()
easyFruitProblem = do
    let [x, y, fruits, im, re] =
            map (variable2D @256 @256) ["x", "y", "fruits", "im", "re"]
        one = constant2D @256 @256 1
        zero = constant2D @256 @256 0
    let objectiveFunction =
            norm2square (ft (x +: y) - (re +: im)) +
            const 12000000 *
            (huberNorm 1000 (x - rotate (0, 1) x) +
             huberNorm 1000 (x - rotate (1, 0) x) +
             huberNorm 1000 (y - rotate (0, 1) y) +
             huberNorm 1000 (y - rotate (1, 0) y)) +
            const 1000000 * norm2square ((one - fruits) * x) +
            const 1000000 * norm2square ((one - fruits) * y)
    let valMap =
            fromList
                [ ("fruits", V2DFile HDF5 "fruits.h5")
                , ("re", V2DFile HDF5 "re.h5")
                , ("im", V2DFile HDF5 "im.h5")
                , ("x", V2D $ listArray ((0, 0), (255, 255)) $ repeat 0)
                , ("y", V2D $ listArray ((0, 0), (255, 255)) $ repeat 0)
                ]
        vars = ["x", "y"]
    let problem = constructProblem objectiveFunction vars NoConstraint
    print problem
    case generateProblemCode valMap problem of
        Invalid str -> putStrLn str
        Success proceed -> proceed "algorithms/lbfgs"

anotherFruit :: IO ()
anotherFruit = do
    let [s, img, c, median] =
            map (variable2D @256 @256) ["s", "img", "c", "median"]
        one = constant2D @256 @256 1
        zero = constant2D @256 @256 0
    let objectiveFunction =
            norm2square ((img - s * c) * (const 0.00000001 *. median)) +
            (norm2square (s - rotate (0, 1) s) +
             norm2square (s - rotate (1, 0) s))
    let valMap =
            fromList
                [ ("img", V2DFile HDF5 "img.h5")
                , ("c", V2DFile HDF5 "c.h5")
                , ("median", V2DFile HDF5 "median.h5")
                , ("s", V2D $ listArray ((0, 0), (255, 255)) $ repeat 0)
                ]
        vars = ["s"]
    let constraint = NoConstraint
    let problem = constructProblem objectiveFunction vars constraint
    print problem
    case generateProblemCode valMap problem of
        Invalid str -> putStrLn str
        Success proceed -> proceed "algorithms/lbfgs-b"
