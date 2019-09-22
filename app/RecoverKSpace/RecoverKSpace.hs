{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RecoverKSpace.RecoverKSpace where

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
directory = "app/RecoverKSpace/data/"

smilingFaceProblem :: IO ()
smilingFaceProblem = do
    maskValue <- read2DValues (directory ++ "mask.txt")
    reValue <- read2DValues (directory ++ "re.txt")
    imValue <- read2DValues (directory ++ "im.txt")
    headValue <- read2DValues (directory ++ "head.txt")
    let [x, y, mask, head, im, re] =
            map (variable2D @128 @128) ["x", "y", "mask", "head", "im", "re"]
        one = constant2D @128 @128 1
        zero = constant2D @128 @128 0
    let objectiveFunction =
            norm2square ((mask +: zero) * (ft x - (re +: im))) +
            norm2square (x - rotate (0, 1) x) +
            norm2square (x - rotate (1, 0) x) +
            norm2square ((one - head) * x)

    let valMap =
            fromList
                [ ("mask", V2D maskValue)
                , ("head", V2D headValue)
                , ("re", V2D reValue)
                , ("im", V2D imValue)
                ]
        vars = Set.fromList ["x"]
    let problem = constructProblem objectiveFunction vars
        codes = generateProblemCode valMap problem

    writeFile "algorithms/lbfgs/problem.c" $ intercalate "\n" codes
    return ()
