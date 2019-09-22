{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module SmilingFace.SmilingFace where

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
directory = "app/SmilingFace/data/"

smilingFaceProblem :: IO ()
smilingFaceProblem = do
    maskValue <- read2DValues (directory ++ "mask.txt")
    reValue <- read2DValues (directory ++ "re.txt")
    imValue <- read2DValues (directory ++ "im.txt")
    headValue <- read2DValues (directory ++ "head.txt")
    let [x, y, mask, head, im, re] =
            map (variable2D @32 @32) ["x", "y", "mask", "head", "im", "re"]
        one = constant2D @32 @32 1
        zero = constant2D @32 @32 0
        p = x +: y
    let objectiveFunction =
            norm2square ((mask +: zero) * (ft p - (re +: im))) +
            norm2square (p - rotate (0, 1) p) +
            norm2square (p - rotate (1, 0) p) +
            norm2square (((one - head) +: zero) * p)
    let valMap =
            fromList
                [ ("mask", V2D maskValue)
                , ("head", V2D headValue)
                , ("im", V2D imValue)
                , ("re", V2D reValue)
                ]
        vars = Set.fromList ["x", "y"]
    let problem = constructProblem objectiveFunction vars
        codes = generateProblemCode valMap problem

    writeFile "algorithms/lbfgs/problem.c" $ intercalate "\n" codes
    return ()
