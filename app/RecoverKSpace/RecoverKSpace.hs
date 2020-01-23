{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RecoverKSpace.RecoverKSpace where

import Data.Array
import Data.Complex
import qualified Data.IntMap.Strict as IM
import Data.Map (empty, fromList, union)
import qualified Data.Set as Set
import HashedExpression.Derivative
import HashedExpression.Expression

import HashedExpression.Interp
import HashedExpression.Normalize
import HashedExpression.Operation hiding (product, sum)
import qualified HashedExpression.Operation
import HashedExpression.Prettify
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
import HashedExpression.CollectDifferential
import HashedExpression.Plot
import HashedExpression.Solver
import HashedExpression.ToC (singleExpressionCProgram)
import HashedExpression.Utils
import HashedExpression.Var
import Test.Hspec
import ToF.VelocityGenerator

directory :: FilePath
directory = "app/RecoverKSpace/data/"

smilingFaceProblem :: IO ()
smilingFaceProblem = do
    let [x, mask, im, re] =
            map (variable2D @128 @128) ["x", "mask", "im", "re"]
        one = constant2D @128 @128 1
        zero = constant2D @128 @128 0
    let vars = ["x"]
    let objectiveFunction =
            norm2square ((mask +: zero) * (ft x - (re +: im))) +
            const 3000 *
            (norm2square
                 (rotate (0, 1) x + rotate (0, -1) x - const 2 *. x) +
             norm2square
                 (rotate (1, 0) x + rotate (-1, 0) x - const 2 *. x))
    let xLowerBound = VFile $ HDF5 "x_lb.h5" "x_lb"
        xUpperBound = VFile $ HDF5 "x_ub.h5" "x_ub"
    let constraint =
            Constraint
                [ x .>= xLowerBound
                , x .<= xUpperBound
                ]
    let (ProblemValid problem) =
            constructProblem objectiveFunction vars constraint
    let valMap =
            fromList
                [ ("mask", VFile $ HDF5 "mask.h5" "mask")
                , ("re", VFile $ HDF5 "re.h5" "re")
                , ("im", VFile $ HDF5 "im.h5" "im")
                , ("x", VNum 0)
                ]
    case generateProblemCode valMap problem of
        Invalid str -> putStrLn str
        Success proceed -> proceed "algorithms/lbfgs-b"
