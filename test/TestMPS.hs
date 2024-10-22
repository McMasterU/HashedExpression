{-|
Module      : test.TestMPS
Copyright   :  (c) Curtis D'Alves 2024
License     :  BSD (see the LICENSE file)
Maintainer  :  curtis.dalves@gmail.com
Stability   :  experimental
Portability :  portable

Description:
  Tests MPS file generation
-}

module TestMPS where

import HashedExpression
import HashedExpression.Modeling.Typed
import HashedExpression.Prettify
import HashedExpression.Codegen.MPS

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Process


-- | Example Optimization problem
--      obj = 40x0 + 30x1
--      subject to
--              x0 + x1 <= 12
--              2*x0 + x1 <= 16
--              x1,x2 >= 0
-- Solution:
--   x0 = 4 and x1 = 8, obj = -400
exProblem :: OptimizationProblem
exProblem =
  let
    x0 = variable "x0"
    x1 = variable "x1"
    bZero = bound "zeroBound"

    objective = -40*x0 - 30*x1
    initialVals = [x0 :-> VScalar 0.0
                  ,x1 :-> VScalar 0.0
                  ,bZero :-> VScalar 0.0]
  in OptimizationProblem
     { objective = objective
     , constraints = [x0 + x1 .<= 12
                     ,2*x0 + x1 .<= 16
                     ,x0 .>= bZero
                     ,x1 .>= bZero
                     ]
     , values = initialVals
     }

generateMPS :: IO ()
generateMPS = let
  mpsFile = Text.unlines $ toMPS exProblem
  filePath = "./mps/prob.mps"
  in do TextIO.writeFile filePath mpsFile
        -- readProcess "highs" [filePath] ""
        putStrLn "TODO"
