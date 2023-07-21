{-|
Module      : test.TestGLPKBindings
Copyright   :  (c) Curtis D'Alves 2023
License     :  GPL (see the LICENSE file)
Maintainer  :  curtis.dalves@gmail.com
Stability   :  experimental
Portability :  portable

Description :
Tests for using GLPK solver with Hashed Expression, see documentation for
haskell bindings
 - https://hackage.haskell.org/package/hmatrix-glpk-0.19.0.0/docs/Numeric-LinearProgramming.html
-}
module TestGLPKBindings where

import HashedExpression
import HashedExpression.Modeling.Typed
import HashedExpression.Prettify
import HashedExpression.Differentiation.Reverse (partialDerivativesMap)
import HashedExpression.Solvers.GLPK

import Numeric.LinearProgramming

import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (isJust)
import qualified Data.Array as Array
import Prelude hiding ((^))

-- | Example GLPK Problem (with Hashed Expression integration)
--
-- minimize 8*x1 + 1*x2
--   with linear constraints
--     2*x1 + 1*x2 <= 60
--     1*x1 + 1*x2 >= 10
--   and bounds
--     x1 >= 0, y2 >= 1
exGLPKProb :: Optimization
exGLPKProb = Minimize [8,1]

exGLPKConstraints :: Constraints
exGLPKConstraints = Dense [ [2,1] :<=: 60
                          , [1,1] :>=: 10
                          ]
exGLPKBounds :: [Numeric.LinearProgramming.Bound Int]
exGLPKBounds = [ 2 :>=: 1
               , 1 :>=: 1
         -- NOTE by default variables have bounds >= 0, so 1 :>=: 0 is not necessary
         --      if you want to specify a variable has no bounds, use
         -- ,Free 1
         ]

solveGLPKProb :: Solution
solveGLPKProb = simplex exGLPKProb exGLPKConstraints exGLPKBounds

-- | Example Hashed Expression Problem
-- The same optimization problem above modeled in HashedExpression
exProblem :: OptimizationProblem
exProblem =
  let
    x1 = variable "x1"
    x2 = variable "x2"
    lowerBound = bound "lowerBound"

    objective = 8*x1 + 1*x2
    constraints = [ 2*x1 + x2 .<= 60
                  , x1 + x2 .>= 10
                  , x2 .>= lowerBound
                  , x1 .>= lowerBound
                  ]
    initialVals = [ lowerBound :-> VScalar 1.0] -- GLPK doesn't take initial values, so this can be safely ignored
  in OptimizationProblem
     { objective = objective
     , constraints = constraints
     , values = initialVals
     }

-- unbounded example 
exProblem1 :: OptimizationProblem
exProblem1 =
  let
    x0 = variable "x0"
    x1 = variable "x1"
    objective = 8*x0 + 1*x1
    initialVals = [x0 :-> VScalar 5.0
                  ,x1 :-> VScalar (-5.0)]
  in OptimizationProblem
     { objective = objective
     , constraints = []
     , values = initialVals
     }

testExProblem1 = glpkSolve exProblem1

-- 4D constant constraints 
exProblem2 :: OptimizationProblem
exProblem2 = 
  let
    x0 = variable "x0"
    x1 = variable "x1"
    x2 = variable "x2"
    objective = 18*x0 + 4*x1 + 6*x2
    initialVals = [x0 :-> VScalar 1, x0 :-> VScalar 3, x0 :-> VScalar 2]
  in OptimizationProblem
     { objective = objective
     , constraints = [    x0 .>= (-1.0), x0 .<= 5.0
                        , x1 .<= 6.0, x1 .>= (-2.0)
                        , x2 .>= 1.0, x2 .<= 10.0 ]
     , values = initialVals
     }

testExProblem2 = glpkSolve exProblem2

-- 4D linear constraints 
exProblem3 :: OptimizationProblem
exProblem3 = 
  let
    x0 = variable "x0"
    x1 = variable "x1"
    x2 = variable "x2"
    objective = 18*x0 + 4*x1 + 6*x2
    initialVals = [x0 :-> VScalar 1, x0 :-> VScalar 3, x0 :-> VScalar 2]
  in OptimizationProblem
     { objective = objective
     , constraints = [    x0 .>= (-1.0), x0 .<= 5.0
                        , x1 .<= 6.0, x1 .>= (-2.0)
                        , x2 .>= 1.0, x2 .<= 10.0
                        , x0+x1+x2 .>= 3 ]
     , values = initialVals
     }

testExProblem3 = glpkSolve exProblem3


-- 4D constant constraints with bounds 
exProblem4 :: OptimizationProblem
exProblem4 = 
  let
    x0 = variable "x0"
    x1 = variable "x1"
    x2 = variable "x2"

    lowerx0Bound = bound "lowerx0Bound"
    lowerx1Bound = bound "lowerx1Bound"
    lowerx2Bound = bound "lowerx2Bound"

    objective = 18*x0 + 4*x1 + 6*x2
    initialVals = [x0 :-> VScalar 1, x0 :-> VScalar 3, x0 :-> VScalar 2, lowerx0Bound :-> VScalar (-1.0), lowerx1Bound :-> VScalar (-2.0), lowerx2Bound :-> VScalar 1.0]
  in OptimizationProblem
     { objective = objective
     , constraints = [    x0 .>= lowerx0Bound, x0 .<= 5.0
                        , x1 .<= 6.0, x1 .>= lowerx1Bound
                        , x2 .>= lowerx2Bound, x2 .<= 10.0]
     , values = initialVals
     }

testExProblem4 = glpkSolve exProblem4


-- 4D linear constraints with bounds 
exProblem5 :: OptimizationProblem
exProblem5 = 
  let
    x0 = variable "x0"
    x1 = variable "x1"
    x2 = variable "x2"

    lowerx0Bound = bound "lowerx0Bound"
    lowerx1Bound = bound "lowerx1Bound"
    lowerx2Bound = bound "lowerx2Bound"

    objective = 18*x0 + 4*x1 + 6*x2
    initialVals = [x0 :-> VScalar 1, x0 :-> VScalar 3, x0 :-> VScalar 2, lowerx0Bound :-> VScalar (-1.0), lowerx1Bound :-> VScalar (-2.0), lowerx2Bound :-> VScalar 1.0]
  in OptimizationProblem
     { objective = objective
     , constraints = [    x0 .>= lowerx0Bound, x0 .<= 5.0
                        , x1 .<= 6.0, x1 .>= lowerx1Bound
                        , x2 .>= lowerx2Bound, x2 .<= 10.0
                        , x0+x1+x2 .>= 3 ]
     , values = initialVals
     }

testExProblem5 = glpkSolve exProblem5


-- Also test something with many more variables
-- Equality constraints .=