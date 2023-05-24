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

import Numeric.LinearProgramming


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
    objective = 8*x1 + 1*x2
    constraints = [ 2*x1 + x2 .<= 60
                  , x1 + x2 .>= 10
                  ]
    initialVals = [] -- GLPK doesn't take initial values, so this can be safely ignored
  in OptimizationProblem
     { objective = objective
     , constraints = constraints
     , values = initialVals
     }
