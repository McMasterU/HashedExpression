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

import Numeric.LinearProgramming

import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (isJust)

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

-- | Hashed Expression Solver Using GLPK Bindings
-- Example usage:
--   TODO
glpkSolve :: OptimizationProblem -> IO ()
glpkSolve (OptimizationProblem objective constraints values) =
  let
    -- Construct a HashedExpression Problem from an OptimizationProblem, this creates
    -- a unified expressionMap for the objective,gradient and constraints
    prob@(Problem variables objectiveId expressionMap boxConstraints generalConstraints)
      = case constructProblem objective constraints of
          Left err -> error $ "error constructing problem: " ++ err
          Right prob -> prob

    varIndexes = zip variables [0..]
    varScalars = map (\var@(Variable vName nID pID)
                      -> case exprIsConstant (expressionMap,pID) of
                         Just d -> d
                         Nothing -> error $ "objective is non-linear, partial is not constant: "
                                         ++ show pID
                     )
                    variables

    glpkProb = Minimize varScalars

    -- TODO change GeneralConstaints to all be of the form expr <= c
  in error "TODO"

-- | Computes if a problem is linear by checking if the partial derivatives of
-- its objective and constraints functions are constant
problemIsLinear :: Problem -> Bool
problemIsLinear (Problem variables objectiveId expressionMap boxConstraints generalConstraints) =
  let
    -- The objective is linear if all of it's partial derivatives are constant
    objIsLinear = and
       $ map (\n -> isJust $ exprIsConstant $ simplify (expressionMap,n)) partials
    partials = map partialDerivativeId variables

    -- The general constraints also all need to be linear, check their partials are constant too
    constraintsAreLinear = and
      $ map (\n -> isJust $ exprIsConstant $ simplify (expressionMap,n)) constraintPartials
    constraintPartials = concatMap constraintPartialDerivatives generalConstraints

  in objIsLinear && constraintsAreLinear

-- | Checks if an expression is a @Constant@, and if it is returns the
-- corresponding @Double@, it's recommended to call @simplify@ on the expression
-- beforehand
exprIsConstant :: IsExpression e => e -> Maybe Double
exprIsConstant expr =
  let
    (exprMap,nID) = asRawExpr expr
  in case IntMap.lookup (unNodeID nID) exprMap of
       Just ([],R,Const d) -> Just d
       _ -> Nothing
