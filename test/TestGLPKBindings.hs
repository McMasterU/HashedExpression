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
import qualified Data.Array as Array

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
glpkSolve :: OptimizationProblem -> IO (Double,[((VarName,Int),Double)])
glpkSolve (OptimizationProblem objective constraints values) =
  let
    -- Construct a HashedExpression Problem from an OptimizationProblem, this creates
    -- a unified expressionMap for the objective,gradient and constraints
    prob@(Problem variables objectiveId expressionMap boxConstraints generalConstraints)
      = case constructProblem objective constraints of
          Left err -> error $ "error constructing problem: " ++ err
          Right prob -> prob

    -- Create a @ValMap@ from values, GLPK doesn't take initial values so these
    -- will be ignored, but the map is still needed to lookup Bound values for
    -- BoxConstraint
    valMap = mkValMap values

    -- Each variable needs to be given an index for the GLPK problem. When
    -- constructing the objective and constraints we can safely use the order in
    -- which varialbes and partialIDs are already in their lists
    -- NOTE vector variables are unpacked/flattened
    varNameToIndex = zip [0..] remappedVars
    remappedVars = concatMap varNameReMap variables
    varNameReMap (Variable vName nID pID) =
      case IntMap.lookup (unNodeID nID) expressionMap of
        Just (shape,_,_) -> [ (vName,idx) | idx <- [0..sizeOf shape-1] ]
        Nothing -> error $ "Variable node missing from expressionMap: " ++ show nID

    varScalars = concatMap (\var@(Variable vName nID pID)
                      -> case exprIsConstant (expressionMap,pID) of
                          Just d -> d
                          Nothing -> error $ "objective is non-linear, partial is not constant: "
                                          ++ show pID
                     )
                    variables
    -- Problem
    glpkProblem = Minimize varScalars

    -- General Constraints
    glpkConstraints = Dense $ concatMap (\(GeneralConstraint gID pIDs lb ub)
                                         -> let
                                            -- NOTE pIDs should be in the same order as variables
                                            partials = concatMap (partialToConstant gID) pIDs
                                          in [partials :<=: ub
                                             ,partials :>=: lb]) generalConstraints
    partialToConstant gID pID = case exprIsConstant (expressionMap,pID) of
                                  Just d -> d
                                  Nothing -> error $ "general constraint is non-linear: "
                                             ++ show gID

    -- Variable Bounds
    glpkBounds = map boxesToBound boxesByIndex

    boxesByIndex = map (\(vIdx,(vName,idx')) -> ((vIdx,idx'), filter (\b -> boxVarName b == vName) boxConstraints))
                   varNameToIndex
    boxVarName (BoxUpper vName _) = vName
    boxVarName (BoxLower vName _) = vName

    boxesToBound ((varIndex,idx'),bConstraints) = let

      lookupBVal bID = case Map.lookup bID valMap of
                         Just (VScalar d) -> d
                         Just (VNum d) -> d
                         Just (V1D arr) -> arr Array.! idx'
                         -- FIXME V2D and V3D indxing innefficent
                         -- , check how array indices are packed then index them directly
                         Just (V2D arr) -> (Array.elems arr) Prelude.!! idx'
                         Just (V3D arr) -> (Array.elems arr) Prelude.!! idx'
                         Just _ -> error $ "Bound value is non-scalar: " ++ bID
                         Nothing -> error $ "Bound ID missing from valMap: " ++ bID
      in case bConstraints of
           [] -> Free varIndex
           [BoxLower _ bID] -> varIndex :>=: lookupBVal bID
           [BoxUpper _ bID] -> varIndex :<=: lookupBVal bID
           [BoxLower _ bID0,BoxUpper _ bID1] -> varIndex :&: (lookupBVal bID0,lookupBVal bID1)
           [BoxUpper _ bID1,BoxLower _ bID0] -> varIndex :&: (lookupBVal bID0,lookupBVal bID1)
           bs -> error $ "duplicate combinations of box constraints for variable: " ++ show varIndex

    -- Run Simplex method to solve
    -- NOTE have a choice between simplex and exact methods? (see glp_exact documentation)
    solution = simplex glpkProblem glpkConstraints glpkBounds
  in case solution of
       Optimal (sol,vars) -> return $ (sol,zip remappedVars vars)
       Undefined -> error "GLPK returned Undefined"
       -- TODO should this be an error? check glpk documentation on feasible
       Feasible (sol,vars) -> error $ "GLPK returned Feasible"
                              ++ show (sol,zip remappedVars vars)
       Infeasible (sol,vars) -> error $ "GLPK returned Infeasible"
                                ++ show (sol,zip remappedVars vars)
       NoFeasible -> error "GLPK returned NoFeasible"
       Unbounded -> error "GLPK returned Unbounded"

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
exprIsConstant :: IsExpression e => e -> Maybe [Double]
exprIsConstant expr =
  let
    (exprMap,nID) = asRawExpr expr
  in case IntMap.lookup (unNodeID nID) exprMap of
       Just (shape,R,Const d) -> Just (replicate (sizeOf shape) d)
       _ -> Nothing
