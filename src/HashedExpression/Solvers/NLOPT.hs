-- |
-- Module      :  HashedExpression.Solvers.NLOPT
-- Copyright   :  (c) OCA 2023
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- This module provides bindings to solve an @OptimizationProblem@ using NLOPT,
-- via the nlopt-haskell package, see
--  - https://hackage.haskell.org/package/nlopt-haskell-0.1.3.0/docs/Numeric-Optimization-NLOPT-Bindings.html
-- Bindings very closely follow the corresponding c functions in the official NLOpt documentation
--  - https://nlopt.readthedocs.io/en/latest/

module HashedExpression.Solvers.NLOPT where

import HashedExpression
import HashedExpression.Modeling.Typed
import HashedExpression.Prettify

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Vector.Storable hiding ((++))
import Data.Vector.Storable.Mutable hiding ((++))
import qualified Data.Primitive.Array as Array
import Data.Vector.Mutable (fromMutableArray)

import qualified Numeric.Optimization.NLOPT.Bindings as NLOPT


-- TODO still need to add box constraint support?

-- | Hashed Expression Solver
-- Example usage:
--   @nloptSolve exProblem NLOPT.LD_SLSQP@
nloptSolve :: OptimizationProblem -> NLOPT.Algorithm -> IO (Double,[((VarName,Int),Double)])
nloptSolve (OptimizationProblem objective constraints values) algorithm =
  let
    -- Construct a HashedExpression Problem from an OptimizationProblem, this creates
    -- a unified expressionMap for the objective,gradient and constraints
    prob@(Problem variables objectiveId expressionMap boxConstraints generalConstraints)
      = case constructProblem objective constraints of
          Left err -> error $ "error constructing problem: " ++ err
          Right prob -> prob

    -- evaluate an expression given by its NodeID by looking up its evaluation in
    -- the expressionMap, returning an InterpValue
    evalN :: ValMap -> NodeID ->  InterpValue
    evalN valMap nID = eval valMap (expressionMap,nID)

    -- same as evalN, but explicitely extracts the Double value from InterpValue
    -- assuming the result is VR (i.e, Value Real), and throwing an error specific
    -- to the gradient function being non-scalar otherwise (should only be used to eval
    -- partial derivatives)
    evalPD :: ValMap -> NodeID ->  Double
    evalPD valMap nID = case eval valMap (expressionMap,nID) of
                          VR pd -> pd
                          interpVal -> error $ "Given grad function is non-scalar: "
                                             ++ show (nID,interpVal)

    -- TODO this assumes all variables are VScalar
    size :: Int
    size = Prelude.length variables

    -- TODO all variables are assumed to be VScalar
    -- We need a function that projects any non-scalar variables into scalars
    varsWithIdx :: [(Int,Variable)]
    varsWithIdx = zip [0..] variables

    -- given to NLOPT.min_objective_function
    -- takes in the current point xs that the function/gradient is being evaluated at
    -- returns the result of evaluting the objective function
    -- if mgrad is not Nothing, fills it in with each of the partial derivatives
    -- (mgrad is a Maybe IOVector, i.e. a Mutable Vector)
    objFunc :: NLOPT.ScalarFunction ()
    objFunc xs mgrad _ =
      let
        -- create a valMap (mapping from variable name to value) by looking up each
        -- variables assigned index in the vector xs
        valMap = Map.fromList
                 $ Prelude.map (\(idx,var)
                                -> (varName var,VScalar $ xs ! idx)) varsWithIdx
        -- evaluate the objective function, must return a scalar / real number (i.e, VR which stands
        -- for value real) or we throw an error
        obj = case evalN valMap objectiveId of
                VR obj0 -> obj0
                interpVal -> error $ "Given objective function is non-scalar: " ++ show interpVal
        -- evaluate the parital derivative of each variable corresponding to its assigned index
        -- to create a list of index and corresponding parital derivative evaluation
        grad = Prelude.map (\(idx,var)
                            -> (idx,evalPD valMap $ partialDerivativeId var)) varsWithIdx
      in do case mgrad of
              -- when Just grad0, write the values of each parital derivative computed in grad
              -- at the correct index (grad0 is a mutable IOVector)
              Just grad0 -> sequence_ $ Prelude.map (\(idx,v) -> write grad0 idx v) grad
              Nothing -> return ()
            return obj

    -- non-linear constraints
    constraintFunc :: (GeneralConstraint,Bool) -> NLOPT.ScalarFunction ()
    constraintFunc (constraint,upBound) xs mgrad _ =
      let
        -- create a valMap (mapping from variable name to value) by looking up each
        -- variables assigned index in the vector xs
        valMap = Map.fromList
                 $ Prelude.map (\(idx,var)
                                -> (varName var,VScalar $ xs ! idx)) varsWithIdx
        -- evaluate the constraint function
        -- NOTE NLOP requires constraints to be of the form c <= 0
        constraintVal = case evalN valMap $ constraintValueId constraint of
                          VR c0 -> if upBound
                                   then c0 - constraintUpperBound constraint
                                   else constraintLowerBound constraint - c0
                          interpVal -> error $ "Given constraint function is non-scalar: "
                                             ++ show interpVal
        -- NOTE constraintPartialDerivatives should be in corresponding order to the list variables
        -- so this should yield the same index specified by varsWithIdx
        partialsWithIdx = zip [0..] $ constraintPartialDerivatives constraint
        -- evaluate partial derivatives of constraints
        -- if a lowerbound, we need to negate the result
        grad = Prelude.map (\(idx,pID) -> (idx,evalPD valMap pID)) partialsWithIdx
      in do case mgrad of
              -- when Just grad0, write the values of each parital derivative computed in grad
              -- at the correct index (grad0 is a mutable IOVector)
              Just grad0 -> sequence_ $ Prelude.map (\(idx,v) -> write grad0 idx v) grad
              Nothing -> return ()
            return constraintVal

    allGenConstraints = Prelude.concatMap
                        (\g -> [constraintFunc (g,False),constraintFunc (g,True)]) generalConstraints

    vMap = mkValMap values

    -- variable bounds
    infinity = 1 / 0
    upperBounds = Prelude.map (\(_,v) -> getUpperBound $ varName v) varsWithIdx
    getUpperBound vName = case filterUpBounds vName of
                            [] -> infinity
                            [BoxUpper _ bID] -> case Map.lookup bID vMap of
                                                  Just (VScalar d) -> d
                                                  Just s -> error $ "bound is not VScalar " ++ show s
                                                  Nothing -> error $ "bound missing: " ++ show bID
                            bs -> error $ "duplicate upper bounds for " ++ vName
    filterUpBounds vName = Prelude.filter (\b ->
                                            case b of
                                              BoxUpper bName _ -> bName == vName
                                              _ -> False
                                             ) boxConstraints
    lowerBounds = Prelude.map (\(_,v) -> getLowerBound $ varName v) varsWithIdx
    getLowerBound vName = case filterLowBounds vName of
                            [] -> -infinity
                            [BoxLower _ bID] -> case Map.lookup bID vMap of
                                                  Just (VScalar d) -> d
                                                  Just s -> error $ "bound: "++show bID
                                                                   ++" is not VScalar " ++ show s
                                                  Nothing -> error $ "bound missing: " ++ show bID
                            bs -> error $ "duplicate lower bounds for " ++ vName
    filterLowBounds vName = Prelude.filter (\b ->
                                            case b of
                                              BoxLower bName _ -> bName == vName
                                              _ -> False
                                             ) boxConstraints

    -- for each variable thats been assigned an index in varsWithIdx, lookup the initial value
    -- provided by values in OptimizationProblem and pack it into a Vector in the correct order
    initVals :: Vector Double
    initVals = fromList
             $ Prelude.map (\(idx,val) -> case val of
                                            VScalar d -> d
                                            val -> error $ "initialVals given non-scalar: " ++ show val)
             $ List.sortOn fst
             $ Prelude.map (\(idx,var)
                            -> (idx,fromJust $ Map.lookup (varName var) vMap)) varsWithIdx

  in do putStrLn $ "Constructing NLOPT on algorithm: " ++ show algorithm
        putStrLn $ "Variables" ++ show variables
        -- Configure optimization problem
        mOpt <- NLOPT.create algorithm $ fromIntegral size
        let opt = case mOpt of
                    Just opt0 -> opt0
                    Nothing -> error $ "failed to create nlopt problem"
        NLOPT.set_xtol_rel opt (1e-4)
        -- Set Objective Function
        NLOPT.set_min_objective opt objFunc ()
        -- Set General (Inequality) Constraints
        sequence_ $ Prelude.map (\g -> NLOPT.add_inequality_constraint opt g () (1e-8)) allGenConstraints
        -- Set Box Cconstraints
        NLOPT.set_lower_bounds opt $ fromList lowerBounds
        NLOPT.set_upper_bounds opt $ fromList upperBounds
        -- TODO set equality constraints?
        -- Perform Optimization
        putStrLn $ "Running NLOPT minimize objective"
        output <- NLOPT.optimize opt initVals
        putStrLn $ "Finished with output: "
        -- TODO need to return the output paired with varNames
        printOutput output
        NLOPT.destroy opt
        return $ formatOutput varsWithIdx output

-- | NLOPT Output Pretty Printer
printOutput :: NLOPT.Output -> IO ()
printOutput (NLOPT.Output resultCode resultCost resultParams) =
  do putStrLn $
          "resultCode : " ++ show resultCode
       ++ "\nresultCost : " ++ show resultCost
       ++ "\nresultParams : " ++ show resultParams

formatOutput :: [(Int,Variable)] -> NLOPT.Output -> (Double,[((VarName,Int),Double)])
formatOutput varsWithIdx (NLOPT.Output resultCode resultCost resultParams) =
  let
    -- TODO assumes all variables are VScalar, hence (vName,0)
    varNames = Prelude.map (\(_,Variable vName _ _) -> (vName,0)) varsWithIdx
    -- NOTE assumes varsWithIdx and resultParams are in corresponding order (they should be)
    resultWithVars = zip varNames $ toList resultParams
  in case resultCode of
       NLOPT.SUCCESS -> (resultCost,resultWithVars)
       NLOPT.STOPVAL_REACHED -> (resultCost,resultWithVars)
       _ -> error $ "NLOPT failed with result code: " ++ show resultCode
