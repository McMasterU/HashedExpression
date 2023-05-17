module TestNLOptBindings where

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

-- | NLOPT Output Pretty Printer
printOutput :: NLOPT.Output -> IO ()
printOutput (NLOPT.Output resultCode resultCost resultParams) =
  do putStrLn $
          "resultCode : " ++ show resultCode
       ++ "\nresultCost : " ++ show resultCost
       ++ "\nresultParams : " ++ show resultParams

-- | Example Usage of NLOPT Solver (no integration with HashedExpression)
testNLOPT :: IO ()
testNLOPT =
  let
    algorithm = NLOPT.LD_SLSQP
              -- NLOPT.LD_CCSAQ
    size = 2
    -- f(x) = 5*x0 + x1*x1
    sfunc :: NLOPT.ScalarFunction ()
    sfunc xs mgrad _ =
      let
        x0 = xs ! 0
        x1 = xs ! 1
        obj = x0*x0 + x1*x1

        grad0 = 2*x0
        grad1 = 2*x1

        grad = [grad0,grad1]
      in do case mgrad of
              Just grad0 -> sequence_ [ write grad0 n (grad Prelude.!! n) | n <- [0..size-1]]
              Nothing -> return ()
            return obj

    initV = fromList [-5.0,0.0]
  in do mOpt <- NLOPT.create algorithm $ fromIntegral size
        let opt = fromJust mOpt
        NLOPT.set_min_objective opt sfunc ()
        output <- NLOPT.optimize opt initV
        printOutput output
        NLOPT.destroy opt

-- | Example Hashed Expression Problem
exProblem :: OptimizationProblem
exProblem =
  let
    x0 = variable "x0"
    x1 = variable "x1"
    objective = x0*x0 + x1*x1
    initialVals = [x0 :-> VScalar 5.0
                  ,x1 :-> VScalar (-5.0)]
  in OptimizationProblem
     { objective = objective
     , constraints = []
     , values = initialVals
     }

-- | Hashed Expression Solver
nloptSolve :: OptimizationProblem -> NLOPT.Algorithm -> IO ()
nloptSolve (OptimizationProblem objective constraints values) algorithm =
  let
    prob@(Problem variables objectiveId expressionMap boxConstraints generalConstraints)
      = case constructProblem objective constraints of
          Left err -> error $ "error constructing problem: " ++ err
          Right prob -> prob

    evalN :: ValMap -> NodeID ->  InterpValue
    evalN valMap nID = eval valMap (expressionMap,nID)

    evalPD :: ValMap -> NodeID ->  Double
    evalPD valMap nID = case eval valMap (expressionMap,nID) of
                          VR pd -> pd
                          interpVal -> error $ "Given grad function is non-scalar: "
                                             ++ show (nID,interpVal)

    size :: Int
    size = Prelude.length variables

    varsWithIdx :: [(Int,Variable)]
    varsWithIdx = zip [0..] variables

    sfunc :: NLOPT.ScalarFunction ()
    sfunc xs mgrad _ =
      let
        valMap = Map.fromList
                 $ Prelude.map (\(idx,var)
                                -> (varName var,VScalar $ xs ! idx)) varsWithIdx
        obj = case evalN valMap objectiveId of
                VR obj0 -> obj0
                interpVal -> error $ "Given objective function is non-scalar: " ++ show interpVal
        grad = Prelude.map (\(idx,var)
                            -> (idx,evalPD valMap $ partialDerivativeId var)) varsWithIdx
      in do case mgrad of
              Just grad0 -> sequence_ $ Prelude.map (\(idx,v) -> write grad0 idx v) grad
              Nothing -> return ()
            return obj

    initVals :: Vector Double
    initVals = fromList
             $ Prelude.map (\(idx,val) -> case val of
                                            VScalar d -> d
                                            val -> error $ "initialVals given non-scalar: " ++ show val)
             $ List.sortOn fst
             $ Prelude.map (\(idx,var)
                            -> (idx,fromJust $ Map.lookup (varName var) (mkValMap values))) varsWithIdx

  in do putStrLn $ "Constructing NLOPT on algorithm: " ++ show algorithm
        putStrLn $ "Variables" ++ show variables
        mOpt <- NLOPT.create algorithm $ fromIntegral size
        let opt = case mOpt of
                    Just opt0 -> opt0
                    Nothing -> error $ "failed to create nlopt problem"
        putStrLn $ "Running NLOPT minimize objective"
        NLOPT.set_min_objective opt sfunc ()
        output <- NLOPT.optimize opt initVals
        putStrLn $ "Finished with output: "
        printOutput output
        NLOPT.destroy opt
