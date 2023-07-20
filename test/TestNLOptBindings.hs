{-|
Module      : test.TestNLOptBindings
Copyright   :  (c) Curtis D'Alves 2023
License     :  GPL (see the LICENSE file)
Maintainer  :  curtis.dalves@gmail.com
Stability   :  experimental
Portability :  portable

Description:
  Tests for using NLOpt solver with Hashed Expression, see documentation for haskell bindings
  - https://hackage.haskell.org/package/nlopt-haskell-0.1.3.0/docs/Numeric-Optimization-NLOPT-Bindings.html
  Bindings very closely follow the corresponding c functions in the official NLOpt documentation
  - https://nlopt.readthedocs.io/en/latest/
-}
module TestNLOptBindings where

import HashedExpression
import HashedExpression.Modeling.Typed
import HashedExpression.Prettify
import HashedExpression.Solvers.NLOPT

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Vector.Storable hiding ((++))
import Data.Vector.Storable.Mutable hiding ((++))
import qualified Data.Primitive.Array as Array
import Data.Vector.Mutable (fromMutableArray)

import Prelude hiding ((^))

import qualified Numeric.Optimization.NLOPT.Bindings as NLOPT


-- | Example Usage of NLOPT Solver (no integration with HashedExpression)
testNLOPT :: IO ()
testNLOPT =
  let
    algorithm = NLOPT.LD_SLSQP
              -- NLOPT.LD_CCSAQ
    size = 2
    -- f(x) = 5*x0 + x1*x1
    objFunc :: NLOPT.ScalarFunction ()
    objFunc xs mgrad _ =
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
        NLOPT.set_min_objective opt objFunc ()
        output <- NLOPT.optimize opt initV
        printOutput output
        NLOPT.destroy opt

-- | Example Hashed Expression Problem
exProblem1 :: OptimizationProblem
exProblem1 =
  let
    x0 = variable "x0"
    x1 = variable "x1"
    objective = x0*x0 + x1*x1
    -- objective = (x0-1)*(x0-1) + (x1-1)*(x1-1)
    -- objective = (x0-1)HashedExpression.^2 + (x1-1)HashedExpression.^2
    initialVals = [x0 :-> VScalar 5.0
                  ,x1 :-> VScalar (-5.0)]
  in OptimizationProblem
     { objective = objective
     , constraints = []
     , values = initialVals
     }

testExProblem1 = nloptSolve exProblem1 NLOPT.LD_SLSQP

-- Banana Function (Rosenbrock)
exProblem2 :: OptimizationProblem
exProblem2 = 
  let
    a :: TypedExpr Scalar R
    a = 1
    b :: TypedExpr Scalar R
    b = 100

    x0 = variable "x0"
    x1 = variable "x1"
    objective = (a-x0)^2 + b*(x1-x0^2)^2
    initialVals = [x0 :-> VScalar 5.0
                  ,x1 :-> VScalar (-5.0)]
  in OptimizationProblem
     { objective = objective
     , constraints = []
     , values = initialVals
     }


testExProblem2 = nloptSolve exProblem2 NLOPT.LD_SLSQP

-- Banana Function (Rosenbrock)
exProblem3 :: OptimizationProblem
exProblem3 = 
  let
    a :: TypedExpr Scalar R
    a = 3
    b :: TypedExpr Scalar R
    b = 100

    x0 = variable "x0"
    x1 = variable "x1"
    objective = (a-x0)^2 + b*(x1-x0^2)^2
    initialVals = [x0 :-> VScalar 5.0
                  ,x1 :-> VScalar (-5.0)]
  in OptimizationProblem
     { objective = objective
     , constraints = []
     , values = initialVals
     }


testExProblem3 = nloptSolve exProblem3 NLOPT.LD_SLSQP

-- Sin x0 + Cos x1
exProblem4 :: OptimizationProblem
exProblem4 = 
  let
    x0 = variable "x0"
    x1 = variable "x1"
    objective = sin x0 + cos x1
    initialVals = [x0 :-> VScalar 5.0
                  ,x1 :-> VScalar (-5.0)]
  in OptimizationProblem
     { objective = objective
     , constraints = []
     , values = initialVals
     }


testExProblem4 = nloptSolve exProblem4 NLOPT.LD_SLSQP

-- x^2
exProblem5 :: OptimizationProblem
exProblem5 = 
  let
    x = variable "x"
    objective = x^2
    initialVals = [x :-> VScalar 1]
  in OptimizationProblem
     { objective = objective
     , constraints = [    x .>= (-2.0)
                        , x .<= 2.0]
     , values = initialVals
     }


testExProblem5 = nloptSolve exProblem5 NLOPT.LD_SLSQP

-- x1^2 - x1 + x0^2 - 3x0
exProblem6 :: OptimizationProblem
exProblem6 = 
  let
    x0 = variable "x0"
    x1 = variable "x1"
    objective = x1^2 - x1 + x0^2 - 3*x0
    initialVals = [x0 :-> VScalar 5.0
                  ,x1 :-> VScalar (-5.0)]
  in OptimizationProblem
     { objective = objective
     , constraints = []
     , values = initialVals
     }


testExProblem6 = nloptSolve exProblem6 NLOPT.LD_SLSQP

-- x^4 + x^2
exProblem7 :: OptimizationProblem
exProblem7 = 
  let
    x = variable "x"
    objective = x^4 + x^2
    initialVals = [x :-> VScalar 5.0]
  in OptimizationProblem
     { objective = objective
     , constraints = []
     , values = initialVals
     }


testExProblem7 = nloptSolve exProblem7 NLOPT.LD_SLSQP

-- x0^2 - x1^2 + 2
exProblem8 :: OptimizationProblem
exProblem8 = 
  let
    x0 = variable "x0"
    x1 = variable "x1"
    objective = x0^2 - x1^2
    initialVals = [x0 :-> VScalar 5.0
                  ,x1 :-> VScalar (-5.0)]
  in OptimizationProblem
     { objective = objective
     , constraints = []
     , values = initialVals
     }


testExProblem8 = nloptSolve exProblem8 NLOPT.LD_SLSQP