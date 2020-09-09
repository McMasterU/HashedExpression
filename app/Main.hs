{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import qualified Data.Array as Array
import Data.List (intercalate)
import Data.Map (empty, fromList, union)
import qualified Data.Map as Map
import HashedExpression
import System.FilePath ((</>))
import Prelude hiding ((^))

linearRegression :: OptimizationProblem
linearRegression =
  let x = param1D @97 "x"
      y = param1D @97 "y"
      theta0 = variable "theta0"
      theta1 = variable "theta1"
      objective = norm2square ((theta0 *. 1) + (theta1 *. x) - y)
   in OptimizationProblem
        { objective = objective,
          constraints = [],
          values =
            [ x :-> VFile (TXT "x.txt"),
              y :-> VFile (TXT "y.txt")
            ],
          workingDir = "problems" </> "ex1"
        }

main :: IO ()
main = proceed linearRegression CSimpleConfig {output = OutputText}
