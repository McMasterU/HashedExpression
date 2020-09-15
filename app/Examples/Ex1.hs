module Examples.Ex1 where

import HashedExpression
import System.FilePath ((</>))
import Prelude hiding ((^), (**))

ex1_linearRegression :: OptimizationProblem
ex1_linearRegression =
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
          workingDir = "examples" </> "ex1"
        }

ex1 :: IO ()
ex1 = proceed ex1_linearRegression CSimpleConfig {output = OutputText, maxIteration = Nothing}
