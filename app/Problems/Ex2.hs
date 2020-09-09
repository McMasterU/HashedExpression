module Problems.Ex2 where

import HashedExpression
import System.FilePath ((</>))
import Prelude hiding ((^), (**))

sigmoid :: (Dimension d) => Expression d R -> Expression d R
sigmoid x = 1.0 / (1.0 + exp (-x))

ex2_logisticRegression :: OptimizationProblem
ex2_logisticRegression =
  let x = param2D @118 @28 "x"
      y = param2D @118 @1 "y"
      theta = variable2D @28 @1 "theta"
      hypothesis = sigmoid (x ** theta)
      lambda = 1
      regTheta = project (range @1 @27, at @0) theta
      regularization = (lambda / 2) * (regTheta <.> regTheta)
   in OptimizationProblem
        { objective = sumElements ((-y) * log hypothesis - (1 - y) * log (1 - hypothesis)) + regularization,
          constraints = [],
          values =
            [ x :-> VFile (TXT "x_expanded.txt"),
              y :-> VFile (TXT "y.txt")
            ],
          workingDir = "problems" </> "ex2"
        }

ex2 :: IO ()
ex2 = proceed ex2_logisticRegression CSimpleConfig {output = OutputText}
