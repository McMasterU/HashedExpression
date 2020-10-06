module Examples.LogisticRegression where

import HashedExpression
import System.FilePath ((</>))
import Prelude hiding ((**), (^))

sigmoid :: (Dimension d) => TypedExpr d R -> TypedExpr d R
sigmoid x = 1.0 / (1.0 + exp (- x))

ex2_logisticRegression :: OptimizationProblem
ex2_logisticRegression =
  let -- variables
      theta = variable1D @28 "theta"
      -- parameters
      x = param2D @118 @28 "x"
      y = param1D @118 "y"
      hypothesis = sigmoid (x ** theta)
      -- regularization
      lambda = 1
      regTheta = project (range @1 @27) theta
      regularization = (lambda / 2) * (regTheta <.> regTheta)
   in OptimizationProblem
        { objective = sumElements ((- y) * log hypothesis - (1 - y) * log (1 - hypothesis)) + regularization,
          constraints = [],
          values =
            [ x :-> VFile (TXT "x_expanded.txt"),
              y :-> VFile (TXT "y.txt")
            ],
          workingDir = "examples" </> "LogisticRegression"
        }

ex2 :: IO ()
ex2 = proceed ex2_logisticRegression CSimpleConfig {output = OutputText, maxIteration = Nothing}
