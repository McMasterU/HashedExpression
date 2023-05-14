module Examples.IpoptSimple.IpoptSimple where

import HashedExpression
import System.FilePath ((</>))
import Prelude hiding ((^), (**))
import HashedExpression.Modeling.Typed


genProblemCode :: OptimizationProblem -> IO ()
genProblemCode ex = proceed ex
                            CSimpleConfig {output = OutputText, maxIteration = Nothing}
                            ("app" </> "Examples" </> "IpoptSimple" </> "src")

ex1_Betts :: OptimizationProblem
ex1_Betts =
  let
      [x1,x2] = map variable ["x1","x2"]
      objective = 0.01*x1^2 + x2^2 - 100
   in OptimizationProblem
        { objective = objective,
          constraints = [ x1 .>= 2.0
                        , x1 .<= 50.0
                        , x2 .>= (- 50.0)
                        , x2 .<= 50.0
                        , (10.0 * x1 - x2) .>= 10.0
                        ],
          values =
            [
              -- x1 :-> VFile (TXT "x1.txt"),
              -- x2 :-> VFile (TXT "x2.txt")
            ]
        }

