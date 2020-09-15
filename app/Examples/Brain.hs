module Examples.Brain where

import HashedExpression
import System.FilePath ((</>))
import Prelude hiding ((**), (^))

brain_reconstructFromMRI :: OptimizationProblem
brain_reconstructFromMRI =
  let -- variables
      x = variable2D @128 @128 "x"
      -- parameters
      im = param2D @128 @128 "im"
      re = param2D @128 @128 "re"
      mask = param2D @128 @128 "mask"
      -- regularization
      lambda = 3000
      regularization = lambda * (norm2square (rotate (0, 1) x - x) + norm2square (rotate (1, 0) x - x))
   in OptimizationProblem
        { objective = norm2square ((mask +: 0) * (ft (x +: 0) - (re +: im))) + regularization,
          constraints =
            [ x .<= VFile (HDF5 "bound.h5" "ub"),
              x .>= VFile (HDF5 "bound.h5" "lb")
            ],
          values =
            [ im :-> VFile (HDF5 "kspace.h5" "im"),
              re :-> VFile (HDF5 "kspace.h5" "re"),
              mask :-> VFile (HDF5 "mask.h5" "mask")
            ],
          workingDir = "examples" </> "Brain"
        }

brain :: IO ()
brain = proceed brain_reconstructFromMRI CSimpleConfig {output = OutputHDF5, maxIteration = Nothing}
