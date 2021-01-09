module Examples.Brain where

import HashedExpression
import HashedExpression.Modeling.Typed
import System.FilePath ((</>))
import Prelude hiding ((**), (^))

brainReconstructFromMRI :: OptimizationProblem
brainReconstructFromMRI =
  let -- variables
      x = variable2D @128 @128 "x"
      --- bound
      xLowerBound = bound2D @128 @128 "x_lb"
      xUpperBound = bound2D @128 @128 "x_ub"
      -- parameters
      im = param2D @128 @128 "im"
      re = param2D @128 @128 "re"
      mask = param2D @128 @128 "mask"
      -- regularization
      regularization = norm2square (rotate (0, 1) x - x) + norm2square (rotate (1, 0) x - x)
      lambda = 3000
   in OptimizationProblem
        { objective =
            norm2square ((mask +: 0) * (ft (x +: 0) - (re +: im)))
              + lambda * regularization,
          constraints =
            [ x .<= xUpperBound,
              x .>= xLowerBound
            ],
          values =
            [ im :-> VFile (HDF5 "kspace.h5" "im"),
              re :-> VFile (HDF5 "kspace.h5" "re"),
              mask :-> VFile (HDF5 "mask.h5" "mask"),
              xLowerBound :-> VFile (HDF5 "bound.h5" "lb"),
              xUpperBound :-> VFile (HDF5 "bound.h5" "ub")
            ]
        }

brain :: IO ()
brain =
  proceed
    brainReconstructFromMRI
    CSimpleConfig {output = OutputHDF5, maxIteration = Nothing}
    ("examples" </> "Brain")
