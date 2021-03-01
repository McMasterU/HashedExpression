{-# LANGUAGE TypeOperators #-}

module Examples.SVM where

import Data.Function ((&))
import GHC.TypeLits (KnownNat, type (+), type (-))
import HashedExpression
import HashedExpression.Modeling.Typed
import System.FilePath ((</>))
import Prelude hiding ((**), (^))

type N = 51

ex6_svm :: OptimizationProblem
ex6_svm =
  let alpha = variable2D @N @1 "alpha"
      km = param2D @N @N "km"
      y = param2D @N @1 "y"
      alphaLB = bound2D @N @1 "alphaLB"
   in OptimizationProblem
        { objective = (1 / 2) * sumElements (((alpha * y) ** transpose (alpha * y)) * km) - sumElements alpha,
          constraints =
            [ (alpha <.> y) .== 0.0,
              alpha .>= alphaLB
            ],
          values =
            [ km :-> VFile (HDF5 "data.h5" "km"),
              y :-> VFile (HDF5 "data.h5" "y"),
              alpha :-> VFile (HDF5 "data.h5" "alpha"),
              alphaLB :-> VFile (HDF5 "data.h5" "alphaLB")
            ]
        }

ex6 :: IO ()
ex6 =
  proceed
    ex6_svm
    CSimpleConfig {output = OutputHDF5, maxIteration = Just 600}
    ("examples" </> "SVM")
