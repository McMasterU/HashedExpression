{-# LANGUAGE TypeOperators #-}

module Examples.SVM where

import Data.Function ((&))
import GHC.TypeLits (KnownNat, type (+), type (-))
import HashedExpression
import HashedExpression.Modeling.Typed
import System.FilePath ((</>))
import Prelude hiding ((**), (^))

ex6_svm :: OptimizationProblem
ex6_svm =
  let alpha = variable2D @863 @1 "alpha"
      km = param2D @863 @863 "km"
      y = param2D @863 @1 "y"
      alphaLB = bound2D @863 @1 "alphaLB"
   in OptimizationProblem
        { objective =
            sumElements alpha
              - (1 / 2) * sumElements ((alpha ** transpose alpha) * km * (y ** transpose y)),
          constraints =
            [ (alpha <.> y) .== 0.0,
              alpha .>= alphaLB
            ],
          values =
            [ km :-> VFile (HDF5 "data.h5" "km"),
              y :-> VFile (HDF5 "data.h5" "y"),
              alphaLB :-> VFile (HDF5 "data.h5" "alphaLB")
            ]
        }

ex6 :: IO ()
ex6 =
  proceed
    ex6_svm
    CSimpleConfig {output = OutputHDF5, maxIteration = Just 600}
    ("examples" </> "SVM")
