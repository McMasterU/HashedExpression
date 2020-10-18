{-# LANGUAGE TypeOperators #-}

module Examples.NeuralNetwork where

import Data.Function ((&))
import GHC.TypeLits (KnownNat, type (+), type (-))
import HashedExpression
import System.FilePath ((</>))
import Prelude hiding ((**), (^))
import HashedExpression.Modeling.Typed

sigmoid :: (IsShape d) => TypedExpr d R -> TypedExpr d R
sigmoid x = 1.0 / (1.0 + exp (- x))

prependColumn ::
  forall m n.
  (Injectable 0 (m - 1) m m, Injectable 1 n n (n + 1)) =>
  Double ->
  TypedExpr '[m, n] R ->
  TypedExpr (D2 m (n + 1)) R
prependColumn v exp = inject (range @0 @(m - 1), range @1 @n) exp (constant2D @m @(n + 1) v)

ex4_neuralNetwork :: OptimizationProblem
ex4_neuralNetwork =
  let x = param2D @5000 @400 "x"
      y = param2D @5000 @10 "y"
      -- variables
      theta1 = variable2D @401 @25 "theta1"
      theta2 = variable2D @26 @10 "theta2"
      -- neural net
      a1 = prependColumn 1 x
      z2 = sigmoid (a1 ** theta1)
      a2 = prependColumn 1 z2
      hypothesis = sigmoid (a2 ** theta2)
      -- regularization
      lambda = 1
      regTheta1 = project (range @1 @400, range @0 @24) theta1 -- no first row
      regTheta2 = project (range @1 @25, range @0 @9) theta2 -- no first row
      regularization = (lambda / 2) * (norm2square regTheta1 + norm2square regTheta2)
   in OptimizationProblem
        { objective = sumElements ((- y) * log hypothesis - (1 - y) * log (1 - hypothesis)) + regularization,
          constraints = [],
          values =
            [ x :-> VFile (HDF5 "data.h5" "x"),
              y :-> VFile (HDF5 "data.h5" "y")
            ],
          workingDir = "examples" </> "NeuralNetwork"
        }

ex4 :: IO ()
ex4 = proceed ex4_neuralNetwork CSimpleConfig {output = OutputHDF5, maxIteration = Just 400}
