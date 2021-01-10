{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import qualified Data.Text.Lazy as LT
import Examples.Brain
import Examples.LinearRegression
import Examples.LogisticRegression
import Examples.NeuralNetwork
import HashedExpression
import HashedExpression.Modeling.Typed
import HashedExpression.Prettify

main :: IO ()
main = do
  let x = variable2D @128 @128 "x"
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
      objective =
            norm2square ((mask +: 0) * (ft (x +: 0) - (re +: im)))
              + lambda * regularization
  case constructProblem objective [] of 
    Right p -> writeFile "haha.dot" $ toDotCodeProblem ShowOp p
  
