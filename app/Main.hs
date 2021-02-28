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
import Examples.SVM (ex6)

main :: IO ()
main = ex6
