{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import Examples.Brain
import Examples.LinearRegression
import Examples.LogisticRegression ()
import Examples.NeuralNetwork

main :: IO ()
main = ex4
