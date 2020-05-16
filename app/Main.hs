{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import Data.Array
import Data.Complex
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import Data.Map (empty, fromList, union)
import Data.Maybe (fromJust)
import Data.STRef.Strict
import qualified Data.Set as Set
import HashedExpression.Derivative.Partial
import Graphics.EasyPlot
import HashedExpression
import HashedExpression.Derivative
import HashedExpression.Interp
import HashedExpression.Operation
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import Data.String.Interpolate
import Prelude hiding ((^))

main :: IO ()
main = do
  let x = variable "x"
  let y = variable "y"
  let f = sum [x^2 , 2*x , 1] * (x^2 + 2 * y + 1)
  print $ allEntries f
