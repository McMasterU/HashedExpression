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

main :: IO ()
main = do
  let vX = variable2D @10 @10 "X"
      vY = variable2D @10 @10 "Y"
      z = variable "z"
      exp = vX <.> vY + z
  showExp $ partialDerivative exp vX 
