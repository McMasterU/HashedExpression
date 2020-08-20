{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import Control.Monad (forM_)
import qualified Data.Array as Array
import Data.Complex
import Data.Data
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import Data.Map (empty, fromList, union)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.STRef.Strict
import qualified Data.Set as Set
import Data.String.Interpolate
import Graphics.EasyPlot
import HashedExpression.Codegen
import HashedExpression.Codegen.CSimple
import HashedExpression.Differentiation.Reverse
import HashedExpression.Internal.Expression
import HashedExpression.Interp
import HashedExpression.Operation
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import HashedExpression.Problem
import HashedExpression.Value
import Prelude hiding ((^))

main :: IO ()
main = do
  let x = variable2D @10 @10 "x"
  let y = variable2D @10 @10 "y"
  let z = variable "z"
  let n = variable "n"
  let exp = ^ n z
  let valMap =
        Map.fromList
          [ ("x", V2D $ Array.listArray ((0, 0), (9, 9)) (replicate 50 1 ++ replicate 50 2)),
            ("y", V2D $ Array.listArray ((0, 0), (9, 9)) (replicate 100 2)),
            ("z", VScalar 2),
            ("n", VScalar 3)
          ]
  print $ eval valMap exp
