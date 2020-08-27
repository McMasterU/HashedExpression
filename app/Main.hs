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
import Graphics.EasyPlot
import HashedExpression.Codegen
import HashedExpression.Codegen.CSimple
import HashedExpression.Differentiation.Reverse
import HashedExpression.Internal.Expression
import HashedExpression.Interp
import HashedExpression.Operation
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import HashedExpression.Value
import HashedExpression.Problem
import Prelude hiding ((^))

main :: IO ()
main = do
  let x = variable2D @10 @10 "x"
  let obj = x <.> log x
  case constructProblem obj (Constraint []) of 
    ProblemValid p -> 
      case generateProblemCode CSimpleConfig {output = OutputText} p Map.empty of 
        Success proceed -> proceed "algorithms/ipopt"
