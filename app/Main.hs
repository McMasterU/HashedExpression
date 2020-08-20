{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import qualified Data.Array as Array
import Data.Complex
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import Data.Map (empty, fromList, union)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.STRef.Strict
import qualified Data.Set as Set
import Graphics.EasyPlot
import HashedExpression.Internal.Expression
import HashedExpression.Codegen
import HashedExpression.Codegen.CSimple
import HashedExpression.Differentiation.Reverse
import HashedExpression.Interp
import HashedExpression.Operation
import HashedExpression.Problem
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import HashedExpression.Value
import Data.String.Interpolate
import Prelude hiding ((^))
import Control.Monad (forM_)
import Data.Data

main :: IO ()
main = do
  let x = variable2D @10 @10 "x"
  let y = variable2D @10 @10 "y"
  let z = param "z"
  let f = x <.> y + z
  let valMap = Map.fromList [("z", VScalar 1.2)]
  case constructProblem f (Constraint []) of
    ProblemValid problem ->
      case generateProblemCode (CSimpleConfig { output = OutputText }) problem valMap of
        Success proceed -> proceed "algorithms/lbfgs-b"
        _ -> return ()
    _ -> return ()
