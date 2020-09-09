{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import qualified Data.Array as Array
import Data.List (intercalate)
import Data.Map (empty, fromList, union)
import qualified Data.Map as Map
import HashedExpression.Codegen
import HashedExpression.Codegen.CSimple
import HashedExpression.Internal.Expression
import HashedExpression.Operation
import HashedExpression.Prettify
import HashedExpression.Value
import HashedExpression.Problem
import Prelude hiding ((^))

main :: IO ()
main = do
  let x = variable2D @10 @10 "x"
  let obj = x <.> log x
  case constructProblem obj (Constraint [
        x <.> log x .>= VNum (-30)
      ]) of
    ProblemValid p ->
      case generateProblemCode CSimpleConfig {output = OutputText} p Map.empty of
        Success proceed -> proceed "solvers/ipopt"
