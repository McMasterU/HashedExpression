-- |
-- Module      :  HashedExpression
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- This module exports everything needed to build and evaluate HashedExpressions;
-- including all HashedExpression operators, derivative functions, eval methods
-- and normalization. For example,
--
-- @
--  x = variable "x"
--  y = variable "y"
--  expr = x + y
-- @
--
-- the above code creates a simple HashedExpression using the
-- 'variable' constructor method and taking advantage of the 'Num' class instance
module HashedExpression
  ( module HashedExpression.Modeling.Typed,
    module HashedExpression.Internal.Simplify,
    module HashedExpression.Prettify,
    module HashedExpression.Interp,
    module HashedExpression.Problem,
    module HashedExpression.Internal.Base,
    module HashedExpression.Value,
    module HashedExpression.Codegen,
    module HashedExpression.Codegen.CSimple,
    ValueAssignment (..),
    OptimizationProblem (..),
    proceed,
  )
where

import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import HashedExpression.Codegen
import HashedExpression.Codegen.CSimple
import HashedExpression.Internal.Base
import HashedExpression.Internal.Node
import HashedExpression.Internal.Simplify
import HashedExpression.Interp
import HashedExpression.Modeling.Typed
import HashedExpression.Prettify
import HashedExpression.Problem
import HashedExpression.Value
import Prelude hiding ((**), (^))

data ValueAssignment
  = forall d et. Expression d et :-> Val

mkValMap :: [ValueAssignment] -> ValMap
mkValMap ss = Map.fromList $ mapMaybe f ss
  where
    f (e :-> val)
      | (_, _, Var name) <- retrieveNode nID mp = Just (name, val)
      | (_, _, Param name) <- retrieveNode nID mp = Just (name, val)
      | otherwise = Nothing
      where
        (mp, nID) = asExpression e

data OptimizationProblem = OptimizationProblem
  { objective :: Expression Scalar R,
    constraints :: [ConstraintStatement],
    values :: [ValueAssignment],
    workingDir :: String
  }

proceed :: Codegen codegen => OptimizationProblem -> codegen -> IO ()
proceed OptimizationProblem {..} codegen = do
  case constructProblem objective (Constraint constraints) of
    Right problem ->
      case generateProblemCode codegen problem (mkValMap values) of
        Right ok -> ok workingDir
        Left reason -> putStrLn reason
    Left reason -> putStrLn reason
