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
  ( module HashedExpression.Internal.Simplify,
    module HashedExpression.Prettify,
    module HashedExpression.Interp,
    module HashedExpression.Internal.Base,
    module HashedExpression.Problem,
    module HashedExpression.Interface,
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
import GHC.TypeLits (KnownNat, Nat)
import HashedExpression.Codegen
import HashedExpression.Codegen.CSimple
import HashedExpression.Interface
import HashedExpression.Internal.Base
import HashedExpression.Internal.Node
import HashedExpression.Internal.Simplify
import HashedExpression.Interp
import HashedExpression.Prettify
import HashedExpression.Problem
import HashedExpression.Value
import HashedExpression.Modeling.Typed

import Prelude hiding ((**), (^))

proceed ::
  Codegen codegen =>
  -- | the optimization problem to solve
  OptimizationProblem ->
  -- | code generator
  codegen ->
  -- | working directory
  FilePath ->
  IO ()
proceed OptimizationProblem {..} codegen workingDir = case constructProblemAndGenCode of
  Right ok -> ok workingDir
  Left reason -> putStrLn reason
  where
    constructProblemAndGenCode :: Either String (FilePath -> IO ())
    constructProblemAndGenCode = do
      problem <- constructProblem objective constraints
      generateProblemCode codegen problem (mkValMap values)

