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
  ( module HashedExpression.Internal.Expression,
    module HashedExpression.Operation,
    module HashedExpression.Internal.Simplify,
    module HashedExpression.Prettify,
    module HashedExpression.Interp,
    module HashedExpression.Problem,
    module HashedExpression.Value,
  )
where

import HashedExpression.Internal.Expression
import HashedExpression.Internal.Simplify
import HashedExpression.Interp
import HashedExpression.Operation
import HashedExpression.Prettify
import HashedExpression.Problem
import HashedExpression.Value
import Prelude hiding ((**), (^))
