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
  ( -- * Expression Constructors
    Expression,
    R,
    C,
    Covector,
    Scalar,
    PowerOp (..),
    PiecewiseOp (..),
    VectorSpaceOp (..),
    FTOp (..),
    NodeID,
    ComplexRealOp (..),
    RotateOp (..),
    InnerProductSpaceOp (..),

    -- * Combinators
    constant,
    constant1D,
    constant2D,
    constant3D,
    variable,
    variable1D,
    variable2D,
    variable3D,
    param,
    param1D,
    param2D,
    param3D,

    -- * Evaluation
    Evaluable (..),
    prettify,
  )
where

import HashedExpression.Internal.Expression
import HashedExpression.Interp
import HashedExpression.Operation
import HashedExpression.Prettify
import Prelude hiding ((^))
