{-|
Module      :  HashedExpression
Copyright   :  (c) OCA 2020
License     :  MIT (see the LICENSE file)
Maintainer  :  anandc@mcmaster.ca
Stability   :  provisional
Portability :  unportable

This module exports everything needed to build and evaluate HashedExpressions;
including all HashedExpression operators, derivative functions, eval methods
and normalization. For example,

@
 x = variable "x"
 y = variable "y"
 expr = x + y
@

the above code creates a simple HashedExpression using the
'variable' constructor method and taking advantage of the 'Num' class instance

-}
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
    sum,
    product,
    normalize,
    -- * Evaluation
    Evaluable (..),
    -- * Derivatives
    exteriorDerivative,
    derivativeAllVars,
    prettify
  )
where
import HashedExpression.Prettify
import HashedExpression.Derivative
import HashedExpression.Internal.CollectDifferential
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Normalize
import HashedExpression.Interp
import HashedExpression.Operation
import Prelude hiding ((^))
