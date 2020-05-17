{-|
Module      :  HashedExpression
Copyright   :  (c) OCA 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  anandc@mcmaster.ca
Stability   :  provisional
Portability :  unportable

This module exports everything needed to build and evaluate HashedExpression's
@
 x = variable "x"
 y = variable "y"
 expr = x + y
@
TODO put some better example code here?? maybe example with eval
TODO point to what to import to generate C
-}
module HashedExpression
  ( exteriorDerivative,
    derivativeAllVars,
    R,
    C,
    Covector,
    Expression,
    Scalar,
    PowerOp (..),
    PiecewiseOp (..),
    VectorSpaceOp (..),
    FTOp (..),
    NodeID,
    ComplexRealOp (..),
    RotateOp (..),
    InnerProductSpaceOp (..),
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
    Evaluable (..),
  )
where

import HashedExpression.Derivative
import HashedExpression.Internal.CollectDifferential
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Normalize
import HashedExpression.Interp
import HashedExpression.Operation
import Prelude hiding ((^))
