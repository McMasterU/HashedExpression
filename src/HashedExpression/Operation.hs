{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  HashedExpression.Operation
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- This module overloads and redefines operations to use with expressions,
-- defining additional operations as necessary as well as providing the means
-- to create multidimensional constants and variables
module HashedExpression.Operation where

import Data.Array
import Data.IntMap.Strict (fromList, union, unions)
import Data.List (sort)
import Data.Proxy
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import GHC.TypeLits (CmpNat, Div, KnownNat, Mod, natVal, type (+), type (-), type (<=))
import HashedExpression.Internal
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Hash
import HashedExpression.Internal.Node
import HashedExpression.Internal.OperationSpec
import HashedExpression.Internal.Utils
import Prelude hiding ((^), (**))

instance (Dimension d) => PowerOp (Expression d et) Int where
  (^) :: Expression d et -> Int -> Expression d et
  (^) e1 x = applyUnary (specPower x) e1

-- | Converts a double-precision floating-point number to a real-number expression with dimension constraint `d`
--
-- @
--    (fromDouble 15) :: Expression Scalar R
-- @
fromDouble :: forall d. Dimension d => Double -> Expression d R
fromDouble value = fromNode (toShape (Proxy @d), R, Const value)

-- | Basic operations on Num class expressions with dimension constraint `d`
instance Dimension d => Num (Expression d R) where
  e1 + e2 = applyNary specSum [e1, e2]
  e1 * e2 = applyNary specMul [e1, e2]
  negate = applyUnary specNeg
  fromInteger val = fromDouble $ fromIntegral val
  abs = error "TODO: abs"
  signum = error "Not applicable to tensor"

-- | Define division operation and representation for real-number fractional expressions with dimension constraint `d`
--
-- @
--    let e1 = (fromRational 11) :: Expression Scalar R
--    let e2 = (fromRational 12) :: Expression Scalar R
--    e1 / e2
-- @
instance Dimension d => Fractional (Expression d R) where
  e1 / e2 = e1 * e2 ^ (-1)
  fromRational r = fromDouble $ fromRational r

-- | Represent common functions for real-number floating-point expressions with dimension constraint `d`
--
-- @
--    let val = (fromDouble 1.2345) :: Expression Scalar R
--    `function` val
-- @
instance Dimension d => Floating (Expression d R) where
  pi = fromDouble pi
  sqrt = applyUnary specSqrt
  exp = applyUnary specExp
  log = applyUnary specLog
  sin = applyUnary specSin
  cos = applyUnary specCos
  tan = applyUnary specTan
  asin = applyUnary specAsin
  acos = applyUnary specAcos
  atan = applyUnary specAtan
  sinh = applyUnary specSinh
  cosh = applyUnary specCosh
  tanh = applyUnary specTanh
  asinh = applyUnary specAsinh
  acosh = applyUnary specAcosh
  atanh = applyUnary specAtanh

-- | Basic operations on complex-number expressions with dimension constraint `d`
--
-- @
--     let e1 = ((fromDouble 10) +: fromIntegral 1) :: Expression Scalar C
--     let e2 = ((fromDouble 15) +: fromIntegral 3) :: Expression Scalar C
--     e1 `binary operation` e2
--     `unary operation` e1
-- @
instance Dimension d => Num (Expression d C) where
  e1 + e2 = applyNary specSum [e1, e2]
  e1 * e2 = applyNary specMul [e1, e2]
  negate = applyUnary specNeg
  fromInteger val = fromIntegral val +: 0
  abs = error "TODO: abs"
  signum = error "Not applicable"

-- | Define division operation and transformation to complex fractional expression from rational real number with dimension constraint `d`
--
-- @
--     let e1 = ((fromRational 10) +: fromIntegral 1) :: Expression Scalar C
--     let e2 = ((fromRational 15) +: fromIntegral 3) :: Expression Scalar C
--     e1 / e2
-- @
instance Dimension d => Fractional (Expression d C) where
  e1 / e2 = e1 * e2 ^ (-1)
  fromRational r = fromDouble (fromRational r) +: 0

-- | Scale in vector space
instance ScaleOp (Expression Scalar R) (Expression d et) where
  scale :: Expression Scalar s -> Expression d et -> Expression d et
  scale = applyBinary specScale

--
--instance ScaleOp (Expression Scalar R) (Expression d C) where
--  scale :: Expression Scalar s -> Expression d et -> Expression d et
--  scale = applyBinary specScale

instance ScaleOp (Expression Scalar C) (Expression d C) where
  scale :: Expression Scalar s -> Expression d et -> Expression d et
  scale = applyBinary specScale

-- | Create a complex expression from two real number expression
--
--  @
--   let exp = ((fromDouble 1.2345) :: Expression Scalar R) +: ((fromDouble 2) :: Expression Scalar R)
--   xRe exp
--   xIm exp
--  @
instance (Dimension d) => ComplexRealOp (Expression d R) (Expression d C) where
  (+:) :: Expression d R -> Expression d R -> Expression d C
  (+:) = applyBinary specRealImag
  xRe :: Expression d C -> Expression d R
  xRe = applyUnary specRealPart
  xIm :: Expression d C -> Expression d R
  xIm = applyUnary specImagPart
  conjugate :: Expression d C -> Expression d C
  conjugate = applyUnary specConjugate

instance InnerProductSpaceOp (Expression d et) (Expression Scalar et) where
  (<.>) :: Expression d s -> Expression d et -> Expression Scalar et
  (<.>) = applyBinary specInnerProd

-- | Huber loss: https://en.wikipedia.org/wiki/Huber_loss.
-- Piecewise loss function where the loss algorithm chosen depends on delta
huber :: forall d. (Dimension d) => Double -> Expression d R -> Expression d R
huber delta e = piecewise [- delta, delta] e [outerLeft, inner, outerRight]
  where
    inner = constant 0.5 *. (e * e)
    outerLeft = constant (- delta) *. e - constant (delta * delta / 2) *. 1
    outerRight = constant delta *. e - constant (delta * delta / 2) *. 1

-- | Norm 2 uses inner product space
norm2 :: (Dimension d) => Expression d R -> Expression Scalar R
norm2 expr = sqrt (expr <.> expr)

---- | Norm 1
norm1 :: (Dimension d) => Expression d R -> Expression Scalar R
norm1 expr = sumElements (sqrt (expr * expr))

-- | Norm 2 square interface
class Norm2SquareOp a b | a -> b where
  norm2square :: a -> b

-- | Norm 2 square of real expression
instance (Dimension d) => Norm2SquareOp (Expression d R) (Expression Scalar R) where
  norm2square :: Expression d R -> Expression Scalar R
  norm2square exp = exp <.> exp

-- | Norm 2 square of complex expression
instance (Dimension d) => Norm2SquareOp (Expression d C) (Expression Scalar R) where
  norm2square :: Expression d C -> Expression Scalar R
  norm2square exp = (xRe exp <.> xRe exp) + (xIm exp <.> xIm exp)

-- | Outlier-sensitive error measure using huber loss
huberNorm :: (Dimension d) => Double -> Expression d R -> Expression Scalar R
huberNorm alpha = sumElements . huber alpha

-- | Discrete fourier transform
--
-- | Sum elements of a `d`-dimensional vector
sumElements :: forall d. (Dimension d) => Expression d R -> Expression Scalar R
sumElements expr = expr <.> 1

-- | Piecewise, with a condition expression and branch expressions
-- This is element corresponding, so condition and all branches should have the same dimension and shape
instance (Dimension d) => PiecewiseOp (Expression d R) (Expression d et) where
  piecewise :: HasCallStack => [Double] -> Expression d R -> [Expression d et] -> Expression d et
  piecewise marks conditionExp branchExps = applyConditionAry (specPiecewise marks) conditionExp branchExps

-- Fourier transform on complex expression
instance (Dimension d) => FTOp (Expression d C) (Expression d C) where
  ft :: Expression d C -> Expression d C
  ft = applyUnary specFT

  ift :: Expression d C -> Expression d C
  ift = applyUnary specIFT

-- |
instance (KnownNat n) => RotateOp Int (Expression (D1 n) et) where
  rotate :: Int -> Expression (D1 n) et -> Expression (D1 n) et
  rotate x = applyUnary (specRotate [x])

instance (KnownNat m, KnownNat n) => RotateOp (Int, Int) (Expression (D2 m n) et) where
  rotate :: (Int, Int) -> Expression (D2 m n) et -> Expression (D2 m n) et
  rotate (x, y) = applyUnary (specRotate [x, y])

instance (KnownNat m, KnownNat n, KnownNat p) => RotateOp (Int, Int, Int) (Expression (D3 m n p) et) where
  rotate :: (Int, Int, Int) -> Expression (D3 m n p) et -> Expression (D3 m n p) et
  rotate (x, y, z) = applyUnary (specRotate [x, y, z])

-------------------------------------------------------------------------------
type x < y = (CmpNat x y ~ 'LT)

type Size start end step n = (((n + end - start) `Mod` n) `Div` step + 1)

-------------------------------------------------------------------------------
instance
  ( KnownNat i,
    KnownNat n,
    i < n
  ) =>
  ProjectInjectOp (Proxy i) (Expression (D1 n) et) (Expression Scalar et)
  where
  project _ = applyUnary (specProject [At $ nat @i])
  inject _ = applyBinary (specInject [At $ nat @i])

instance
  ( (KnownNat start, KnownNat end, KnownNat step),
    KnownNat n,
    (start < n, end < n, 0 < step),
    res ~ Size start end step n
  ) =>
  ProjectInjectOp (Proxy '(start, end, step)) (Expression (D1 n) et) (Expression (D1 res) et)
  where
  project _ = applyUnary (specProject [Range (nat @start) (nat @end) (nat @step)])
  inject _ = applyBinary (specInject [Range (nat @start) (nat @end) (nat @step)])

instance
  ( KnownNat m,
    KnownNat n,
    (KnownNat i, i < m),
    (KnownNat j, j < n)
  ) =>
  ProjectInjectOp (Proxy i, Proxy j) (Expression (D2 m n) et) (Expression Scalar et)
  where
  project _ = applyUnary (specProject [At (nat @i), At (nat @j)])
  inject _ = applyBinary (specInject [At (nat @i), At (nat @j)])

instance
  ( KnownNat m,
    KnownNat n,
    (KnownNat j, j < n),
    (KnownNat startM, KnownNat endM, KnownNat stepM),
    (startM < m, endM < m, 0 < stepM),
    resM ~ Size startM endM stepM m
  ) =>
  ProjectInjectOp (Proxy '(startM, endM, stepM), Proxy j) (Expression (D2 m n) et) (Expression (D1 resM) et)
  where
  project _ = applyUnary (specProject [Range (nat @startM) (nat @endM) (nat @stepM), At (nat @j)])
  inject _ = applyBinary (specInject [Range (nat @startM) (nat @endM) (nat @stepM), At (nat @j)])

instance
  ( KnownNat m,
    KnownNat n,
    (KnownNat i, i < m),
    (KnownNat startN, KnownNat endN, KnownNat stepN),
    (startN < n, endN < n, 0 < stepN),
    resN ~ Size startN endN stepN n
  ) =>
  ProjectInjectOp (Proxy i, Proxy '(startN, endN, stepN)) (Expression (D2 m n) et) (Expression (D1 resN) et)
  where
  project _ = applyUnary (specProject [At (nat @i), Range (nat @startN) (nat @endN) (nat @stepN)])
  inject _ = applyBinary (specInject [At (nat @i), Range (nat @startN) (nat @endN) (nat @stepN)])

instance
  ( KnownNat m,
    KnownNat n,
    (KnownNat startM, KnownNat endM, KnownNat stepM),
    (startM < m, endM < m, 0 < stepM),
    (KnownNat startN, KnownNat endN, KnownNat stepN),
    (startN < n, endN < n, 0 < stepN),
    resM ~ Size startM endM stepM m,
    resN ~ Size startN endN stepN n
  ) =>
  ProjectInjectOp (Proxy '(startM, endM, stepM), Proxy '(startN, endN, stepN)) (Expression (D2 m n) et) (Expression (D2 resM resN) et)
  where
  project _ = applyUnary (specProject [Range (nat @startM) (nat @endM) (nat @stepM), Range (nat @startN) (nat @endN) (nat @stepN)])
  inject _ = applyBinary (specInject [Range (nat @startM) (nat @endM) (nat @stepM), Range (nat @startN) (nat @endN) (nat @stepN)])

-- TODO: 3D


-------------------------------------------------------------------------------

instance (KnownNat m, KnownNat n, KnownNat p) =>
  MatrixMulOp (Expression (D2 m n) et) (Expression (D2 n p) et) (Expression (D2 m p) et) where
  (**) = applyBinary specMatMul

instance (KnownNat m, KnownNat n) =>
  MatrixMulOp (Expression (D2 m n) et) (Expression (D1 n) et) (Expression (D1 m) et) where
  (**) = applyBinary specMatMul

instance (KnownNat m, KnownNat n) => 
  TransposeOp (Expression (D2 m n) et) (Expression (D2 n m) et) where 
  transpose = applyUnary specTranspose

instance (KnownNat m) => 
  TransposeOp (Expression (D1 m) et) (Expression (D2 1 m) et) where 
  transpose = applyUnary specTranspose
-------------------------------------------------------------------------------

at :: forall i. (KnownNat i) => Proxy i
at = Proxy

range :: forall start end. (KnownNat start, KnownNat end) => Proxy '(start, end, 1)
range = Proxy

ranges :: forall start end step. (KnownNat start, KnownNat end, KnownNat step) => Proxy '(start, end, step)
ranges = Proxy

---- | Create primitive expressions
variable :: String -> Expression Scalar R
variable name = fromNode ([], R, Var name)

-- | Create primitive expressions using Nat kind.
--
-- @
--   let exp = variable1D "var"
--   let exp = variableD \@10 "var"
-- @
variable1D ::
  forall n.
  (KnownNat n) =>
  String ->
  Expression (D1 n) R
variable1D name = fromNode ([nat @n], R, Var name)

-- | Create a variable for two-dimensional nat values
-- @
--  exp = variable2D "var"
--  exp = variable2D \@10 \@20 "var"
-- @
variable2D :: forall m n. (KnownNat m, KnownNat n) => String -> Expression (D2 m n) R
variable2D name = fromNode ([nat @m, nat @n], R, Var name)

-- | Create a variable for three-dimensional nat values
-- @
--  exp = variable3D "var"
--  exp = variable3D @10 @20 @30 "var"
-- @
variable3D :: forall m n p. (KnownNat m, KnownNat n, KnownNat p) => String -> Expression (D3 m n p) R
variable3D name = fromNode ([nat @m, nat @n, nat @p], R, Var name)

-- | create a scalar (non-vector) constant Expression
constant :: Double -> Expression Scalar R
constant val = fromNode ([], R, Const val)

-- | Declare a one-dimensional constant
-- @
--  constant2D @1 40
-- @
constant1D :: forall n. (KnownNat n) => Double -> Expression (D1 n) R
constant1D val = fromNode ([nat @n], R, Const val)

-- | Two-dimensional constant
-- @
--  constant2D @1 @2 40
-- @
constant2D :: forall m n. (KnownNat m, KnownNat n) => Double -> Expression (D2 m n) R
constant2D val = fromNode ([nat @m, nat @n], R, Const val)

-- | Three-dimensional constant
-- @
--  constant2D @1 @2 @3 40
-- @
constant3D :: forall m n p. (KnownNat m, KnownNat n, KnownNat p) => Double -> Expression (D3 m n p) R
constant3D val = fromNode ([nat @m, nat @n, nat @p], R, Const val)

-- | Create parameter
param :: String -> Expression Scalar R
param name = fromNode ([], R, Param name)

-- | Create a param for one-dimensional nat values
--
-- @
--   let exp = param1D "var"
--   let exp = param1D \@10 "var"
-- @
param1D :: forall n. (KnownNat n) => String -> Expression (D1 n) R
param1D name = fromNode ([nat @n], R, Param name)

-- | Create a param for two-dimensional nat values
-- @
--  exp = param2D "var"
--  exp = param2D \@10 \@20 "var"
-- @
param2D :: forall m n. (KnownNat m, KnownNat n) => String -> Expression (D2 m n) R
param2D name = fromNode ([nat @m, nat @n], R, Param name)

-- | Create a param for three-dimensional nat values
-- @
--  exp = param3D "var"
--  exp = param3D @10 @20 @30 "var"
-- @
param3D :: forall m n p. (KnownNat m, KnownNat n, KnownNat p) => String -> Expression (D3 m n p) R
param3D name = fromNode ([nat @m, nat @n, nat @p], R, Param name)
