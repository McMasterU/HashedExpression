{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  HashedExpression.Internal.TypedExpr
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- The @TypedExpr@ data type is the core data structure of the HashedExpresion library. This module contains all necessary definitions for
-- constructing the TypedExpr type.
module HashedExpression.Modeling.Typed where

import Data.Proxy
import GHC.Stack (HasCallStack)
import GHC.TypeLits (CmpNat, Div, KnownNat, Mod, Nat, natVal, type (+), type (-))
import HashedExpression.Internal.Base
import HashedExpression.Internal.Builder
import HashedExpression.Internal.MonadExpression
import Prelude hiding ((**), (^))

-- |
newtype TypedExpr (d :: [Nat]) (et :: ElementType) = TypedExpr {extractBuilder :: ExprBuilder}
  deriving (Show, Eq, Ord)

type role TypedExpr nominal nominal

-- --------------------------------------------------------------------------------------------------------------------

-- * TypedExpr Element Types

-- --------------------------------------------------------------------------------------------------------------------

type R = 'R

type C = 'C

class IsElementType (d :: ElementType) where
  toElementType :: ElementType

instance IsElementType R where
  toElementType = R

instance IsElementType C where
  toElementType = C

-- --------------------------------------------------------------------------------------------------------------------

-- * TypedExpr shape

-- --------------------------------------------------------------------------------------------------------------------

-- | Type-level encoding of shapes
type Scalar = '[]

class ToShape d where
  toShape :: Shape

instance ToShape '[] where
  toShape = []

instance (KnownNat x, ToShape xs) => ToShape (x ': xs) where
  toShape = (nat @x) : toShape @xs

-- | Helper function, wrapper over 'natVal' from 'GHC.TypeLits' that automaticaly converts resulting value
--   from Integer to Int
nat :: forall n. (KnownNat n) => Int
nat = fromIntegral $ natVal (Proxy :: Proxy n)

-----------------------------------------------------------------------------------------------------------------------

instance IsExpression (TypedExpr d et) where
  asRawExpr = buildExpr . extractBuilder

instance IsScalarReal (TypedExpr Scalar R) where
  asScalarRealRawExpr = buildExpr . extractBuilder

-----------------------------------------------------------------------------------------------------------------------
unary :: (ExprBuilder -> ExprBuilder) -> TypedExpr d1 et1 -> TypedExpr d2 et2
unary f (TypedExpr e) = TypedExpr $ f e

binary :: (ExprBuilder -> ExprBuilder -> ExprBuilder) -> TypedExpr d1 et1 -> TypedExpr d2 et2 -> TypedExpr d3 et3
binary f (TypedExpr e1) (TypedExpr e2) = TypedExpr $ f e1 e2

fromDouble :: forall d. ToShape d => Double -> TypedExpr d R
fromDouble value = TypedExpr $ introduceNode (toShape @d, R, Const value)

instance (ToShape d) => PowerOp (TypedExpr d et) Int where
  (^) :: TypedExpr d et -> Int -> TypedExpr d et
  (^) e x = unary (^ x) e

-- | Basic operations on Num class
instance ToShape d => Num (TypedExpr d R) where
  (+) = binary (+)
  (-) = binary (-)
  (*) = binary (*)
  negate = unary negate
  fromInteger val = fromDouble $ fromIntegral val
  abs = error "TODO"
  signum = error "Not applicable"

-- | Basic operations on Fractional class
instance ToShape d => Fractional (TypedExpr d R) where
  (/) = binary (/)
  fromRational r = fromDouble $ fromRational r

-- | Basic operations on Floating class
instance ToShape d => Floating (TypedExpr d R) where
  pi = fromDouble pi
  sqrt = unary sqrt
  exp = unary exp
  log = unary log
  sin = unary sin
  cos = unary cos
  tan = unary tan
  asin = unary asin
  acos = unary acos
  atan = unary atan
  sinh = unary sinh
  cosh = unary cosh
  tanh = unary tanh
  asinh = unary asinh
  acosh = unary acosh
  atanh = unary atanh

-- | Basic operations on class Num
instance ToShape d => Num (TypedExpr d C) where
  (+) = binary (+)
  (*) = binary (*)
  negate = unary negate
  fromInteger val = fromIntegral val +: 0
  abs = error "TODO: abs"
  signum = error "Not applicable"

-- | Basic operations on class Fractional
instance ToShape d => Fractional (TypedExpr d C) where
  (/) = binary (/)
  fromRational r = fromDouble (fromRational r) +: 0

-- | Scale in vector space
instance ScaleOp (TypedExpr Scalar R) (TypedExpr d et) where
  scale :: TypedExpr Scalar s -> TypedExpr d et -> TypedExpr d et
  scale = binary scale

instance ScaleOp (TypedExpr Scalar C) (TypedExpr d C) where
  scale :: TypedExpr Scalar s -> TypedExpr d et -> TypedExpr d et
  scale = binary scale

instance (ToShape d) => ComplexRealOp (TypedExpr d R) (TypedExpr d C) where
  (+:) :: TypedExpr d R -> TypedExpr d R -> TypedExpr d C
  (+:) = binary (+:)
  xRe :: TypedExpr d C -> TypedExpr d R
  xRe = unary xRe
  xIm :: TypedExpr d C -> TypedExpr d R
  xIm = unary xIm
  conjugate :: TypedExpr d C -> TypedExpr d C
  conjugate = unary conjugate

instance InnerProductSpaceOp (TypedExpr d et) (TypedExpr Scalar et) where
  (<.>) :: TypedExpr d s -> TypedExpr d et -> TypedExpr Scalar et
  (<.>) = binary (<.>)

-- | Huber loss: https://en.wikipedia.org/wiki/Huber_loss.
-- Piecewise loss function where the loss algorithm chosen depends on delta
huber :: forall d. (ToShape d) => Double -> TypedExpr d R -> TypedExpr d R
huber delta e = piecewise [- delta, delta] e [outerLeft, inner, outerRight]
  where
    inner = constant 0.5 *. (e * e)
    outerLeft = constant (- delta) *. e - fromDouble (delta * delta / 2)
    outerRight = constant delta *. e - fromDouble (delta * delta / 2)

-- | Norm 2 uses inner product space
norm2 :: (ToShape d) => TypedExpr d R -> TypedExpr Scalar R
norm2 expr = sqrt (expr <.> expr)

---- | Norm 1
norm1 :: (ToShape d) => TypedExpr d R -> TypedExpr Scalar R
norm1 expr = sumElements (sqrt (expr * expr))

-- | Norm 2 square interface
class Norm2SquareOp a b | a -> b where
  norm2square :: a -> b

-- | Norm 2 square of real expression
instance (ToShape d) => Norm2SquareOp (TypedExpr d R) (TypedExpr Scalar R) where
  norm2square :: TypedExpr d R -> TypedExpr Scalar R
  norm2square exp = exp <.> exp

-- | Norm 2 square of complex expression
instance (ToShape d) => Norm2SquareOp (TypedExpr d C) (TypedExpr Scalar R) where
  norm2square :: TypedExpr d C -> TypedExpr Scalar R
  norm2square exp = (xRe exp <.> xRe exp) + (xIm exp <.> xIm exp)

-- | Outlier-sensitive error measure using huber loss
huberNorm :: (ToShape d) => Double -> TypedExpr d R -> TypedExpr Scalar R
huberNorm alpha = sumElements . huber alpha

-- | Sum elements
sumElements :: forall d. (ToShape d) => TypedExpr d R -> TypedExpr Scalar R
sumElements expr = expr <.> 1

-- | Piecewise, with a condition expression and branch expressions
-- This is element corresponding, so condition and all branches should have the same dimension and shape
instance (ToShape d) => PiecewiseOp (TypedExpr d R) (TypedExpr d et) where
  piecewise :: HasCallStack => [Double] -> TypedExpr d R -> [TypedExpr d et] -> TypedExpr d et
  piecewise marks conditionExp branchExps =
    TypedExpr $ piecewise marks (extractBuilder conditionExp) (map extractBuilder branchExps)

-- Fourier transform on complex expression
instance (ToShape d) => FTOp (TypedExpr d C) (TypedExpr d C) where
  ft :: TypedExpr d C -> TypedExpr d C
  ft = unary ft

  ift :: TypedExpr d C -> TypedExpr d C
  ift = unary ift

-- |
instance (KnownNat n) => RotateOp Int (TypedExpr '[n] et) where
  rotate :: Int -> TypedExpr '[n] et -> TypedExpr '[n] et
  rotate x = unary (rotate [x])

instance (KnownNat m, KnownNat n) => RotateOp (Int, Int) (TypedExpr '[m, n] et) where
  rotate :: (Int, Int) -> TypedExpr '[m, n] et -> TypedExpr '[m, n] et
  rotate (x, y) = unary (rotate [x, y])

instance (KnownNat m, KnownNat n, KnownNat p) => RotateOp (Int, Int, Int) (TypedExpr '[m, n, p] et) where
  rotate :: (Int, Int, Int) -> TypedExpr '[m, n, p] et -> TypedExpr '[m, n, p] et
  rotate (x, y, z) = unary (rotate [x, y, z])

-------------------------------------------------------------------------------
type x < y = (CmpNat x y ~ 'LT)

type Size start end step n = (((n + end - start) `Mod` n) `Div` step + 1)

type Injectable start end sub base =
  ( (KnownNat start, KnownNat end),
    KnownNat sub,
    KnownNat base,
    (start < base, end < base),
    sub ~ Size start end 1 base
  )

-------------------------------------------------------------------------------
instance
  ( KnownNat i,
    KnownNat n,
    i < n
  ) =>
  ProjectInjectOp (Proxy i) (TypedExpr '[n] et) (TypedExpr Scalar et)
  where
  project _ = unary (project [At $ nat @i])
  inject _ = binary (inject [At $ nat @i])

instance
  ( (KnownNat start, KnownNat end, KnownNat step),
    KnownNat n,
    (start < n, end < n, 0 < step),
    res ~ Size start end step n
  ) =>
  ProjectInjectOp (Proxy '(start, end, step)) (TypedExpr '[n] et) (TypedExpr '[res] et)
  where
  project _ = unary (project [Range (nat @start) (nat @end) (nat @step)])
  inject _ = binary (inject [Range (nat @start) (nat @end) (nat @step)])

instance
  ( KnownNat m,
    KnownNat n,
    (KnownNat i, i < m),
    (KnownNat j, j < n)
  ) =>
  ProjectInjectOp (Proxy i, Proxy j) (TypedExpr '[m, n] et) (TypedExpr Scalar et)
  where
  project _ = unary (project [At (nat @i), At (nat @j)])
  inject _ = binary (inject [At (nat @i), At (nat @j)])

instance
  ( KnownNat m,
    KnownNat n,
    (KnownNat j, j < n),
    (KnownNat startM, KnownNat endM, KnownNat stepM),
    (startM < m, endM < m, 0 < stepM),
    resM ~ Size startM endM stepM m
  ) =>
  ProjectInjectOp (Proxy '(startM, endM, stepM), Proxy j) (TypedExpr '[m, n] et) (TypedExpr '[resM] et)
  where
  project _ = unary (project [Range (nat @startM) (nat @endM) (nat @stepM), At (nat @j)])
  inject _ = binary (inject [Range (nat @startM) (nat @endM) (nat @stepM), At (nat @j)])

instance
  ( KnownNat m,
    KnownNat n,
    (KnownNat i, i < m),
    (KnownNat startN, KnownNat endN, KnownNat stepN),
    (startN < n, endN < n, 0 < stepN),
    resN ~ Size startN endN stepN n
  ) =>
  ProjectInjectOp (Proxy i, Proxy '(startN, endN, stepN)) (TypedExpr '[m, n] et) (TypedExpr '[resN] et)
  where
  project _ = unary (project [At (nat @i), Range (nat @startN) (nat @endN) (nat @stepN)])
  inject _ = binary (inject [At (nat @i), Range (nat @startN) (nat @endN) (nat @stepN)])

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
  ProjectInjectOp (Proxy '(startM, endM, stepM), Proxy '(startN, endN, stepN)) (TypedExpr '[m, n] et) (TypedExpr '[resM, resN] et)
  where
  project _ = unary (project [Range (nat @startM) (nat @endM) (nat @stepM), Range (nat @startN) (nat @endN) (nat @stepN)])
  inject _ = binary (inject [Range (nat @startM) (nat @endM) (nat @stepM), Range (nat @startN) (nat @endN) (nat @stepN)])

-- TODO: 3D

-------------------------------------------------------------------------------

instance
  (KnownNat m, KnownNat n, KnownNat p) =>
  MatrixMulOp (TypedExpr '[m, n] et) (TypedExpr '[n, p] et) (TypedExpr '[m, p] et)
  where
  (**) = binary (**)

instance
  (KnownNat m, KnownNat n) =>
  MatrixMulOp (TypedExpr '[m, n] et) (TypedExpr '[n] et) (TypedExpr '[m] et)
  where
  (**) = binary (**)

instance
  (KnownNat m, KnownNat n) =>
  TransposeOp (TypedExpr '[m, n] et) (TypedExpr '[n, m] et)
  where
  transpose = unary transpose

-------------------------------------------------------------------------------

at :: forall i. (KnownNat i) => Proxy i
at = Proxy

range :: forall start end. (KnownNat start, KnownNat end) => Proxy '(start, end, 1)
range = Proxy

ranges :: forall start end step. (KnownNat start, KnownNat end, KnownNat step) => Proxy '(start, end, step)
ranges = Proxy

-------------------------------------------------------------------------------

-- | General version of creating variables, parameters, constants
gvariable :: forall d. ToShape d => String -> TypedExpr d R
gvariable name = TypedExpr $ introduceNode (toShape @d, R, Var name)

gparam :: forall d. ToShape d => String -> TypedExpr d R
gparam name = TypedExpr $ introduceNode (toShape @d, R, Param name)

gconstant :: forall d. ToShape d => Double -> TypedExpr d R
gconstant value = TypedExpr $ introduceNode (toShape @d, R, Const value)

---- | Auxiliary for creating variables
--
-- @
--   let x = variable "x"
--   let x = variable1D \@10 "x"
--   let x = variable2D \@10 \@20 "x"
--   let x = variable3D \@10 \@20 \@30 "x"
-- @
variable :: String -> TypedExpr Scalar R
variable = gvariable @Scalar

variable1D :: forall n. (KnownNat n) => String -> TypedExpr '[n] R
variable1D = gvariable @'[n]

variable2D :: forall m n. (KnownNat m, KnownNat n) => String -> TypedExpr '[m, n] R
variable2D = gvariable @'[m, n]

variable3D :: forall m n p. (KnownNat m, KnownNat n, KnownNat p) => String -> TypedExpr '[m, n, p] R
variable3D = gvariable @'[m, n, p]

--
-- @
--   let x = constant 0.12
--   let x = constant1D \@10 0.12
--   let x = constant2D \@10 \@20 0.12
--   let x = constant3D \@10 \@20 \@30 0.12
-- @
constant :: Double -> TypedExpr Scalar R
constant = gconstant @Scalar

constant1D :: forall n. (KnownNat n) => Double -> TypedExpr '[n] R
constant1D = gconstant @'[n]

constant2D :: forall m n. (KnownNat m, KnownNat n) => Double -> TypedExpr '[m, n] R
constant2D = gconstant @'[m, n]

constant3D :: forall m n p. (KnownNat m, KnownNat n, KnownNat p) => Double -> TypedExpr '[m, n, p] R
constant3D = gconstant @'[m, n, p]

---- | Auxiliary for creating parameters
--
-- @
--   let x = param "x"
--   let x = param1D \@10 "x"
--   let x = param2D \@10 \@20 "x"
--   let x = param3D \@10 \@20 \@30 "x"
-- @
param :: String -> TypedExpr Scalar R
param = gparam @Scalar

param1D :: forall n. (KnownNat n) => String -> TypedExpr '[n] R
param1D = gparam @'[n]

param2D :: forall m n. (KnownNat m, KnownNat n) => String -> TypedExpr '[m, n] R
param2D = gparam @'[m, n]

param3D :: forall m n p. (KnownNat m, KnownNat n, KnownNat p) => String -> TypedExpr '[m, n, p] R
param3D = gparam @'[m, n, p]
