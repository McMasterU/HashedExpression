{-# LANGUAGE ConstraintKinds #-}

-- |
-- Module      :  HashedExpression.Internal.Expression
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- The @Expression@ data type is the core data structure of the HashedExpresion library. This module contains all necessary definitions for
-- constructing the Expression type.
module HashedExpression.Internal.Expression
  ( -- * Expression Type

    -- | The Expression data structure is collection of 'Node', where
    --   each 'Node' is either an atomic value like variables and constants or
    --   an operator. Each 'Node' is given a 'NodeID' via a generated hash value,
    --   assuring reuse of common subexpressions
    Op (..),
    Node,
    NodeID,
    ExpressionMap,
    Expression (..),
    Arg,
    Args,
    BranchArg,
    ConditionArg,

    -- * Expression Element Types

    -- | Each 'Node' in an 'Expression' is either an operator or an element. Elements
    --   can be numeric values (i.e real or complex values), or a covector object
    --   (used to perform exterior differentiation)
    ET (..),
    R,
    C,
    ElementType,
    NumType,
    Covector,

    -- * Expression Dimensions

    -- | The following types and classes are used to contrain and inquire about
    --   vector dimensions of Expressions
    Dimension,
    Scalar,
    ToShape (..),
    Shape,
    DimensionType,
    VectorSpace,
    InnerProductSpace,

    -- * Generic Combinators

    -- | The following classes define 'Expression' operators that can be overloaded to directly
    --   support a variety of functionality; such as interpretation, pattern matching, differentiation, etc
    PowerOp (..),
    PiecewiseOp (..),
    VectorSpaceOp (..),
    FTOp (..),
    ComplexRealOp (..),
    RotateOp (..),
    RotateAmount,
    InnerProductSpaceOp (..),
    MulCovectorOp (..),
    ScaleCovectorOp (..),
    CovectorScaleOp (..),
    InnerProductCovectorOp (..),
  )
where

import Data.Array
import qualified Data.Complex as DC
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Prelude hiding ((^))

-- --------------------------------------------------------------------------------------------------------------------

-- * Expression Type

-- --------------------------------------------------------------------------------------------------------------------

-- | The bulk of an 'Expression' is a collection of 'Node' (with their dimensions), in
--   a Map indexed 'NodeID' (a generated hash value)
type ExpressionMap = IntMap Node

-- | The internals of an 'Expression' are a collection of 'Op' with
--   their dimensions
type Node = (Shape, ET, Op)

-- | A hash value used to identify a 'Node' (in order to provide automatic subexpression reuse).
--   Used as the index/key to perform a lookup in 'ExpressionMap'
type NodeID = Int

-- | HashedExpression stores expressions as a collection of 'Node' indexed by a
--   hash value (i.e 'NodeID').
--   The type is parameterized by two phantom types, the expressions 'Dimension' and
--   'ElementType'
--
-- @
--    variable "x" :: Expression Scalar R
--    constant1D @10 1 :: Expression 10 R
-- @
data Expression d et = Expression
  { -- | index to the topological root of ExpressionMap
    exRootID :: Int,
    -- | Map of all 'Node' indexable by 'NodeID'
    exMap :: ExpressionMap
  }
  deriving (Show, Eq, Ord, Typeable)

type role Expression nominal nominal

-- | The Op type provides constructors for variables, constants and operators used to create expressions.
data Op
  = -- | variable with an identifier, wrapped by either @Expression d R@ or @Expression d C@
    Var String
  | -- | constants, only wrapped byf @Expression d R@, non-scalar constants repeat the same value
    Const Double
  | -- | element-wise sum
    Sum Args
  | -- | multiply, overloaded via 'Dimension'
    Mul Args
  | -- | power to, overloaded via 'PowerOp'
    Power Int Arg
  | -- | negation, wrapped byf @Expression d R@ or @Expression d C@
    Neg Arg
  | -- | scaling, overloaded via 'VectorSpaceOp'
    Scale Arg Arg
  | -- | division operator, wrapped by @Expression d R@
    Div Arg Arg
  | -- | square root operator, wrapped by @Expression d R@
    Sqrt Arg
  | -- | sin operator, wrapped by @Expression d R@
    Sin Arg
  | -- | cos operator, wrapped by @Expression d R@
    Cos Arg
  | -- | tan operator, wrapped by @Expression d R@
    Tan Arg
  | -- | exp operator, wrapped by @Expression d R@
    Exp Arg
  | -- | log operator, wrapped by @Expression d R@
    Log Arg
  | -- | sinh operator, wrapped by @Expression d R@
    Sinh Arg
  | -- | cosh operator, wrapped by @Expression d R@
    Cosh Arg
  | -- | tanh operator, wrapped by @Expression d R@
    Tanh Arg
  | -- | asin operator, wrapped by @Expression d R@
    Asin Arg
  | -- | acos operator, wrapped by @Expression d R@
    Acos Arg
  | -- | atan operator, wrapped by @Expression d R@
    Atan Arg
  | -- | asinh operator, wrapped by @Expression d R@
    Asinh Arg
  | -- | acosh operator, wrapped by @Expression d R@
    Acosh Arg
  | -- | atanh operator, wrapped by @Expression d R@
    Atanh Arg
  | -- | construct a complex value from real and imagine parts, respectively, wrapped by @Expression d C@
    RealImag Arg Arg
  | -- | extract real from complex (transforms @Expression d C@ to @Expression d R@)
    RealPart Arg
  | -- | extract imaginary from complex (transforms @Expression d C@ to @Expression d R@)
    ImagPart Arg
  | -- | conjugate a complex expression
    Conjugate Arg
  | -- | inner product operator, overload via 'InnerProductSpace'
    InnerProd Arg Arg
  | -- | piecewise function, overload via 'PiecewiseOp'. Evaluates 'ConditionArg' to select 'BranchArg'
    Piecewise [Double] ConditionArg [BranchArg]
  | -- | rotate transformation, rotates vector elements by 'RotateAmount'
    Rotate RotateAmount Arg
  | -- | real part of fourier transform
    ReFT Arg
  | -- | imag part of fourier transform
    ImFT Arg
  | -- | real part of fourier transform, performed twice
    TwiceReFT Arg
  | -- | imag part of fourier transform, performed twice
    TwiceImFT Arg
  | -- MARK: differential

    -- | differentiable operator (such as dx), only wrapped by @Expression d Covector@ (1-form)
    DVar String
  | DZero
  | MulD Arg CovectorArg
  | ScaleD Arg CovectorArg
  | DScale CovectorArg Arg
  | InnerProdD Arg CovectorArg
  deriving (Show, Eq, Ord)

-- | Used by operators in the 'Node' type to reference another subexpression (i.e another 'Node')
type Arg = NodeID

-- | Used by operators in the 'Node' type to reference other subexpressions (i.e other 'Node')
type Args = [NodeID]

-- | A 'Node' used by 'Piecewise' functions to select a 'BranchArg' function based on what partition
--   it's evaluation is contained in
type ConditionArg = NodeID

-- | 'Piecewise' functions select from different 'BranchArg' expressions based on the evaluation
--   of a 'ConditionArg' expression
type BranchArg = NodeID

type CovectorArg = NodeID

-- --------------------------------------------------------------------------------------------------------------------

-- * Expression Element Types

-- --------------------------------------------------------------------------------------------------------------------

-- | Data representation of 'ElementType'. Used to represent types of elements in an 'Expression'
--   Represents Real, Complex, and Covector values with R, C, Covector respectively
data ET
  = -- | Real Elements
    R
  | -- | Complex Elements
    C
  | -- | Covector (contains 'DVar' operator) Elements
    Covector
  deriving (Show, Eq, Ord)

-- | Type representation of 'ElementType' for Real values
--   HashedExpression values are either 'R', 'C' or a 'Covector'
data R
  deriving (NumType, ElementType, Typeable)

-- | Type representation of 'ElementType' for Complex values
--   HashedExpression values are either 'R', 'C' or a 'Covector'
data C
  deriving (NumType, ElementType, Typeable)

-- | Type representation of 'ElementType' for Covector values
--   HashedExpression values are either 'R', 'C' or a 'Covector'
data Covector
  deriving (ElementType, Typeable)

-- | Class used to constrain 'Expression' operations by the type of element
--   (i.e Real, Complex or Covector). See 'ET' for the corresponding data representation.
class ElementType et

-- | Class used to constrain 'Expression' operations by the type of element.
--   Should be implemented by only numeric 'ElementType' (like 'R' and 'C')
class ElementType et => NumType et

-- --------------------------------------------------------------------------------------------------------------------

-- * Expression Dimensions

-- --------------------------------------------------------------------------------------------------------------------

-- | An 'Expression' type is parameterized by two phantom types, the first of which should be 'DimensionType'.
--   Using the 'Dimension' and 'ToShape' classes provides the dimensions of an expression at the type and data level (respectively).
type DimensionType d = (Dimension d, ToShape d)

-- | Use to constrain 'Expression' dimensions at the type level. The size of each dimension in a vector can be specified
--   using a 'KnownNat', for vectors of n-dimensions use an n-sized tuple
class Dimension d

-- | Dummy type (no data) for the zero dimension vectors (i.e not a vector)
--   When specifying vector dimensions for type class instances, in general
--   use either 'Scalar' or an instance of 'KnownNat'
--
--   @
--   variable "x" :: Expression Scalar R
--   @
data Scalar
  deriving (Dimension, Typeable)

-- | Dimension encoding for a 1D Vector (use 'KnownNat' to specify size of Vector)
--
--   @
--   variable1D  "x" :: KnownNat n => Expression n R
--   variable1D @10 "x" :: Expression 10 R
--   @
--
--   With this instance, the above definition will satisfy the constraint
--   @Dimension d => Expression d R@
instance (KnownNat n) => Dimension n

-- | Dimension encoding for a 2D Vector (use 'KnownNat' to specify size of Vector)
--
--   @
--   variable2D  "x" :: (KnownNat n, KnownNat m) => Expression '(n,m) R
--   variable2D @10 @5 "x" :: Expression '(10,5) R
--   @
--
--   With this instance, the above definition will satisfy the constraint
--   @Dimension d => Expression d R@
instance (KnownNat m, KnownNat n) => Dimension '(m, n)

-- | Dimension encoding for a 3D Vector (use 'KnownNat' to specify size of Vector)
--
--   @
--   variable3D  "x" :: (KnownNat n, KnownNat m, KnownNat p) => Expression '(n,m,p) R
--   variable3D @10 @5 @5 "x" :: Expression '(10,5,5) R
--   @
--
--   With this instance, the above definition will satisfy the constraint
--   @Dimension d => Expression d R@
instance (KnownNat m, KnownNat n, KnownNat p) => Dimension '(m, n, p)

-- | A @VectorSpace d et s@ is a space of vectors of @Dimension d@ and @ElementType et@,
--   that can be scaled by values of @ElementType s@. Used primarily as a base for 'VectorSpaceOp'
class VectorSpace d et s

-- | A 'VectorSpace' exists for all 'DimensionType' and 'ElementType' if scaled by a 'R' (Real element type)
instance (DimensionType d, NumType et) => VectorSpace d et R

-- | A 'VectorSpace' exists for all 'DimensionType' over 'C' (Complex) vectors and scalings
instance (DimensionType d) => VectorSpace d C C

-- | Every @VectorSpace@ that can be scaled by the same ElementType has an @InnerProductSpace@.
--   Used primarily as a base for 'InnerProductSpaceOp'
class VectorSpace d s s => InnerProductSpace d s

-- | Every 'VectorSpace' with the same 'ElementType' for vectors and their scalings has a corresponding
--   'InnerProductSpace'
instance VectorSpace d s s => InnerProductSpace d s

-- | ToShape is used to reify dimensions/sizes encoded at the type level (i.e any implementation of 'Dimension')
--   The type level generally uses tuples with type level naturals (via 'KnownNat') to specify dimensions and
--   their respective sizes. The 'toShape' method reifies these types to a corresponding list of Int
class (Dimension d) => ToShape d where
  toShape :: Proxy d -> Shape

-- | A scalar essentially has no dimensions/size, so we provide an empty list
instance ToShape Scalar where
  toShape _ = []

-- | Implementation for a 1D Vector
instance (KnownNat n) => ToShape n where
  toShape _ = [nat @n]

-- | Implementation for a 2D Vector
instance (KnownNat m, KnownNat n) => ToShape '(m, n) where
  toShape _ = [nat @m, nat @n]

-- | Implementation for a 2D Vector
instance (KnownNat m, KnownNat n, KnownNat p) => ToShape '(m, n, p) where
  toShape _ = [nat @m, nat @n, nat @p]

-- | Helper function, wrapper over 'natVal' from 'GHC.TypeLits' that automaticaly converts resulting value
--   from Integer to Int
nat :: forall n. (KnownNat n) => Int
nat = fromIntegral $ natVal (Proxy :: Proxy n)

-- | A Shape encodes the the dimensions (via the length of a list) and size of elements.
--   It is the data level representation of the 'Dimension' class that's used to encode
--   dimensions/sizes of a 'Expression' at the type level. For example,
--
-- >  []        => 'Scalar'
-- >  [n]       => KnownNat n => Dimension n
-- >  [n, m]    => (KnownNat n, KnownNat m) => Dimension '(n,m)
type Shape = [Int]

-- | Rotation in each dimension, given respectively in a list.
--   The length of the list matches the number of dimensions
type RotateAmount = [Int]

-- --------------------------------------------------------------------------------------------------------------------

-- * Generic Combinators

-- --------------------------------------------------------------------------------------------------------------------

-- | Interface for power (i.e power to) combinator for constructing 'Expression' types. Can be overloaded
--   to support different functionality performed on 'Expresion' (such as evaluation, pattern matching, code generation)
class PowerOp a b | a -> b where
  (^) :: a -> b -> a

-- | Interface for scaling (i.e vector scaling) combinator for constructing 'Expression' types. Can be overloaded
--   to support different functionality performed on 'Expresion' (such as evaluation, pattern matching, code generation)
class VectorSpaceOp a b where
  scale :: a -> b -> b
  (*.) :: a -> b -> b
  (*.) = scale

-- | Interface for complex combinators for constructing 'Expression' types. Can be overloaded
--   to support different functionality performed on 'Expresion' (such as evaluation, pattern matching, code generation)
class ComplexRealOp r c | r -> c, c -> r where
  -- | construct complex data from real / imaginary parts
  (+:) :: r -> r -> c

  -- | extract real part from complex data
  xRe :: c -> r

  -- | extract imaginary part from complex data
  xIm :: c -> r

  -- conjugate
  conjugate :: c -> c

-- | Interface for Inner Product combinator for constructing 'Expression' types. Can be overloaded
--   to support different functionality performed on 'Expresion' (such as evaluation, pattern matching, code generation)
class InnerProductSpaceOp a b c | a b -> c where
  (<.>) :: a -> b -> c

-- | Interface for rotation combinator for constructing 'Expression' types. Can be overloaded
--   to support different functionality performed on 'Expresion' (such as evaluation, pattern matching, code generation)
class RotateOp k a | a -> k where
  rotate :: k -> a -> a

-- | Interface for rotation combinator for constructing 'Expression' types. Can be overloaded
--   to support different functionality performed on 'Expresion' (such as evaluation, pattern matching, code generation)
class PiecewiseOp a b where
  piecewise :: [Double] -> a -> [b] -> b

-- | Interface for fourier transform on 'Expression' types. Can be overloaded
--   to support different functionality performed on 'Expresion' (such as evaluation, pattern matching, code generation)
class FTOp a b | a -> b where
  ft :: a -> b

class MulCovectorOp a b c | a b -> c, c -> a, c -> b where
  (|*|) :: a -> b -> c

class ScaleCovectorOp a b c | a b -> c where
  (|*.|) :: a -> b -> c

class CovectorScaleOp a b c | a b -> c where
  (|.*|) :: a -> b -> c

class InnerProductCovectorOp a b c | a b -> c where
  (|<.>|) :: a -> b -> c

infixl 6 +:

infixl 8 *., `scale`, <.>

infixl 8 ^

infixl 7 |*|

infixl 8 |<.>|, |*.|
