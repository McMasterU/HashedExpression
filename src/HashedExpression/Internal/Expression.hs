{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

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
  ( nat,
    Op (..),
    Node,
    NodeID,
    DimSelector (..),
    ProjectInjectOp (..),
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
    ElementType (..),

    -- * Expression Dimensions

    -- | The following types and classes are used to contrain and inquire about
    --   vector dimensions of Expressions
    Dimension (..),
    Scalar,
    D1,
    D2,
    D3,
    Shape,
    -- | The following classes define 'Expression' operators that can be overloaded to directly
    --   support a variety of functionality; such as interpretation, pattern matching, differentiation, etc
    PowerOp (..),
    PiecewiseOp (..),
    ScaleOp (..),
    FTOp (..),
    ComplexRealOp (..),
    RotateOp (..),
    RotateAmount,
    InnerProductSpaceOp (..),
  )
where

import Data.Array
import qualified Data.Complex as DC
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Proxy (Proxy (..))
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownNat, Nat, natVal)
import HashedExpression.Internal.Base
import Prelude hiding ((^))

-- --------------------------------------------------------------------------------------------------------------------

-- * Expression Type

-- --------------------------------------------------------------------------------------------------------------------

-- | The bulk of an 'Expression' is a collection of 'Node' (with their dimensions), in
--   a Map indexed 'NodeID' (a generated hash value)
type ExpressionMap = IntMap Node

-- | The internals of an 'Expression' are a collection of 'Op' with
--   their dimensions
type Node = (Shape, ElementType, Op)

-- | HashedExpression stores expressions as a collection of 'Node' indexed by a
--   hash value (i.e 'NodeID').
--   The type is parameterized by two phantom types, the expressions 'Dimension' and
--   'ElementType'
--
-- @
--    variable "x" :: Expression Scalar R
--    constant1D @10 1 :: Expression 10 R
-- @
data Expression (d :: [Nat]) (et :: ElementType) = Expression
  { -- | index to the topological root of ExpressionMap
    exRootID :: NodeID,
    -- | Map of all 'Node' indexable by 'NodeID'
    exMap :: ExpressionMap
  }
  deriving (Show, Eq, Ord)

type role Expression nominal nominal

-- | The Op type provides constructors for variables, constants and operators used to create expressions.
data Op
  = -- | variable with an identifier
    Var String
  | -- | parameter with an identifier
    Param String
  | -- | constants
    Const Double
  | -- | element-wise sum
    Sum Args
  | -- | element-wise multiplication
    Mul Args
  | -- | power
    Power Int Arg
  | -- | negation
    Neg Arg
  | -- | scaling
    Scale Arg Arg
  | -- | division
    Div Arg Arg
  | -- | square root
    Sqrt Arg
  | -- | sin
    Sin Arg
  | -- | cos
    Cos Arg
  | -- | tan
    Tan Arg
  | -- | exp
    Exp Arg
  | -- | log
    Log Arg
  | -- | sinh
    Sinh Arg
  | -- | cosh
    Cosh Arg
  | -- | tanh
    Tanh Arg
  | -- | asin
    Asin Arg
  | -- | acos
    Acos Arg
  | -- | atan
    Atan Arg
  | -- | asinh
    Asinh Arg
  | -- | acosh
    Acosh Arg
  | -- | atanh
    Atanh Arg
  | -- | complex out of real part and imaginary part: a +: b
    RealImag Arg Arg
  | -- | extract real from complex
    RealPart Arg
  | -- | extract imaginary from complex
    ImagPart Arg
  | -- | conjugate a complex expression
    Conjugate Arg
  | -- | inner product operator
    InnerProd Arg Arg
  | -- | (element-wise) piecewise function, evaluate condition to select branch
    Piecewise [Double] ConditionArg [BranchArg]
  | -- | rotate
    Rotate RotateAmount Arg
  | -- | Fourier Transform
    FT Arg
  | -- | Inverse Fourier Transform
    IFT Arg
  | -- | Projection
    Project [DimSelector] Arg
  | -- | Injection
    Inject [DimSelector] SubArg BaseArg -- inject Arg into BaseArg
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

-- |
type SubArg = NodeID

type BaseArg = NodeID

-- | DimSelector for projection
data DimSelector
  = At Int -- Will collapse the corresponding dimension
  | Range -- (inclusion)
      Int -- start
      Int -- end
      Int -- step
  deriving (Show, Eq, Ord)

-- | Rotation in each dimension, given respectively in a list.
--   The length of the list matches the number of dimensions
type RotateAmount = [Int]

-- --------------------------------------------------------------------------------------------------------------------

-- * Expression Element Types

-- --------------------------------------------------------------------------------------------------------------------

-- | Data representation of 'ElementType'. Used to represent types of elements in an 'Expression'
--   Represents Real, Complex
data ElementType
  = -- | Real Elements
    R
  | -- | Complex Elements
    C
  deriving (Show, Eq, Ord)

type R = 'R

type C = 'C

-- --------------------------------------------------------------------------------------------------------------------

-- * Expression Dimensions

-- --------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------
type Scalar = '[]

type D1 (n :: Nat) = '[n]

type D2 (m :: Nat) (n :: Nat) = '[m, n]

type D3 (m :: Nat) (n :: Nat) (p :: Nat) = '[m, n, p]

instance Dimension '[] where
  toShape _ = []

--
instance (KnownNat x, Dimension xs) => Dimension (x ': xs) where
  toShape _ = (nat @x) : toShape (Proxy @xs)

--

-- | Use to constrain 'Expression' dimensions at the type level. The size of each dimension in a vector can be specified
--   using a 'KnownNat', for vectors of n-dimensions use an n-sized tuple
class Dimension d where
  toShape :: Proxy d -> Shape

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

-- --------------------------------------------------------------------------------------------------------------------

-- * Generic Combinators

-- --------------------------------------------------------------------------------------------------------------------

-- | Interface for power (i.e power to) combinator for constructing 'Expression' types. Can be overloaded
--   to support different functionality performed on 'Expresion' (such as evaluation, pattern matching, code generation)
class PowerOp a b | a -> b where
  (^) :: a -> b -> a

-- | Interface for scaling (i.e vector scaling) combinator for constructing 'Expression' types. Can be overloaded
--   to support different functionality performed on 'Expresion' (such as evaluation, pattern matching, code generation)
class ScaleOp a b where
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
class InnerProductSpaceOp a b | a -> b where
  (<.>) :: a -> a -> b

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
class FTOp a b | a -> b, b -> a where
  ft :: a -> b
  ift :: b -> a

-- |
class ProjectInjectOp s a b | s a -> b where
  project :: s -> a -> b
  inject :: s -> b -> a -> a

-------------------------------------------------------------------------------
infixl 6 +:

infixl 8 *., `scale`, <.>

infixl 8 ^
