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
module HashedExpression.Internal.Expression where

import Data.Array
import qualified Data.Complex as DC
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import GHC.Stack (HasCallStack)
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
type Node = (Shape, ElementType, Op)

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
data Expression (d :: [Nat]) (et :: ElementType) = Expression
  { -- | index to the topological root of ExpressionMap
    exRootID :: Int,
    -- | Map of all 'Node' indexable by 'NodeID'
    exMap :: ExpressionMap
  }
  deriving (Show, Eq, Ord, Typeable)

type role Expression nominal nominal

-- | The Op type provides constructors for variables, constants and operators used to create expressions.
data Op
  = -- | variable with an identifier
    Var String
  | -- | parameter with an identifier
    Param String
  | -- | constants, only wrapped byf @Expression d R@
    Const Double
  | -- | element-wise sum
    Sum Args
  | -- | multiply, overloaded via 'Dimension'
    Mul Args
  | -- | power to, overloaded via 'PowerOp'
    Power Int Arg
  | -- | negation, wrapped byf @Expression d R@ or @Expression d C@
    Neg Arg
  | -- | scaling, overloaded via 'ScaleOp'
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
  | FT Arg
  | IFT Arg
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
type CovectorArg = NodeID

type SubArg = NodeID

type BaseArg = NodeID

-- |
type Position = [Int]

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
