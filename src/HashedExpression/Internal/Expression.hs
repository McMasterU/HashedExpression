{-# LANGUAGE ConstraintKinds #-}

{-|
Module      :  HashedExpression.Internal.Expression
Copyright   :  (c) OCA 2020 
License     :  MIT (see the LICENSE file)
Maintainer  :  anandc@mcmaster.ca
Stability   :  provisional
Portability :  unportable

The @Expression@ data type is the core data structure of the HashedExpresion library.
This module contains all necessary definitions for constructing the Expression type.
-}

module HashedExpression.Internal.Expression
  ( -- * Expression Type
    -- | The Expression data structure is essentially a collection of 'Node', where
    --   each 'Node' is either an atomic value like variables and constants or
    --   an operator. Each 'Node' is given a 'NodeID' via a generated hash value,
    --   assuring reuse of like subexpressions
    Node (..),
    Internal,
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
    -- | The following cLasses define Expression combinators that can be overloaded
    --   to directly support a variety of functionality; such as interpretation, pattern matching,
    --   differentiation, etc
    PowerOp (..),
    PiecewiseOp (..),
    VectorSpaceOp (..),
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
import Data.Typeable (Typeable, typeRep)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Prelude hiding ((^))

-- --------------------------------------------------------------------------------------------------------------------
-- * Expression Type
-- --------------------------------------------------------------------------------------------------------------------

type Internal = (Shape, Node)

-- | Hash map of all subexpressions
type ExpressionMap = IntMap Internal

-- | The index/key to look for the node on the hash table
type NodeID = Int

-- | Expression with 2 phantom types (dimension and num type)
data Expression d et
  = Expression
      { exRootID :: Int, -- the index this expression
        exMap :: ExpressionMap -- all subexpressions
      }
  deriving (Show, Eq, Ord, Typeable)

-- | A type role implemented so users cannot use coerce to convert between expression types
type role Expression nominal nominal

-- | Node type
data Node
  = Var String
  | DVar String -- ^ only contained in **Expression d Covector (1-form)**
  | Const Double -- ^ only all elements the same
      -- MARK: Basics
  | Sum ET Args -- element-wise sum
  | Mul ET Args -- multiply --> have different meanings (scale in vector space, multiplication, ...)
  | Power Int Arg
  | Neg ET Arg
  | Scale ET Arg Arg
  | -- MARK: only apply to R
    Div Arg Arg -- TODO: Delete?
  | Sqrt Arg
  | Sin Arg
  | Cos Arg
  | Tan Arg
  | Exp Arg
  | Log Arg
  | Sinh Arg
  | Cosh Arg
  | Tanh Arg
  | Asin Arg
  | Acos Arg
  | Atan Arg
  | Asinh Arg
  | Acosh Arg
  | Atanh Arg
  | -- MARK: Complex related
    RealImag Arg Arg -- from real and imagine
  | RealPart Arg -- extract real part
  | ImagPart Arg -- extract imaginary part
      -- MARK: Inner product Space
  | InnerProd ET Arg Arg
  | -- MARK: Piecewise
    Piecewise [Double] ConditionArg [BranchArg]
  | Rotate RotateAmount Arg
  | -- MARK: Discrete Fourier Transform
    ReFT Arg
  | ImFT Arg
  | -- Need these inside because taking real of FT twice can be very fast
    TwiceReFT Arg
  | TwiceImFT Arg
  deriving (Show, Eq, Ord)

-- | When a 'Node' type references a subexpression (i.e because the Node is an operator)
--   it references the subexpression by 'NodeID'
type Arg = NodeID

type Args = [NodeID]

type ConditionArg = NodeID

type BranchArg = NodeID


-- --------------------------------------------------------------------------------------------------------------------
-- * Expression Element Types
-- --------------------------------------------------------------------------------------------------------------------

-- | Data representation of element type
data ET
  = R
  | C
  | Covector
  deriving (Show, Eq, Ord)

-- | Type representation of ElementType for Real values
--   HashedExpression values are either 'R', 'C' or a 'Covector'
data R
  deriving (NumType, ElementType, Typeable)

-- | Type representation of ElementType for Complex values
--   HashedExpression values are either 'R', 'C' or a 'Covector'
data C
  deriving (NumType, ElementType, Typeable)

-- | Type representation of ElementType for Covector values
--   HashedExpression values are either 'R', 'C' or a 'Covector'
data Covector
  deriving (ElementType, Typeable)


-- | Classes as constraints
class ElementType et

class ElementType et => NumType et

-- --------------------------------------------------------------------------------------------------------------------
-- * Expression Dimensions
-- --------------------------------------------------------------------------------------------------------------------

type DimensionType d = (Dimension d, ToShape d)

class Dimension d

-- | Dummy type (no data) for the zero dimension vectors (i.e not a vector)
--   When specifying vector dimensions for type class instances, in general
--   use either 'Scalar' or an instance of 'KnownNat'
data Scalar
  deriving (Dimension, Typeable)

instance (KnownNat n) => Dimension n

instance (KnownNat m, KnownNat n) => Dimension '(m, n)

instance (KnownNat m, KnownNat n, KnownNat p) => Dimension '(m, n, p)

class VectorSpace d et s

class VectorSpace d s s => InnerProductSpace d s

instance (DimensionType d, ElementType et) => VectorSpace d et R

instance (DimensionType d) => VectorSpace d C C

instance VectorSpace d s s => InnerProductSpace d s

class (Dimension d) => ToShape d where
  toShape :: Proxy d -> Shape

instance ToShape Scalar where
  toShape _ = []

instance (KnownNat n) => ToShape n where
  toShape _ = [nat @n]

instance (KnownNat m, KnownNat n) => ToShape '(m, n) where
  toShape _ = [nat @m, nat @n]

instance (KnownNat m, KnownNat n, KnownNat p) => ToShape '(m, n, p) where
  toShape _ = [nat @m, nat @n, nat @p]

-- | Shape type:
-- []        --> scalar
-- [n]       --> 1D with size n
-- [n, m]    --> 2D with size n × m
-- [n, m, p] --> 3D with size n × m × p
type Shape = [Int]

-- | Rotation in each dimension.
-- | Property:  the length of this must match the dimension of the data
type RotateAmount = [Int]

nat :: forall n. (KnownNat n) => Int
nat = fromIntegral $ natVal (Proxy :: Proxy n)

-- --------------------------------------------------------------------------------------------------------------------
-- * Generic Combinators
-- --------------------------------------------------------------------------------------------------------------------

class PowerOp a b | a -> b where
  (^) :: a -> b -> a

class VectorSpaceOp a b where
  scale :: a -> b -> b
  (*.) :: a -> b -> b
  (*.) = scale

class ComplexRealOp r c | r -> c, c -> r where
  (+:) :: r -> r -> c
  xRe :: c -> r
  xIm :: c -> r

class InnerProductSpaceOp a b c | a b -> c where
  (<.>) :: a -> b -> c

class RotateOp k a | a -> k where
  rotate :: k -> a -> a

class PiecewiseOp a b where
  piecewise :: [Double] -> a -> [b] -> b

class FTOp a b | a -> b where
  ft :: a -> b

infixl 6 +:

infixl 8 *., `scale`, <.>

infixl 8 ^

