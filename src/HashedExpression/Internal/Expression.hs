{-# LANGUAGE ConstraintKinds #-}

module HashedExpression.Internal.Expression
  ( R,
    C,
    Covector,
    ET (..),
    Node (..),
    Internal,
    NodeID,
    ExpressionMap,
    Expression (..),
    Scalar,
    Dimension,
    ToShape (..),
    DimensionType,
    ElementType,
    NumType,
    VectorSpace,
    InnerProductSpace,
    PowerOp (..),
    PiecewiseOp (..),
    VectorSpaceOp (..),
    FTOp (..),
    ComplexRealOp (..),
    RotateOp (..),
    Shape,
    RotateAmount,
    Arg,
    Args,
    BranchArg,
    ConditionArg,
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

-- | Type representation of elements in the 1D, 2D, 3D, ... grid
data R
  deriving (NumType, ElementType, Typeable)

data C
  deriving (NumType, ElementType, Typeable)

data Covector
  deriving (ElementType, Typeable)

-- | Type representation of vector dimension
data Scalar
  deriving (Dimension, Typeable)

-- |
instance (KnownNat n) => Dimension n

instance (KnownNat m, KnownNat n) => Dimension '(m, n)

instance (KnownNat m, KnownNat n, KnownNat p) => Dimension '(m, n, p)

-- | Classes as constraints
class ElementType et

class ElementType et => NumType et

-------------------------------------------------------------------------------
class
  (Dimension d) =>
  ToShape d where
  toShape :: Proxy d -> Shape

instance ToShape Scalar where
  toShape _ = []

instance (KnownNat n) => ToShape n where
  toShape _ = [nat @n]

instance (KnownNat m, KnownNat n) => ToShape '(m, n) where
  toShape _ = [nat @m, nat @n]

instance (KnownNat m, KnownNat n, KnownNat p) => ToShape '(m, n, p) where
  toShape _ = [nat @m, nat @n, nat @p]

type DimensionType d = (Dimension d, ToShape d)

-------------------------------------------------------------------------------

-- |
nat :: forall n. (KnownNat n) => Int
nat = fromIntegral $ natVal (Proxy :: Proxy n)

-------------------------------------------------------------------------------
class Dimension d

class VectorSpace d et s

class VectorSpace d s s => InnerProductSpace d s

instance (DimensionType d, ElementType et) => VectorSpace d et R

instance (DimensionType d) => VectorSpace d C C

instance VectorSpace d s s => InnerProductSpace d s

-- | Classes for operations so that both Expression and Pattern (in HashedPattern) can implement
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

-- | Shape type:
-- []        --> scalar
-- [n]       --> 1D with size n
-- [n, m]    --> 2D with size n × m
-- [n, m, p] --> 3D with size n × m × p
type Shape = [Int]

-- | Args - list of indices of arguments in the ExpressionMap
type Args = [NodeID]

type Arg = NodeID

type ConditionArg = NodeID

type BranchArg = NodeID

-- | Rotation in each dimension.
-- | Property:  the length of this must match the dimension of the data
type RotateAmount = [Int]

-- | Data representation of element type
data ET
  = R
  | C
  | Covector
  deriving (Show, Eq, Ord)

-- | Internal
-- Shape: Shape of the expression
-- we can reconstruct the type of the Expression
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

type role Expression nominal nominal -- So the users cannot use Data.Coerce.coerce to convert between expression types

-- | Node type
data Node
  = Var String
  | DVar String -- only contained in **Expression d Covector (1-form)**
  | Const Double -- only all elements the same
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
  | -- Need these inside because taking real of FT twice can be computed very fast
    TwiceReFT Arg
  | TwiceImFT Arg
  deriving (Show, Eq, Ord)
