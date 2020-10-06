-- |
-- Module      :  HashedExpression.Internal.Base
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- This modules contains types that should be imported only by Internal modules
module HashedExpression.Internal.Base where

import Data.IntMap.Strict (IntMap)
import Data.Typeable (Typeable)
import Prelude hiding ((**), (^))

newtype NodeID = NodeID {unNodeID :: Int} deriving (Eq, Ord)

instance Show NodeID where
  show (NodeID nID) = show nID

-- | Shape of an expression
-- >  []        => Scalar
-- >  [n]       => 1D
-- >  [n, m]    => 2D
type Shape = [Int]

-- | Represents Real, Complex
data ElementType = R | C
  deriving (Show, Eq, Ord, Typeable)

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
  | -- | Matrix multiplication
    MatMul Arg Arg
  | -- | Transpose
    Transpose Arg
  | -- | Internal only: coerce shape
    Coerce Shape Arg
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
  | Range -- (inclusive)
      Int -- start
      Int -- end
      Int -- step
  deriving (Show, Eq, Ord)

-- | Rotation in each dimension, given respectively in a list.
--   The length of the list matches the number of dimensions
type RotateAmount = [Int]

-- | The bulk of an 'Expression' is a collection of 'Node' (with their dimensions), in
--   a Map indexed 'NodeID' (a generated hash value)
type ExpressionMap = IntMap Node

-- | The internals of an 'Expression' are a collection of 'Op' with
--   their dimensions
type Node = (Shape, ElementType, Op)

-- |
--
-- | Interface for power
class PowerOp a b | a -> b where
  (^) :: a -> b -> a

-- | Interface for scaling
class ScaleOp a b where
  scale :: a -> b -> b
  (*.) :: a -> b -> b
  (*.) = scale

-- | Interface for complex combinators
class ComplexRealOp r c | r -> c, c -> r where
  -- | construct complex from real / imaginary parts
  (+:) :: r -> r -> c

  -- | extract real part from complex
  xRe :: c -> r

  -- | extract imaginary part from complex
  xIm :: c -> r

  -- | conjugate
  conjugate :: c -> c

-- | Interface for inner product
class InnerProductSpaceOp a b | a -> b where
  (<.>) :: a -> a -> b

-- | Interface for rotation
class RotateOp k a | a -> k where
  rotate :: k -> a -> a

-- | Interface for piecewise function
class PiecewiseOp a b where
  piecewise :: [Double] -> a -> [b] -> b

-- | Interface for fourier transform
class FTOp a b | a -> b, b -> a where
  ft :: a -> b
  ift :: b -> a

-- |
class ProjectInjectOp s a b | s a -> b where
  project :: s -> a -> b
  inject :: s -> b -> a -> a

class MatrixMulOp a b c | a b -> c where
  (**) :: a -> b -> c
  matmul :: a -> b -> c
  matmul = (**)

class TransposeOp a b | a -> b where
  transpose :: a -> b

-------------------------------------------------------------------------------
infixl 6 +:

infixl 8 *., `scale`, <.>, **

infixl 8 ^

-------------------------------------------------------------------------------

-- | If the type corresponds to an expression
--
class IsExpression e where 
  asExpression :: e -> (ExpressionMap, NodeID)
  wrapExpression :: (ExpressionMap, NodeID) -> e

-- | If the type corresponds to a scalar real expression
--
class IsExpression e => IsScalarReal e where
  asScalarReal :: e -> (ExpressionMap, NodeID)

-------------------------------------------------------------------------------

instance IsExpression (ExpressionMap, NodeID) where 
  asExpression = id
  wrapExpression = id

instance IsScalarReal (ExpressionMap, NodeID) where 
  asScalarReal = id
