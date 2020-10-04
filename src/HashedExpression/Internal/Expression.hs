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
    IsElementType (..),
    Expression (..),
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
    -- | The following classes define 'Expression' operators that can be overloaded to directly
    --   support a variety of functionality; such as interpretation, pattern matching, differentiation, etc
    PowerOp (..),
    PiecewiseOp (..),
    ScaleOp (..),
    FTOp (..),
    ComplexRealOp (..),
    RotateOp (..),
    InnerProductSpaceOp (..),
    MatrixMulOp (..),
    TransposeOp (..),
    ProjectInjectOp (..),
    module HashedExpression.Internal.Base
  )
where

import Data.Array
import qualified Data.Complex as DC
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Proxy (Proxy (..))
import Data.Typeable
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownNat, Nat, natVal)
import HashedExpression.Internal.Base
import Prelude hiding ((**), (^))


-- | 
-- 
data Expression (d :: [Nat]) (et :: ElementType) = Expression
  { -- | index to the topological root of ExpressionMap
    exRootID :: NodeID,
    -- | Map of all 'Node' indexable by 'NodeID'
    exMap :: ExpressionMap
  }
  deriving (Show, Eq, Ord)

type role Expression nominal nominal

-- --------------------------------------------------------------------------------------------------------------------

-- * Expression Element Types

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

-- * Expression Dimensions

-- --------------------------------------------------------------------------------------------------------------------

-- | Type-level encoding of shapes
type Scalar = '[]

type D1 (n :: Nat) = '[n]

type D2 (m :: Nat) (n :: Nat) = '[m, n]

type D3 (m :: Nat) (n :: Nat) (p :: Nat) = '[m, n, p]

-- | Use to constrain 'Expression' dimensions at the type level. The size of each dimension in a vector can be specified
--   using a 'KnownNat', for vectors of n-dimensions use an n-sized tuple
class Dimension (d :: [Nat]) where
  toShape :: Shape

instance Dimension '[] where
  toShape = []

instance (KnownNat x, Dimension xs) => Dimension (x ': xs) where
  toShape = (nat @x) : toShape @xs

-- | Helper function, wrapper over 'natVal' from 'GHC.TypeLits' that automaticaly converts resulting value
--   from Integer to Int
nat :: forall n. (KnownNat n) => Int
nat = fromIntegral $ natVal (Proxy :: Proxy n)

-- --------------------------------------------------------------------------------------------------------------------

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
--   
class PiecewiseOp a b where
  piecewise :: [Double] -> a -> [b] -> b

-- | Interface for fourier transform 
--   
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
