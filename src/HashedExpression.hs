{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE FunctionalDependencies #-}

module HashedExpression where

import Data.Array
import qualified Data.Complex as DC
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Proxy (Proxy)
import Data.Typeable (Typeable, typeRep)
import GHC.TypeLits (KnownNat, Nat)
import Prelude hiding
    ( (*)
    , (+)
    , (-)
    , (/)
    , (^)
    , acos
    , acosh
    , asin
    , asinh
    , atan
    , atanh
    , cos
    , cosh
    , exp
    , negate
    , sin
    , sinh
    , tan
    , tanh
    )

-- | Type representation of elements in the 1D, 2D, 3D, ... grid
--
data R
    deriving (NumType, ElementType, Addable, Typeable)

data C
    deriving (NumType, ElementType, Addable, Typeable)

data Covector
    deriving (ElementType, Addable, Typeable)

-- | Type representation of vector dimension
--
data Scalar
    deriving (DimensionType, Typeable)

data One
    deriving (DimensionType, Typeable)

data Two
    deriving (DimensionType, Typeable)

data Three
    deriving (DimensionType, Typeable)

-- | 
--
instance (KnownNat n) => DimensionType n

instance (KnownNat m, KnownNat n) => DimensionType '( m, n)

instance (KnownNat m, KnownNat n, KnownNat p) => DimensionType '( m, n, p)

-- | Classes as constraints
--
class ElementType et

class ElementType et =>
      NumType et


class DimensionType d

class ElementType et =>
      Addable et


class (DimensionType d, Addable et, NumType s) =>
      VectorSpace d et s


class VectorSpace d s s =>
      InnerProductSpace d s


-- | Instances
-- Set language pragma {-# OVERLAPPABLE #-} because GHC looks at the head first (e.g VectorSpace d et R) and check
-- the constraints later, therefore it will show overlap instances error if we declare more instances of VectorSpace if
-- if the arguments don't satisfy the constraints
--
--instance {-# OVERLAPPABLE #-} ElementType et => Addable et
instance {-# OVERLAPPABLE #-} (ElementType et, Addable et, DimensionType d) =>
                              VectorSpace d et R

instance (DimensionType d) => VectorSpace d C C

instance (DimensionType d) => VectorSpace d Covector R

instance VectorSpace d s s => InnerProductSpace d s

-- | Classes for operations so that both Expression and Pattern (in HashedPattern) can implement
--
class AddableOp a where
    (+) :: a -> a -> a

class NegateOp a where
    negate :: a -> a

(-) :: (AddableOp a, NegateOp a) => a -> a -> a
x - y = x + negate y

class MultiplyOp a where
    (*) :: a -> a -> a

class PowerOp a b | a -> b where
    (^) :: a -> b -> a

class VectorSpaceOp a b where
    scale :: a -> b -> b
    (*.) :: a -> b -> b
    (*.) = scale

class NumOp a where
    sqrt :: a -> a
    exp :: a -> a
    log :: a -> a
    sin :: a -> a
    cos :: a -> a
    tan :: a -> a
    asin :: a -> a
    acos :: a -> a
    atan :: a -> a
    sinh :: a -> a
    cosh :: a -> a
    tanh :: a -> a
    asinh :: a -> a
    acosh :: a -> a
    atanh :: a -> a
    (/) :: a -> a -> a

class ComplexRealOp r c | r -> c, c -> r where
    (+:) :: r -> r -> c
    xRe :: c -> r
    xIm :: c -> r

class InnerProductSpaceOp a b c | a b -> c where
    (<.>) :: a -> b -> c

class RotateOp k a | a -> k where
    rotate :: k -> a -> a

class FTOp a b | a -> b where
    ft :: a -> b

infixl 6 +, -

infixl 7 *, /

infixl 8 *., `scale`, <.>

infixl 8 ^

-- | Shape type:
-- []        --> scalar
-- [n]       --> 1D with size n
-- [n, m]    --> 2D with size n × m
-- [n, m, p] --> 3D with size n × m × p
type Shape = [Int]

-- | Args - list of indices of arguments in the ExpressionMap
--
type Args = [Int]

type Arg = Int

type ConditionArg = Int

type BranchArg = Int

-- | Rotation in each dimension.
-- | Property:  the length of this must match the dimension of the data
type RotateAmount = [Int]

-- | Data representation of element type
--
data ET
    = R
    | C
    | Covector
    deriving (Show, Eq, Ord)

-- | Internal
-- Shape: Shape of the expression
-- we can reconstruct the type of the Expression
--
type Internal = (Shape, Node)

-- | Hash map of all subexpressions
--
type ExpressionMap = IntMap Internal

-- | Expression with 2 phantom types (dimension and num type)
--
data Expression d et =
    Expression
        { exIndex :: Int -- the index this expression
        , exMap :: ExpressionMap -- all subexpressions
        }
    deriving (Show, Eq, Ord, Typeable)

type role Expression nominal nominal -- So the users cannot use Data.Coerce.coerce to convert between expression types

-- | Node type
--
data Node
    = Var String
    | DVar String -- only contained in **Expression d Covector (1-form)**
    | Const Double -- only all elements the same
    -- MARK: Basics
    | Sum ET Args -- element-wise sum
    | Mul ET Args -- multiply --> have different meanings (scale in vector space, multiplication, ...)
    | Power Int Arg -- TODO: Power for Complex or not ?
    | Neg ET Arg
    | Scale ET Arg Arg
    -- MARK: only apply to R
    | Div Arg Arg
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
    -- MARK: Complex related
    | RealImag Arg Arg -- from real and imagine
    | RealPart Arg -- extract real part
    | ImagPart Arg -- extract imaginary part
    -- MARK: Inner product Space
    | InnerProd ET Arg Arg
    -- MARK: Piecewise
    | Piecewise [Double] ConditionArg [BranchArg]
    | Rotate RotateAmount Arg
    -- MARK: Discrete Fourier Transform
    | ReFT Arg
    | ImFT Arg
    -- Need these inside because taking real of FT twice can be computed very fast
    | TwiceReFT Arg
    | TwiceImFT Arg
    deriving (Show, Eq, Ord)
