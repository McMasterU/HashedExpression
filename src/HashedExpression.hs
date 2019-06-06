{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module HashedExpression where

import Data.Array
import qualified Data.Complex as DC
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Proxy (Proxy)
import Data.Typeable (Typeable, typeRep)
import GHC.TypeLits (Nat)

-- | Type representation of elements in the 1D, 2D, 3D, ... vector
--
data R
    deriving (NumType, ElementType, Typeable)

data C
    deriving (NumType, ElementType, Typeable)

-- we only allow covector fields derived from real scalar fields
data Covector
    deriving (ElementType, Typeable)

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

class NumType rc

class ElementType rc

class DimensionType d

class (DimensionType d, ElementType rc) =>
      Addable d rc


class (DimensionType d, NumType rc) =>
      Ring d rc


class (Addable d rc, ElementType s) =>
      VectorSpace d rc s


class VectorSpace d rc rc =>
      InnerProductSpace d rc


-- | Instances
-- Set language pragma {-# OVERLAPPABLE #-} because GHC looks at the head first (e.g Vector Space d rc R) and check
-- the constraints later, therefore it will show overlap instances error if we declare, say, VectorSpace Covector R R even
-- if Covector R R does not satisfies the constraints
--
instance {-# OVERLAPPABLE #-} (DimensionType d, ElementType rc) =>
                              Addable d rc

instance {-# OVERLAPPABLE #-} (DimensionType d, NumType rc) => Ring d rc

instance {-# OVERLAPPABLE #-} (Addable d rc, ElementType rc) =>
                              VectorSpace d rc R

instance {-# OVERLAPPABLE #-} (Addable d C) => VectorSpace d C C

instance {-# OVERLAPPABLE #-} (DimensionType d, VectorSpace d rc rc) =>
                              InnerProductSpace d rc

--instance (VectorSpace One rc rc, VectorSpace Two rc rc) => Subspace One Two rc
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

-- | Data representation of element type
--
data ET
    = Real
    | Complex
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
data Expression d rc =
    Expression
        Int -- the index this expression
        ExpressionMap -- all subexpressions
    deriving (Show, Eq, Ord, Typeable)

-- | Val type
--
data ConstType
    = Const0D Double
    | All1D Double
    | Const1D (Array Int Double)
    | All2D Double
    | Const2D (Array (Int, Int) Double)
    | All3D Double
    | Const3D (Array (Int, Int, Int) Double)
    deriving (Show, Eq, Ord)

-- | Node type
--
data Node
    = Var String
    | DVar String -- only contained in **Expression Covector R**
    | Const ConstType
    | Sum ET Args -- element-wise sum
    | Mul ET Args -- element-wise multiplication
    | Scale ET Arg Arg -- scalar first
    | InnerProd ET Arg Arg -- inner product
    | RealImg Arg Arg -- from real and imagine
    | Neg ET Arg
    | Abs Arg
    | Signum Arg
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
    | RealPart Arg -- extract real part
    | ImagPart Arg -- extract imaginary part
    deriving (Show, Eq, Ord)

nodeNumType :: Node -> ET
nodeNumType node =
    case node of
        Var _ -> Real
        DVar _ -> Real
        Sum rc _ -> rc
        Mul rc _ -> rc
        Scale rc _ _ -> rc
        InnerProd rc _ _ -> rc
        RealImg _ _ -> Complex

-- | Auxiliary functions for operations
--
expressionElementType :: (ElementType rc) => Expression d rc -> ET
expressionElementType (Expression n mp) =
    case IM.lookup n mp of
        Just (_, node) -> nodeNumType node
        _ -> error "expression not in map"

expressionShape :: Expression d rc -> Shape
expressionShape (Expression n mp) =
    case IM.lookup n mp of
        Just (dim, _) -> dim
        _ -> error "expression not in map"

expressionInternal :: Expression d rc -> Internal
expressionInternal (Expression n mp) =
    case IM.lookup n mp of
        Just internal -> internal
        _ -> error "expression not in map"

expressionNode :: Expression d rc -> Node
expressionNode (Expression n mp) =
    case IM.lookup n mp of
        Just (_, node) -> node
        _ -> error "expression not in map"

retrieveNode :: ExpressionMap -> Int -> Node
retrieveNode mp n =
    case IM.lookup n mp of
        Just (_, node) -> node
        _ -> error "node not in map"

retrieveInternal :: ExpressionMap -> Int -> Internal
retrieveInternal mp n =
    case IM.lookup n mp of
        Just internal -> internal
        _ -> error "node not in map"

ensureSameShape :: Expression d rc -> Expression d rc -> a -> a
ensureSameShape e1 e2 after =
    if expressionShape e1 == expressionShape e2
        then after
        else error "Ensure same shape failed"

fromReal :: Double -> DC.Complex Double
fromReal x = x DC.:+ 0
