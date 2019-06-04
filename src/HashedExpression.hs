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

import qualified Data.Complex as DC
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Proxy (Proxy)
import Data.Typeable (Typeable, typeRep)
import GHC.TypeLits (Nat)

-- | Type representation of Real and Complex num type
--
data R
    deriving (NumType)

data C
    deriving (NumType)

-- | Type representation of vector dimension
--
data Scalar
    deriving (DimensionType)

data One
    deriving (DimensionType)

data Two
    deriving (DimensionType)

data Three
    deriving (DimensionType)

-- | Type classes
--
class NumType rc

class DimensionType d

class (DimensionType d, NumType rc) =>
      Ring d rc


class Ring d rc =>
      VectorSpace d rc s


class VectorSpace d rc rc =>
      InnerProductSpace d rc


class (VectorSpace d1 rc rc, VectorSpace d2 rc rc) => Subspace d1 d2 rc

-- | Instances
--
instance (DimensionType d, NumType rc) => Ring d rc

instance (Ring d rc) => VectorSpace d rc R

instance (Ring d C) => VectorSpace d C C

instance (VectorSpace d rc rc) => InnerProductSpace d rc

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

-- | Data representation of Real and Complex num type
--
data RC
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

--data Form (k :: Nat) =
--    Form
--        Int
--        ExpressionMap
--
--FormNode
--    = Expression Int
--    | Connect Int String

--    = SingleDVar String Expression
--                     | DVarSum String Expression Expression


data Node 
    = Var String
    | DVar String
    | Sum RC Args -- element-wise sum
    | Mul RC Args -- element-wise multiplication
    | Scale RC Arg Arg -- scalar first, TODO: Int Int instead ?
    | InnerProd RC Arg Arg -- inner product, TODO: Int Int instead ?
    | RealImg Arg Arg -- from real and imagine, TODO: Int Int instead ?
    | Neg RC Arg
    | Abs RC Arg
    | Signum RC Arg
    | Div RC Arg Arg
    | Sqrt Arg
    | Sin RC Arg
    | Cos RC Arg
    | Tan RC Arg
    | Exp RC Arg
    | Log RC Arg
    | Sinh RC Arg
    | Cosh RC Arg
    | Tanh RC Arg
    | Asin RC Arg
    | Acos RC Arg
    | Atan RC Arg
    | Asinh RC Arg
    | Acosh RC Arg
    | Atanh RC Arg
    | RealPart Arg -- extract real part
    | ImagPart Arg -- extract imaginary part
    deriving (Show, Eq, Ord)

nodeNumType :: Node -> RC
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
expressionNumType :: (NumType rc) => Expression d rc -> RC
expressionNumType (Expression n mp) =
    case IM.lookup n mp of
        Just (_, node) -> nodeNumType node
        _ -> error "expression not in map"

expressionShape :: (DimensionType d) => Expression d rc -> Shape
expressionShape (Expression n mp) =
    case IM.lookup n mp of
        Just (dim, _) -> dim
        _ -> error "expression not in map"

retrieveNode :: ExpressionMap -> Int -> Node
retrieveNode mp n =
    case IM.lookup n mp of
        Just (_, node) -> node
        _ -> error "node not in map"

ensureSameShape :: (Ring d rc) => Expression d rc -> Expression d rc -> a -> a
ensureSameShape e1 e2 after =
    if expressionShape e1 == expressionShape e2
        then after
        else error "Ensure same shape failed"


fromReal :: Double -> DC.Complex Double
fromReal x = x DC.:+ 0
