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
import HashedUtils

-- | Type representation of elements in the 1D, 2D, 3D, ... grid
--
data R
    deriving (NumType, ElementType, Addable, Typeable)

data C
    deriving (NumType, ElementType, Addable, Typeable)

-- we only allow covector fields derived from real scalar fields
data Covector
    deriving (ElementType, Addable, Typeable)

-- | Type representation of vector dimension
--
data Zero
    deriving (DimensionType, Typeable)

data One
    deriving (DimensionType, Typeable)

data Two
    deriving (DimensionType, Typeable)

data Three
    deriving (DimensionType, Typeable)

class ElementType et

class ElementType et =>
      NumType et


class DimensionType d

class ElementType et =>
      Addable et


class (DimensionType d, Addable et, NumType s) =>
      VectorSpace d et s


-- | Instances
-- Set language pragma {-# OVERLAPPABLE #-} because GHC looks at the head first (e.g VectorSpace d et R) and check
-- the constraints later, therefore it will show overlap instances error if we declare more instances of VectorSpace if
-- if the arguments don't satisfy the constraints
--
--instance {-# OVERLAPPABLE #-} ElementType et => Addable et
instance {-# OVERLAPPABLE #-} (ElementType et, Addable et, DimensionType d) =>
                              VectorSpace d et R

instance {-# OVERLAPPABLE #-} (DimensionType d) => VectorSpace d C C

instance {-# OVERLAPPABLE #-} (DimensionType d) =>
                              VectorSpace d Covector R

--instance (VectorSpace One et et, VectorSpace Two et et) => Subspace One Two et
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
    = R
    | C
    | Covector -- this is data constructor Covector
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

-- | Const type
--
--data ConstType
--    = Const0D Double
--    | All1D Double
--    | Const1D (Array Int Double)
--    | All2D Double
--    | Const2D (Array (Int, Int) Double)
--    | All3D Double
--    | Const3D (Array (Int, Int, Int) Double)
--    deriving (Show, Eq, Ord)
-- | Node type
--
data Node
    = Var String
    | DVar String -- only contained in **Expression d Covector (1-form)**
    | Const Double -- only all elements the same
    | Sum ET Args -- element-wise sum
    | Mul ET Args -- multiply --> have different meanings (scale in vector space, multiplication, ...)
    | RImg Arg Arg -- from real and imagine
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

nodeElementType :: Node -> ET
nodeElementType node =
    case node of
        Var _ -> R
        DVar _ -> Covector
        Sum et _ -> et
        Mul et _ -> et
        RImg _ _ -> C
        -- TODO: and more

-- | Auxiliary functions for operations
--

retrieveNode :: Int -> ExpressionMap -> Node
retrieveNode n mp =
    case IM.lookup n mp of
        Just (_, node) -> node
        _ -> error "node not in map"

retrieveInternal :: Int -> ExpressionMap -> Internal
retrieveInternal n mp =
    case IM.lookup n mp of
        Just internal -> internal
        _ -> error "node not in map"

retrieveElementType :: Int -> ExpressionMap -> ET
retrieveElementType n mp =
    case IM.lookup n mp of
        Just (_, node) -> nodeElementType node
        _ -> error "expression not in map"

retrieveShape :: Int -> ExpressionMap -> Shape
retrieveShape n mp =
    case IM.lookup n mp of
        Just (dim, _) -> dim
        _ -> error "expression not in map"

expressionElementType :: Expression d et -> ET
expressionElementType (Expression n mp) =
    case IM.lookup n mp of
        Just (_, node) -> nodeElementType node
        _ -> error "expression not in map"


expressionShape :: Expression d et -> Shape
expressionShape (Expression n mp) =
    case IM.lookup n mp of
        Just (dim, _) -> dim
        _ -> error "expression not in map"


expressionInternal :: Expression d et -> Internal
expressionInternal (Expression n mp) =
    case IM.lookup n mp of
        Just internal -> internal
        _ -> error "expression not in map"

expressionNode :: Expression d et -> Node
expressionNode (Expression n mp) =
    case IM.lookup n mp of
        Just (_, node) -> node
        _ -> error "expression not in map"

ensureSameShape :: Expression d et1 -> Expression d et2 -> a -> a
ensureSameShape e1 e2 after =
    if expressionShape e1 == expressionShape e2
        then after
        else error "Ensure same shape failed"

ensureSameShapeList :: [Expression d et] -> a -> a
ensureSameShapeList es after =
    if allEqual es
        then after
        else error "Ensure same shape failed"

fromR :: Double -> DC.Complex Double
fromR x = x DC.:+ 0
