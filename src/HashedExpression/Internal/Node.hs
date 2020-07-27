-- |
-- Module      :  HashedExpression.Internal.Node
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- This module contains a variety of helper functions for working with the 'Node' type, including stuff like finding the ElementType of a
-- Node, returning a Node's arguments, returning a Node's shape, etc
module HashedExpression.Internal.Node
  ( nodeTypeWeight,
    sameOp,
    opArgs,
    retrieveElementType,
    retrieveNode,
    retrieveOp,
    retrieveShape,
    retrievesNode,
    retrievesOp,
    retrievesElementType,
    retrievesShape,
    expressionElementType,
    expressionNode,
    expressionOp,
    expressionShape,
    mapOp,
    mapNode,
  )
where

import qualified Data.IntMap.Strict as IM
import Data.List (sort)
import Data.Tuple.Extra (fst3, snd3, thd3)
import GHC.Stack (HasCallStack)
import HashedExpression.Internal.Expression

-- | For ordering things inside 'Sum' or 'Product' so we can write rules like
--
-- @
--   restOfProduct ~* (x +: y) ~* (z +: w) |.~~~~~~> restOfProduct ~*
--   restOfSum ~+ (x +: y) ~+ (u +: v) |.~~~~~~> restOfSum ~+ ((x + u) +: (y + v))
--   ...
-- @
nodeTypeWeight :: HasCallStack => Op -> Int
nodeTypeWeight node =
  case node of
    Const {} -> 0
    Var {} -> 1
    Mul {} -> 3
    Power {} -> 4
    Neg {} -> 5
    Div {} -> 7
    Sqrt {} -> 8
    Sin {} -> 9
    Cos {} -> 10
    Tan {} -> 11
    Exp {} -> 12
    Log {} -> 13
    Sinh {} -> 14
    Cosh {} -> 15
    Tanh {} -> 16
    Asin {} -> 17
    Acos {} -> 18
    Atan {} -> 19
    Asinh {} -> 20
    Acosh {} -> 21
    Atanh {} -> 22
    RealPart {} -> 24
    ImagPart {} -> 25
    InnerProd {} -> 26
    Conjugate {} -> 27
    Piecewise {} -> 28
    Rotate {} -> 29
    ReFT {} -> 30
    ImFT {} -> 31
    TwiceReFT {} -> 32
    TwiceImFT {} -> 33
    Scale {} -> 34 -- Right after RealImag
    RealImag {} -> 35 -- At the end right after sum
    Sum {} -> 36 -- Sum at the end
    ------------------------
    DVar {} -> 101
    DZero {} -> 102
    MulD {} -> 103
    ScaleD {} -> 104
    DScale {} -> 105
    InnerProdD {} -> 106

-- | Equality for 'Node' types (i.e same constructor), not equality of hash
sameOp :: HasCallStack => Op -> Op -> Bool
sameOp node1 node2 = nodeTypeWeight node1 == nodeTypeWeight node2

-- | Retrieve the parameters (i.e 'Args') attached to a given 'Node'
opArgs :: Op -> Args
opArgs node =
  case node of
    Var _ -> []
    DVar _ -> []
    Const _ -> []
    Sum args -> args
    Mul args -> args
    Power _ arg -> [arg]
    Neg arg -> [arg]
    Scale arg1 arg2 -> [arg1, arg2]
    Div arg1 arg2 -> [arg1, arg2]
    Sqrt arg -> [arg]
    Sin arg -> [arg]
    Cos arg -> [arg]
    Tan arg -> [arg]
    Exp arg -> [arg]
    Log arg -> [arg]
    Sinh arg -> [arg]
    Cosh arg -> [arg]
    Tanh arg -> [arg]
    Asin arg -> [arg]
    Acos arg -> [arg]
    Atan arg -> [arg]
    Asinh arg -> [arg]
    Acosh arg -> [arg]
    Atanh arg -> [arg]
    RealImag arg1 arg2 -> [arg1, arg2]
    RealPart arg -> [arg]
    ImagPart arg -> [arg]
    Conjugate arg -> [arg]
    InnerProd arg1 arg2 -> [arg1, arg2]
    Piecewise _ conditionArg branches -> conditionArg : branches
    Rotate _ arg -> [arg]
    ReFT arg -> [arg]
    ImFT arg -> [arg]
    TwiceReFT arg -> [arg]
    TwiceImFT arg -> [arg]
    DZero -> []
    MulD arg1 arg2 -> [arg1, arg2]
    ScaleD arg1 arg2 -> [arg1, arg2]
    DScale arg1 arg2 -> [arg1, arg2]
    InnerProdD arg1 arg2 -> [arg1, arg2]

mapOp :: (NodeID -> NodeID) -> Op -> Op
mapOp f op =
  case op of
    Var x -> Var x
    DVar x -> DVar x
    Const val -> Const val
    Sum args -> Sum $ map f args
    Mul args -> Mul $ map f args
    Power x arg -> Power x (f arg)
    Neg arg -> Neg (f arg)
    Scale arg1 arg2 -> Scale (f arg1) (f arg2)
    Div arg1 arg2 -> Div (f arg1) (f arg2)
    Sqrt arg -> Sqrt (f arg)
    Sin arg -> Sin (f arg)
    Cos arg -> Cos (f arg)
    Tan arg -> Tan (f arg)
    Exp arg -> Exp (f arg)
    Log arg -> Log (f arg)
    Sinh arg -> Sinh (f arg)
    Cosh arg -> Cosh (f arg)
    Tanh arg -> Tanh (f arg)
    Asin arg -> Asin (f arg)
    Acos arg -> Acos (f arg)
    Atan arg -> Atan (f arg)
    Asinh arg -> Asinh (f arg)
    Acosh arg -> Acosh (f arg)
    Atanh arg -> Atanh (f arg)
    RealImag arg1 arg2 -> RealImag (f arg1) (f arg2)
    RealPart arg -> RealPart (f arg)
    ImagPart arg -> ImagPart (f arg)
    Conjugate arg -> Conjugate (f arg)
    InnerProd arg1 arg2 -> InnerProd (f arg1) (f arg2)
    Piecewise marks conditionArg branches -> Piecewise marks (f conditionArg) (map f branches)
    Rotate am arg -> Rotate am (f arg)
    ReFT arg -> ReFT (f arg)
    ImFT arg -> ImFT (f arg)
    TwiceReFT arg -> TwiceReFT (f arg)
    TwiceImFT arg -> TwiceImFT (f arg)
    DZero -> DZero
    MulD arg1 arg2 -> MulD (f arg1) (f arg2)
    ScaleD arg1 arg2 -> ScaleD (f arg1) (f arg2)
    DScale arg1 arg2 -> DScale (f arg1) (f arg2)
    InnerProdD arg1 arg2 -> InnerProdD (f arg1) (f arg2)

mapNode :: (NodeID -> NodeID) -> Node -> Node
mapNode f (shape, et, op) = (shape, et, mapOp f op)

-- | Retrieve a 'Op' from it's base 'ExpressionMap' and 'NodeID'
{-# INLINE retrieveOp #-}
retrieveOp :: HasCallStack => NodeID -> ExpressionMap -> Op
retrieveOp n mp =
  case IM.lookup n mp of
    Just (_, _, op) -> op
    _ -> error "node not in map"

-- | Retrieve a 'Node' structure (i.e a 'Node' with it's 'Shape') from it's base 'ExpressionMap' and 'NodeID'
{-# INLINE retrieveNode #-}
retrieveNode :: HasCallStack => NodeID -> ExpressionMap -> Node
retrieveNode n mp =
  case IM.lookup n mp of
    Just internal -> internal
    _ -> error "node not in map"

-- | Retrieve the ElementType (i.e 'R','C','Covector') of a 'Node' from it's base 'ExpressionMap' and 'NodeID'
{-# INLINE retrieveElementType #-}
retrieveElementType :: HasCallStack => NodeID -> ExpressionMap -> ET
retrieveElementType n mp =
  case IM.lookup n mp of
    Just (_, et, _) -> et
    _ -> error "expression not in map"

-- | Retrieve the 'Shape' of a 'Node' from it's base 'ExpressionMap' and 'NodeID'
{-# INLINE retrieveShape #-}
retrieveShape :: HasCallStack => NodeID -> ExpressionMap -> Shape
retrieveShape n mp =
  case IM.lookup n mp of
    Just (shape, _, _) -> shape
    _ -> error "expression not in map"

-- | Retrieve the 'ElementType' (i.e 'R','C','Covector') of a 'Expression'
{-# INLINE expressionElementType #-}
expressionElementType :: HasCallStack => Expression d et -> ET
expressionElementType (Expression n mp) =
  case IM.lookup n mp of
    Just (_, et, _) -> et
    _ -> error "expression not in map"

-- | Retrieve the 'Shape' of an 'Expression'
{-# INLINE expressionShape #-}
expressionShape :: HasCallStack => Expression d et -> Shape
expressionShape (Expression n mp) =
  case IM.lookup n mp of
    Just (dim, _, _) -> dim
    _ -> error "expression not in map"

-- | Retrieve the 'Node' of an 'Expression'
{-# INLINE expressionNode #-}
expressionNode :: HasCallStack => Expression d et -> Node
expressionNode (Expression n mp) =
  case IM.lookup n mp of
    Just internal -> internal
    _ -> error "expression not in map"

-- | Retrieve the Op of an expression
{-# INLINE expressionOp #-}
expressionOp :: HasCallStack => Expression d et -> Op
expressionOp (Expression n mp) =
  case IM.lookup n mp of
    Just (_, _, op) -> op
    _ -> error "expression not in map"

-- | Retrieve a 'Node' structure from the list of Expression map, must exists
retrievesNode :: HasCallStack => NodeID -> [ExpressionMap] -> Node
retrievesNode n [] = error "node not in any map"
retrievesNode n (mp : mps) =
  case IM.lookup n mp of
    Just internal -> internal
    _ -> retrievesNode n mps

-- | Retrieve a 'Op' structure from the list of Expression map, must exists
retrievesOp :: HasCallStack => NodeID -> [ExpressionMap] -> Op
retrievesOp n mps = thd3 $ retrievesNode n mps

-- | Retrieve a 'ElementType' structure from the list of Expression map, must exists
retrievesElementType :: HasCallStack => NodeID -> [ExpressionMap] -> ET
retrievesElementType n mps = snd3 $ retrievesNode n mps

---- | Retrieve a 'Shape' structure from the list of Expression map, must exists
retrievesShape :: HasCallStack => NodeID -> [ExpressionMap] -> Shape
retrievesShape n mps = fst3 $ retrievesNode n mps
