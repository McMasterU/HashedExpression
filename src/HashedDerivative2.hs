{-# LANGUAGE ScopedTypeVariables #-}

module HashedDerivative2
    ( exteriorDerivative
    ) where

import Data.IntMap.Strict
import HashedExpression
import HashedHash
import HashedOperation
import Prelude hiding ((*), (+))

-- | Untyped version for computing derivative
--
data UntypedExpression =
    UntypedExpression Int ExpressionMap

exteriorDerivative ::
       forall d. (DimensionType d)
    => Expression d R
    -> Expression d Covector
exteriorDerivative e@(Expression n mp) =
    let (shape, node) = retrieveInternal mp n
     in case node of
            Var name ->
                let node = DVar name
                    shape = []
                    (newMap, h) = fromNode (shape, node)
                -- dx = dx
                 in Expression h newMap
            Const _ ->
                let node = Const 0
                    shape = []
                    (newMap, h) = fromNode (shape, node)
                -- dc = 0
                 in Expression h newMap
            Sum R args ->
--                let subExp1 = Expression n1 mp :: Expression d R
--                    subExp2 = Expression n2 mp :: Expression d R
                let mkSubExp nId = Expression n1 mp :: Expression d R
                 in sum
--            Mul R [node1, node2] ->
--                let subExp1 = Expression node1 mp :: Expression Zero R
--                    subExp2 = Expression node2 mp :: Expression Zero R
--                    diff1 = exteriorDerivative subExp1
--                    diff2 = exteriorDerivative subExp2
--                -- d(f * g) = f * dg + g * df
--                 in subExp1 * diff1 + subExp2 * diff1

hiddenDerivative :: UntypedExpression -> UntypedExpression
hiddenDerivative @e(UntypedExpression n mp) =
    let (shape, node) = retrieveInternal mp n
    in undefined
