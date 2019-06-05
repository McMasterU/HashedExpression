module HashedDerivative2 where

import Data.IntMap.Strict
import HashedExpression
import HashedHash
import HashedOperation
import Prelude hiding ((*), (+))

exteriorDerivative :: Expression Scalar R -> Expression Covector R
exteriorDerivative e@(Expression n mp) =
    case retrieveInternal mp n of
        ([], Var name) ->
            let node = DVar name
                shape = []
                (newMap, h) = fromNode (shape, node)
                -- dx = dx
             in Expression h newMap
        ([], Const (Const0D _)) ->
            let node = Const (Const0D 0)
                shape = []
                (newMap, h) = fromNode (shape, node)
                -- dc = 0
             in Expression h newMap
        ([], Sum Real [node1, node2]) ->
            let subExp1 = Expression node1 mp :: Expression Scalar R
                subExp2 = Expression node2 mp :: Expression Scalar R
                -- d(f + g) = df + dg
             in exteriorDerivative subExp1 + exteriorDerivative subExp2
        ([], Mul Real [node1, node2]) ->
            let subExp1 = Expression node1 mp :: Expression Scalar R
                subExp2 = Expression node2 mp :: Expression Scalar R
                diff1 = exteriorDerivative subExp1
                diff2 = exteriorDerivative subExp2
                -- d(f * g) = f * dg + g * df
             in subExp1 * diff1 + subExp2 * diff1
