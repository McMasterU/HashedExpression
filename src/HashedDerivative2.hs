module HashedDerivative2 where

import Data.IntMap.Strict
import HashedExpression
import HashedHash
import HashedOperation
import Prelude hiding ((*), (+))

differentialForm :: Expression Scalar R -> Expression Covector R
differentialForm e@(Expression n mp) =
    case retrieveInternal mp n of
        ([], Var name) ->
            let node = DVar name
                shape = []
                (newMap, h) = addEdge mp (shape, node)
             in Expression h newMap
        ([], Sum Real [node1, node2]) ->
            let
                subExp1 = Expression node1 mp :: Expression Scalar R
                subExp2 = Expression node2 mp :: Expression Scalar R
             in differentialForm subExp1 + differentialForm subExp2
        ([], Mul Real [node1, node2]) ->
            let
                subExp1 = Expression node1 mp :: Expression Scalar R
                subExp2 = Expression node2 mp :: Expression Scalar R
                diff1 = differentialForm subExp1
                diff2 = differentialForm subExp2
             in subExp1 * diff1 + subExp2 * diff1
        ([], Val (S _)) ->
            let node = Val (S 0)
                shape = []
                (newMap, h) = addEdge mp (shape, node)
             in Expression h newMap



