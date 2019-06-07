{-# LANGUAGE ScopedTypeVariables #-}

module HashedDerivative2
    ( exteriorDerivative
    ) where

import qualified Data.IntMap.Strict as IM
import HashedExpression
import HashedHash
import HashedOperation
import Prelude hiding ((*), (+), sum)

-- | TODO - How can we define more kind of type class constraint to reuse the type-safe operations in HashedOperation ?
-- | TODO - Now, we aren't able to do so, so we just write untyped version of the derivative
--
exteriorDerivative ::
       forall d. (DimensionType d)
    => Expression d R
    -> Expression d Covector
exteriorDerivative = toTyped . hiddenExteriorDerivative . toUntyped

-- | Untyped version for computing derivative
--
data UntypedExpression =
    UntypedExpression Int ExpressionMap

toTyped ::
       (DimensionType d, ElementType et) => UntypedExpression -> Expression d et
toTyped (UntypedExpression n mp) = Expression n mp

toUntyped ::
       (DimensionType d, ElementType et) => Expression d et -> UntypedExpression
toUntyped (Expression n mp) = UntypedExpression n mp

-- | General multiplication
--
generalMul :: [UntypedExpression] -> UntypedExpression
generalMul es = undefined
--  where
--    elementType = expressionElementType e1
--    shape = expressionShape e1
--    node = Mul elementType [n1, n2]
--    (newMap, h) = addEdge (mp1 `IM.union` mp2) (shape, node)

generalSum :: [UntypedExpression] -> UntypedExpression
generalSum = undefined

-- |
--
hiddenExteriorDerivative :: UntypedExpression -> UntypedExpression
hiddenExteriorDerivative e@(UntypedExpression n mp) = undefined
--    let (exShape, exNode) = retrieveInternal mp n
--     in undefined
