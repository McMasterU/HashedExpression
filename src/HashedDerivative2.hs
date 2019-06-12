{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module HashedDerivative2
    ( exteriorDerivative
    ) where

import qualified Data.IntMap.Strict as IM
import Data.List.HT (removeEach)
import HashedExpression
import HashedHash
import HashedOperation
import Prelude hiding ((*), (+), sum)

-- | TODO - How can we define more kind of type class constraint to reuse the type-safe operations in HashedOperation ?
-- | TODO - Now, we aren't able to do so, so we just write untyped version of the derivative
-- Assume the expression is correctly built
exteriorDerivative ::
       forall d. (DimensionType d)
    => Expression d R
    -> Expression d Covector
exteriorDerivative (Expression n mp) =
    let (shape, node) = retrieveInternal n mp
     in case node of
            Var name ->
                let node = DVar name
                    (newMap, h) = fromNode (shape, node)
                -- dx = dx
                 in Expression h newMap
            Const _ ->
                let node = Const 0
                    (newMap, h) = fromNode (shape, node)
                -- dc = 0
                 in Expression h newMap
            Sum R args -- sum rule
                | length args >= 2 ->
                    let mkSub nId = (Expression nId mp :: Expression d R)
                     in wrap . generalSum . map (unwrap . exteriorDerivative . mkSub) $ args
            Mul R args -- multiplication rule
                | length args >= 2 -> undefined

-- |
--
unwrap :: Expression d et -> (Int, ExpressionMap)
unwrap (Expression n mp) = (n, mp)

wrap :: (Int, ExpressionMap) -> Expression d et
wrap = uncurry Expression

-- | General multiplication
--
highestShape :: [(Int, ExpressionMap)] -> Shape
highestShape = foldl f []
  where
    f acc (n, mp) =
        if length acc > length (retrieveShape n mp)
            then acc
            else retrieveShape n mp

highestElementType :: [(Int, ExpressionMap)] -> ET
highestElementType = foldl f R
  where
    f acc (n, mp) = max acc (retrieveElementType n mp) -- R < C < Covector (TODO - ad hoc?)

generalMul :: [(Int, ExpressionMap)] -> (Int, ExpressionMap)
generalMul es = (h, newMap)
  where
    elementType = highestElementType es
    shape = highestShape es
    node = Mul elementType . map fst $ es
    mergedMap = foldl1 IM.union . map snd $ es
    (newMap, h) = addEdge mergedMap (shape, node)

-- | General sum
--
generalSum :: [(Int, ExpressionMap)] -> (Int, ExpressionMap)
generalSum es = (h, newMap)
  where
    (n, mp) = head es
    elementType = retrieveElementType n mp
    shape = retrieveShape n mp
    node = Sum elementType . map fst $ es
    mergedMap = foldl1 IM.union . map snd $ es
    (newMap, h) = addEdge mergedMap (shape, node)
