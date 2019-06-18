module HashedInner where

import qualified Data.IntMap.Strict as IM
import HashedExpression
import HashedUtils
import HashedHash
import HashedNode

-- |
--
unwrap :: Expression d et -> (ExpressionMap, Int)
unwrap (Expression n mp) = (mp, n)

wrap :: (ExpressionMap, Int) -> Expression d et
wrap = uncurry $ flip Expression

-- |
--
highestShape :: [(ExpressionMap, Int)] -> Shape
highestShape = foldl f []
  where
    f acc (mp, n) =
        if length acc > length (retrieveShape n mp)
            then acc
            else retrieveShape n mp

highestElementType :: [(ExpressionMap, Int)] -> ET
highestElementType = foldl f R
  where
    f acc (mp, n) = max acc (retrieveElementType n mp) -- R < C < Covector (TODO - is this ok?)

-- | General multiplication and sum
--
mul' :: [(ExpressionMap, Int)] -> (ExpressionMap, Int)
mul' es = addEdge mergedMap (shape, node)
  where
    elementType = highestElementType es
    shape = highestShape es
    node = Mul elementType . map snd $ es
    mergedMap = foldl1 IM.union . map fst $ es

sum' :: [(ExpressionMap, Int)] -> (ExpressionMap, Int)
sum' es = addEdge mergedMap (shape, node)
  where
    (mp, n) = head es
    elementType = retrieveElementType n mp
    shape = retrieveShape n mp
    node = Sum elementType . map snd $ es
    mergedMap = foldl1 IM.union . map fst $ es
