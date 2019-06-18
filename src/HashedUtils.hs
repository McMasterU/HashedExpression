module HashedUtils where

import qualified Data.IntMap.Strict as IM
import HashedExpression
import HashedHash
import HashedNode

import qualified Data.Complex as DC

-- | Check if all elements of the list is equal
--
allEqual :: (Eq a) => [a] -> Bool
allEqual xs = and $ zipWith (==) (safeTail xs) xs
  where
    safeTail [] = []
    safeTail (x:xs) = xs

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
