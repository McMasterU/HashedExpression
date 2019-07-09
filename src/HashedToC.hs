{-# LANGUAGE TupleSections #-}

module HashedToC where

import Data.Graph
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', tails)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as Set
import HashedExpression
    ( ET(..)
    , Expression(..)
    , ExpressionMap
    , Node
    , NumType
    , Shape
    )
import HashedHash
import HashedInner
import HashedNode
import HashedUtils

-- | Assigning
--
(<<-) :: String -> String -> String
(<<-) = (++)

-- |
--
sizeDouble = 8

-- | Topological sort the expression map, all the dependencies will appear before the depended node
--
topologicalSort :: (ExpressionMap, Int) -> [Int]
topologicalSort expr@(mp, n) =
    reverse . mapMaybe (`IM.lookup` vertices2nId) $ topSort graph
  where
    nId2vertices = IM.fromList $ zip (IM.keys mp) [0 ..]
    vertices2nId = IM.fromList $ zip [0 ..] (IM.keys mp)
    exNodeEdges = expressionEdges expr
    verticesEdges =
        mapMaybe
            (bringMaybeOut . mapBoth (`IM.lookup` nId2vertices))
            exNodeEdges
    graph = buildG (0, IM.size mp - 1) verticesEdges

-- | Get all the edges of the expressions
--
expressionEdges :: (ExpressionMap, Int) -> [(Int, Int)]
expressionEdges (mp, n) = Set.toList $ edges n
  where
    edges :: Int -> Set (Int, Int)
    edges nId =
        let args = nodeArgs $ retrieveNode nId mp
            thisNode = Set.fromList . map (nId, ) $ args
         in Set.unions $ thisNode : map edges args

-- | Mem map (offset, R or C, shape)
--
type MemMapEntry = (Int, EntryType, Shape)

data MemMap =
    MemMap
        { entryMap :: IntMap MemMapEntry -- node id -> (offset, size)
        , totalSize :: Int
        }
    deriving (Show, Eq, Ord)

data EntryType
    = EntryR
    | EntryC
    deriving (Show, Eq, Ord)

data PartC
    = Re
    | Im
    deriving (Show, Eq, Ord)

data LocalOffset
    = OffsetR [Int]
    | OffsetC PartC [Int]
    deriving (Show, Eq, Ord)

-- | Make a memory map from an expression
--
makeMemMap :: NumType et => Expression d et -> MemMap
makeMemMap expr@(Expression n mp) = uncurry MemMap $ foldl' f (IM.empty, 0) nIds
  where
    nIds = topologicalSort . unwrap $ expr
    f :: (IntMap MemMapEntry, Int) -> Int -> (IntMap MemMapEntry, Int)
    f (memMapSoFar, sizeSoFar) nId =
        let (shape, node) = retrieveInternal nId mp
            (nodeSz, mmShape)
                | nodeElementType node mp == R = (product shape, EntryR)
                | nodeElementType node mp == C = (2 * product shape, EntryC)
            newMemMap = IM.insert nId (sizeSoFar, mmShape, shape) memMapSoFar
         in (newMemMap, sizeSoFar + nodeSz)

memOffset :: MemMap -> Int -> LocalOffset -> Int
memOffset (MemMap entryMap _) nId lookupOffset
    | Just (globalOffset, entryType, entryShape) <- IM.lookup nId entryMap =
        case (entryType, lookupOffset) of
            (EntryR, OffsetR indices) ->
                globalOffset + computeInnerOffset entryShape indices
            (EntryC, OffsetC Re indices) ->
                globalOffset + computeInnerOffset (2 : entryShape) (0 : indices)
            (EntryC, OffsetC Im indices) ->
                globalOffset + computeInnerOffset (2 : entryShape) (1 : indices)
    | otherwise = error "node id is not in the mem map"

-- | Compute the inner offset:
-- e.g computeInnerOffset [3, 4, 5] [2, 0, 1] = 2 * (4 * 5) + 0 * (5) + 1
--
computeInnerOffset :: [Int] -> [Int] -> Int
computeInnerOffset shape indices
    | length shape == length indices =
        sum . zipWith (*) indices $ map product . tail . tails $ shape
    | otherwise =
        error "shape and indices are not compatible (not the same size)"
