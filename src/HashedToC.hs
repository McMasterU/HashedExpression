{-# LANGUAGE TupleSections #-}

module HashedToC where

import Data.Graph
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as Set
import HashedExpression hiding ((-))
import HashedHash
import HashedInner
import HashedNode
import HashedUtils

-- | Assigning
--
(<<-) :: String -> String -> String
(<<-) = (++)

-- | Slow topological sort
--
topologicalSort1 :: (ExpressionMap, Int) -> [Int]
topologicalSort1 (mp, n) = fst $ dfs n empty
  where
    dfs :: Int -> Set Int -> ([Int], Set Int)
    dfs u seen =
        let vs = filter (not . flip member seen) . nodeArgs $ retrieveNode u mp -- children that are not seen
            -- fold init, empty list and list of seen node
            init :: ([Int], Set Int)
            init = ([], seen)
            f :: ([Int], Set Int) -> Int -> ([Int], Set Int)
            f (ns, visited) v =
                let (xs, moreVisited) = dfs v visited
                 in (ns ++ xs, moreVisited)
            -- fold over
            (children, childrenSeen) = foldl' f init vs
         in (children ++ [u], insert u childrenSeen)

-- |
--
expressionEdges :: (ExpressionMap, Int) -> [(Int, Int)]
expressionEdges (mp, n) = Set.toList $ edges n
  where
    edges :: Int -> Set (Int, Int)
    edges nId =
        let args = nodeArgs $ retrieveNode nId mp
            thisNode = Set.fromList . map (nId, ) $ args
         in Set.unions $ thisNode : map edges args

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
