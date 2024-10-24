{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module StructureSpec where

import Commons
import Control.Applicative (liftA2)
import Data.List (sort)
import HashedExpression.Internal
  ( safeMerges,
    topologicalSort,
    topologicalSortManyRoots,
  )
import HashedExpression.Internal.Node
import Test.Hspec
import Test.QuickCheck
import Prelude hiding ((^))

-- |
noDuplicate :: (Eq a, Ord a) => [a] -> Bool
noDuplicate xs = sort (removeDuplicate xs) == sort xs

-- |
isAfter :: (Eq a) => [a] -> a -> a -> Bool
isAfter xs x y = filter (liftA2 (||) (== x) (== y)) xs == [y, x]

-- | Property of topological sort
prop_TopologicalSort :: ArbitraryExpr -> Bool
prop_TopologicalSort (ArbitraryExpr (mp, n)) =
  let sortedNodeId = topologicalSort (mp, n)
      dependencies n = opArgs $ retrieveOp n mp
      withChildren = zip sortedNodeId (map dependencies sortedNodeId)
      prop (nId, childrenIds) = all (isAfter sortedNodeId nId) childrenIds
   in noDuplicate sortedNodeId && all prop withChildren

---- |
----
prop_TopologicalSortManyRoots :: [ArbitraryExpr] -> Bool
prop_TopologicalSortManyRoots xs
  | length xs <= 1 = True
  | otherwise = noDuplicate sortedNodeId && all prop withChildren
  where
    (mergedMap, roots) = safeMerges $ map unArbitraryExpr xs
    sortedNodeId = topologicalSortManyRoots (mergedMap, roots)
    dependencies n = opArgs $ retrieveOp n mergedMap
    withChildren = zip sortedNodeId (map dependencies sortedNodeId)
    prop (nId, childrenIds) = all (isAfter sortedNodeId nId) childrenIds

spec :: Spec
spec =
  describe "Structure spec" $ do
    specify "Topological sort" $ property prop_TopologicalSort
    specify "Topological sort many roots" $
      property prop_TopologicalSortManyRoots
