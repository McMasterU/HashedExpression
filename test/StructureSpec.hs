{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module StructureSpec where

import Commons
import Control.Applicative (liftA2)
import Control.Monad (replicateM_)
import qualified Data.IntMap.Strict as IM
import Data.List (group, sort)
import Data.Maybe (fromJust)
import HashedExpression.Internal
  ( D_,
    ET_,
    safeMerges,
    topologicalSort,
    topologicalSortManyRoots,
    unwrap,
  )
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Node
import HashedExpression.Internal.Utils
import HashedExpression.Interp
import HashedExpression.Operation hiding (product, sum)
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import Test.Hspec
import Test.QuickCheck
import Var
import Prelude hiding ((^))
import qualified Prelude

-- |
noDuplicate :: (Eq a, Ord a) => [a] -> Bool
noDuplicate xs = sort (removeDuplicate xs) == sort xs

-- |
isAfter :: (Eq a) => [a] -> a -> a -> Bool
isAfter xs x y = filter (liftA2 (||) (== x) (== y)) xs == [y, x]

-- | Property of topological sort
prop_TopologicalSort :: ArbitraryExpresion -> Bool
prop_TopologicalSort (ArbitraryExpresion (Expression n mp)) =
  let sortedNodeId = topologicalSort (mp, n)
      dependencies n = opArgs $ retrieveOp n mp
      withChildren = zip sortedNodeId (map dependencies sortedNodeId)
      prop (nId, childrenIds) = all (isAfter sortedNodeId nId) childrenIds
   in noDuplicate sortedNodeId && all prop withChildren

---- |
----
prop_TopologicalSortManyRoots :: [ArbitraryExpresion] -> Bool
prop_TopologicalSortManyRoots xs
  | length xs <= 1 = True
  | otherwise = noDuplicate sortedNodeId && all prop withChildren
  where
    (mergedMap, roots) = safeMerges $ map getWrappedExp xs
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
