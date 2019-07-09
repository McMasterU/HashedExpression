{-# LANGUAGE ExistentialQuantification #-}

module HashedToCSpec where

import Commons
import Control.Applicative (liftA2)
import Data.List (sort)
import Debug.Trace (traceShowId)
import HashedExpression
import HashedInner
import HashedNode
import HashedToC
import Test.Hspec
import Test.QuickCheck

data ArbitraryExpresion =
    forall d et. ArbitraryExpresion (Expression d et)

instance Show ArbitraryExpresion where
    show (ArbitraryExpresion exp) = show exp

instance Arbitrary ArbitraryExpresion where
    arbitrary =
        let option1 =
                fmap ArbitraryExpresion (arbitrary :: Gen (Expression Zero R))
            option2 =
                fmap ArbitraryExpresion (arbitrary :: Gen (Expression Zero C))
         in oneof [option1, option2]

-- |
--
prop_TopologicalSort :: ArbitraryExpresion -> Bool
prop_TopologicalSort (ArbitraryExpresion (Expression n mp)) =
    noDuplicate && all prop withChildren
  where
    sortedNodeId = topologicalSort (mp, n)
    noDuplicate = sort (removeDuplicate sortedNodeId) == sort sortedNodeId
    isAfter n other =
        filter (liftA2 (||) (== n) (== other)) sortedNodeId == [other, n]
    dependencies n = nodeArgs $ retrieveNode n mp
    withChildren = zip sortedNodeId (map dependencies sortedNodeId)
    prop (nId, childrenIds) = all (nId `isAfter`) childrenIds

-- | Spec
--
spec :: Spec
spec =
    describe "Hashed To C spec" $ do
        specify "Topological sort" $ do property prop_TopologicalSort
        specify "Topological sort 2" $ do pending
