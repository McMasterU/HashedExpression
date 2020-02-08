{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module StructureSpec where

import Commons
import Control.Applicative (liftA2)
import Control.Monad (replicateM_)
import qualified Data.IntMap.Strict as IM
import Data.List (group, sort)
import Data.Maybe (fromJust)
import HashedExpression.Internal.Expression

import HashedExpression.Internal.Inner
    ( D_
    , ET_
    , topologicalSort
    , topologicalSortManyRoots
    , unwrap
    )
import HashedExpression.Internal.Node
import HashedExpression.Internal.Normalize
import HashedExpression.Internal.Utils
import HashedExpression.Interp
import HashedExpression.Operation hiding (product, sum)
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import qualified Prelude
import Prelude hiding ((^))
import Test.Hspec
import Test.QuickCheck
import Var

-- |
--
noDuplicate :: (Eq a, Ord a) => [a] -> Bool
noDuplicate xs = sort (removeDuplicate xs) == sort xs

-- |
--
isAfter :: (Eq a) => [a] -> a -> a -> Bool
isAfter xs x y = filter (liftA2 (||) (== x) (== y)) xs == [y, x]

-- | Property of topological sort
--
prop_TopologicalSort :: ArbitraryExpresion -> Bool
prop_TopologicalSort (ArbitraryExpresion (Expression n mp)) =
    ok exp && ok (normalize exp)
  where
    exp = Expression n mp :: Expression D_ ET_
    ok exp =
        let sortedNodeId = topologicalSort (mp, n)
            dependencies n = nodeArgs $ retrieveNode n mp
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
    mergedMap = IM.unions . map (fst . getWrappedExp) $ xs
    roots = map (snd . getWrappedExp) xs
    sortedNodeId = topologicalSortManyRoots (mergedMap, roots)
    dependencies n = nodeArgs $ retrieveNode n mergedMap
    withChildren = zip sortedNodeId (map dependencies sortedNodeId)
    prop (nId, childrenIds) = all (isAfter sortedNodeId nId) childrenIds

-- |
--
prop_StructureScalarC :: Expression Scalar C -> Bool
prop_StructureScalarC exp
    | RealImag _ _ <- retrieveNode n mp = True
    | otherwise = False
  where
    (Expression n mp) = normalize exp

-- |
--
prop_StructureOneC :: Expression Default1D C -> Bool
prop_StructureOneC exp
    | RealImag _ _ <- retrieveNode n mp = True
    | otherwise = False
  where
    (Expression n mp) = normalize exp

spec :: Spec
spec =
    describe "Structure spec" $ do
        specify "Topological sort" $ property prop_TopologicalSort
        specify "Topological sort many roots" $
            property prop_TopologicalSortManyRoots
        specify "Normalize a Scalar C would give the form x +: y" $
            property prop_StructureScalarC
        specify "Normalize a One C would give the form x +: y" $
            property prop_StructureOneC
--        specify "Check size" $
--            replicateM_ 35 $ do
--                let sz = IM.size . exMap
--                exp1 <- generate (arbitrary :: Gen (Expression Scalar C))
--                exp2 <- generate (arbitrary :: Gen (Expression Scalar C))
--                measureTime $ do
--                    putStrLn "----------------------------"
--                    putStrLn $
--                        "Generate exp1 -> " ++
--                        show (sz exp1) ++ " subexpressions"
--                    putStrLn $
--                        "Generate exp2 -> " ++
--                        show (sz exp2) ++ " subexpressions"
--                    putStrLn $
--                        "Simplifing (exp1 * exp2) -> " ++
--                        show (sz $ normalize (exp1 * exp2)) ++ " subexpressions"
