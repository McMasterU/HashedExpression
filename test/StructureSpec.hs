{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module StructureSpec where

import Commons
import Control.Applicative (liftA2)
import Control.Monad (replicateM_)
import qualified Data.IntMap.Strict as IM
import Data.List (group, sort)
import Data.Maybe (fromJust)
import HashedExpression
import HashedInner (expressionEdges, topologicalSort, unwrap)
import HashedInterp
import HashedNode
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
import HashedSimplify
import HashedUtils
import qualified Prelude
import Prelude hiding
    ( (*)
    , (+)
    , (-)
    , (/)
    , acos
    , acosh
    , asin
    , asinh
    , atan
    , atanh
    , const
    , cos
    , cosh
    , exp
    , log
    , negate
    , product
    , sin
    , sinh
    , sqrt
    , sum
    , sum
    , tan
    , tanh
    )
import Test.Hspec
import Test.QuickCheck

-- | Property of topological sort
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

-- |
--
prop_StructureZeroC :: Expression Zero C -> Bool
prop_StructureZeroC exp
    | RealImag _ _ <- retrieveNode n mp = True
    | otherwise = False
  where
    (Expression n mp) = simplify exp

-- |
--
prop_StructureOneC :: Expression One C -> Bool
prop_StructureOneC exp
    | RealImag _ _ <- retrieveNode n mp = True
    | otherwise = False
  where
    (Expression n mp) = simplify exp

spec :: Spec
spec =
    describe "Structure spec" $ do
        specify "Topological sort" $ property prop_TopologicalSort
        specify "Simplify a Zero C would give the form x +: y" $
            property prop_StructureZeroC
        specify "Simplify a One C would give the form x +: y" $
            property prop_StructureOneC
        specify "Check size" $
            replicateM_ 35 $ do
                ArbitraryExpresion exp <-
                    generate (arbitrary :: Gen ArbitraryExpresion)
                measureTime $ do
                    putStrLn "----------------------------"
                    putStrLn $
                        "Generate exp -> " ++ show (sz exp) ++ " subexpressions"
                    putStrLn $
                        "Simplifing (exp) -> " ++
                        show (sz $ simplify exp) ++ " subexpressions"
