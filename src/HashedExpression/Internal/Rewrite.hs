module HashedExpression.Internal.Rewrite
  ( Rewrite,
    Modification,
    chainModifications,
    toTransformation,
    toRecursiveTransformation,
    runRewrite,
    just,
    sum_,
    product_,
    const_,
    num_,
  )
where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State.Strict
import qualified Data.Array.Unboxed as UA
import Data.Graph (buildG, topSort)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (find, foldl', groupBy, sort, sortBy, sortOn)
import Data.List.Extra (firstJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as Set
import Debug.Trace (traceShowId)
import GHC.Exts (sortWith)
import GHC.Stack (HasCallStack)
import HashedExpression.Internal
import HashedExpression.Internal.Base
import HashedExpression.Internal.MonadExpression
import HashedExpression.Internal.Node
import Prelude hiding ((^))

newtype Rewrite a = Rewrite {unRewrite :: State ExpressionMap a} deriving (Functor, Applicative, Monad)

runRewrite :: Rewrite NodeID -> (ExpressionMap, NodeID) -> (ExpressionMap, NodeID)
runRewrite (Rewrite rw) exp =
  let (nID, newMP) = runState rw (fst exp)
   in (newMP, nID)

type Modification = (ExpressionMap, NodeID) -> Rewrite NodeID

chainModifications :: [Modification] -> Modification
chainModifications rewrite expr = foldM f (snd expr) rewrite
  where
    f :: NodeID -> Modification -> Rewrite NodeID
    f nID rewrite = do
      curM <- getContextMap
      rewrite (curM, nID)

toTransformation :: Modification -> Transformation
toTransformation modify exp = runRewrite (modify exp) exp

toRecursiveTransformation ::
  Modification ->
  -- | resulting rule applied to every 'Node'
  Transformation
toRecursiveTransformation smp exp@(mp, headN) = (finalMap, fromJust $ Map.lookup headN finalSub)
  where
    -------------------------------------------------------------------------------
    topoOrder :: [NodeID]
    topoOrder = topologicalSort exp
    -------------------------------------------------------------------------------
    f :: Map NodeID NodeID -> NodeID -> Rewrite (Map NodeID NodeID)
    f sub nID = do
      curMp <- getContextMap
      let oldNode = retrieveNode nID curMp
          newNode = mapNode (toTotal sub) oldNode
      cID <- if newNode == oldNode then pure nID else introduceNode newNode
      updatedMp <- getContextMap
      appliedRuleNodeID <- smp (updatedMp, cID)
      return $ Map.insert nID appliedRuleNodeID sub
    (finalSub, finalMap) = runState (unRewrite (foldM f Map.empty topoOrder)) mp

instance MonadExpression Rewrite where
  introduceNode node = Rewrite (introduceNode node)
  getContextMap = Rewrite getContextMap

just :: NodeID -> Rewrite NodeID
just = return

num_ :: Double -> Rewrite NodeID
num_ = const_ []
