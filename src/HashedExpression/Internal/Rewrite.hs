module HashedExpression.Internal.Rewrite where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.ST.Strict
import Control.Monad.State.Strict
import Data.Array.MArray
import Data.Array.ST
import qualified Data.Array.Unboxed as UA
import Data.Data (Typeable)
import Data.Graph (buildG, topSort)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (find, foldl', groupBy, sort, sortBy, sortOn)
import Data.List.Extra (firstJust)
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Data.STRef.Strict
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as Set
import Debug.Trace (traceShowId)
import GHC.Exts (sortWith)
import GHC.Stack (HasCallStack)
import HashedExpression.Internal
import HashedExpression.Internal.Context
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Hash
import HashedExpression.Internal.Node
import HashedExpression.Internal.OperationSpec
import HashedExpression.Internal.Utils
import Prelude hiding ((^))

type Modification = (ExpressionMap, NodeID) -> State ExpressionMap NodeID

chainModifications :: [Modification] -> Modification
chainModifications rewrite expr = foldM f (snd expr) rewrite
  where
    f :: NodeID -> Modification -> State ExpressionMap NodeID
    f nID rewrite = do
      curM <- get
      rewrite (curM, nID)

toTransformation :: Modification -> Transformation
toTransformation modify exp =
  let (nID, newMp) = runState (modify exp) (fst exp)
   in (newMp, nID)

toRecursiveTransformation ::
  Modification ->
  -- | resulting rule applied to every 'Node'
  Transformation
toRecursiveTransformation smp exp@(mp, headN) = (finalMap, fromJust $ IM.lookup headN finalSub)
  where
    -------------------------------------------------------------------------------
    topoOrder :: [NodeID]
    topoOrder = topologicalSort exp
    -------------------------------------------------------------------------------
    f :: IM.IntMap NodeID -> NodeID -> State ExpressionMap (IM.IntMap NodeID)
    f sub nID = do
      curMp <- get
      let oldNode = retrieveNode nID curMp
          newNode = mapNode (toTotal sub) oldNode
      cID <- if newNode == oldNode then pure nID else introduceNode newNode
      updatedMp <- get
      appliedRuleNodeID <- smp (updatedMp, cID)
      return $ IM.insert nID appliedRuleNodeID sub
    (finalSub, finalMap) = runState (foldM f IM.empty topoOrder) mp

instance (Monad m) => MonadExpression (StateT ExpressionMap m) where
  introduceNode node = do
    mp <- get
    let nID = hashNode (checkHashFromMap mp) node
    modify' $ IM.insert nID node
    return nID

  getContextMap = get

just :: NodeID -> State ExpressionMap NodeID
just = return

sum_ :: [State ExpressionMap NodeID] -> State ExpressionMap NodeID
sum_ ops = sequence ops >>= perform (Nary specSum)

product_ :: [State ExpressionMap NodeID] -> State ExpressionMap NodeID
product_ ops = sequence ops >>= perform (Nary specMul)

const_ :: Shape -> Double -> State ExpressionMap NodeID
const_ shape val = introduceNode (shape, R, Const val)

num_ :: Double -> State ExpressionMap NodeID
num_ = const_ []
