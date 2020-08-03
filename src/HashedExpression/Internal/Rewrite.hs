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
import Data.List (foldl', groupBy, sort, sortBy, sortOn)
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Data.STRef.Strict
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as Set
import Debug.Trace (traceShowId)
import GHC.Exts (sortWith)
import GHC.Stack (HasCallStack)
import HashedExpression.Internal
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Haha
import HashedExpression.Internal.Hash
import HashedExpression.Internal.Node
import HashedExpression.Internal.OperationSpec
import HashedExpression.Internal.Utils
import Prelude hiding ((^))

type Modification1  = (ExpressionMap, NodeID) -> State ExpressionMap NodeID

instance MonadExpression (State ExpressionMap) where
  introduceNode node = do
    mp <- get
    let nID = hashNode (checkHashFromMap mp) node
    modify' $ IM.insert nID node
    return nID
    
  getContextMap = get
