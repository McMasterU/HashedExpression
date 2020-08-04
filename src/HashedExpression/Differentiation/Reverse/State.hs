-- |
-- Module      :  HashedExpression.Differentiation.Exterior.Collect
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- Helper for reverse accumulation method
module HashedExpression.Differentiation.Reverse.State where

import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.List.HT (removeEach)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Stack (HasCallStack)
import HashedExpression.Internal
import HashedExpression.Internal.Context
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Hash
import HashedExpression.Internal.Node
import HashedExpression.Internal.OperationSpec
import HashedExpression.Internal.Structure
import Prelude hiding ((^))

data ComputeDState = ComputeDState
  { contextMap :: ExpressionMap,
    cumulativeDerivatives :: IM.IntMap [NodeID], -- cumulative derivatives incurred by parents
    partialDerivativeMap :: Map String NodeID
  }

-- |
modifyContextMap :: (ExpressionMap -> ExpressionMap) -> ComputeReverseM ()
modifyContextMap f = modify' $ \s -> s {contextMap = f (contextMap s)}

-- |
addDerivative :: NodeID -> NodeID -> ComputeReverseM ()
addDerivative x dx = modify' $ \s -> s {cumulativeDerivatives = IM.insertWith (++) x [dx] (cumulativeDerivatives s)}

-- |
modifyPartialDerivativeMap :: (Map String NodeID -> Map String NodeID) -> ComputeReverseM ()
modifyPartialDerivativeMap f = modify' $ \s -> s {partialDerivativeMap = f (partialDerivativeMap s)}

-- |
type ComputeReverseM a = State ComputeDState a

instance MonadExpression (State ComputeDState) where
  introduceNode node = do
    mp <- gets contextMap
    let nID = hashNode (checkHashFromMap mp) node
    modifyContextMap $ IM.insert nID node
    return nID

  getContextMap = gets contextMap

-- |
from :: NodeID -> ComputeReverseM NodeID
from = return

-- |
sNum :: Double -> ComputeReverseM NodeID
sNum val = introduceNode ([], R, Const val)
