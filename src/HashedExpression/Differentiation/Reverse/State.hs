-- |
-- Module      :  HashedExpression.Differentiation.Reverse.State
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- Helper for reverse accumulation method
module HashedExpression.Differentiation.Reverse.State
  ( from,
    addDerivative,
    setPartialDerivative,
    ComputeReverseM,
    ComputeDState (..),
  )
where

import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.List.HT (removeEach)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Stack (HasCallStack)
import HashedExpression.Internal.Base
import HashedExpression.Internal.Hash
import HashedExpression.Internal.MonadExpression
import Prelude hiding ((^))

data ComputeDState = ComputeDState
  { contextMap :: ExpressionMap,
    cumulativeDerivatives :: Map NodeID [NodeID], -- cumulative derivatives incurred by parents
    partialDerivativeMap :: Map String NodeID
  }

-- |
addDerivative :: NodeID -> NodeID -> ComputeReverseM ()
addDerivative x dx = modify' $ \s -> s {cumulativeDerivatives = Map.insertWith (++) x [dx] (cumulativeDerivatives s)}

-- |
setPartialDerivative :: (Map String NodeID -> Map String NodeID) -> ComputeReverseM ()
setPartialDerivative f = modify' $ \s -> s {partialDerivativeMap = f (partialDerivativeMap s)}

-- |
type ComputeReverseM a = State ComputeDState a

instance MonadExpression (State ComputeDState) where
  introduceNode node = do
    mp <- gets contextMap
    let nID = hashNode (checkCollisionMap mp) node
    modify' $ \s -> s {contextMap = IM.insert nID node (contextMap s)}
    return $ NodeID nID

  getContextMap = gets contextMap

-- |
from :: NodeID -> ComputeReverseM NodeID
from = return
