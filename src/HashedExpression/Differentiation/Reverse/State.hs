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
import HashedExpression.Internal
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Hash
import HashedExpression.Internal.Node
import HashedExpression.Internal.OperationSpec
import HashedExpression.Internal.Structure
import Prelude hiding ((^))

data ComputeDState = ComputeDState
  { contextMap :: ExpressionMap,
    computedPartsByParents :: IM.IntMap [NodeID],
    partialDerivativeMap :: Map String NodeID
  }

modifyContextMap :: (ExpressionMap -> ExpressionMap) -> ComputeReverseM ()
modifyContextMap f = modify' $ \s -> s {contextMap = f (contextMap s)}

modifyComputedPartsByParents :: (IM.IntMap [NodeID] -> IM.IntMap [NodeID]) -> ComputeReverseM ()
modifyComputedPartsByParents f = modify' $ \s -> s {computedPartsByParents = f (computedPartsByParents s)}

modifyPartialDerivativeMap :: (Map String NodeID -> Map String NodeID) -> ComputeReverseM ()
modifyPartialDerivativeMap f = modify' $ \s -> s {partialDerivativeMap = f (partialDerivativeMap s)}

from :: NodeID -> ComputeReverseM NodeID
from = return

sNum :: Double -> ComputeReverseM NodeID
sNum val = introduceNode ([], R, Const val)

introduceNode :: Node -> ComputeReverseM NodeID
introduceNode node = do
  mp <- gets contextMap
  let nID = hashNode (checkHashFromMap mp) node
  modify' $ \s -> s {contextMap = IM.insert nID node mp}
  return nID

perform :: OperationSpec -> [NodeID] -> ComputeReverseM NodeID
perform spec operandIDs = do
  mp <- gets contextMap
  let operands = map (\nID -> (nID, retrieveNode nID mp)) operandIDs
  let (nID, node) = createEntry (checkHashFromMap mp) spec operands
  modify' $ \s -> s {contextMap = IM.insert nID node mp}
  return nID

type ComputeReverseM a = State ComputeDState a

instance Num (ComputeReverseM NodeID) where
  (+) operand1 operand2 =
    do
      x <- operand1
      y <- operand2
      perform (Nary specSum) [x, y]
  negate operand =
    do
      x <- operand
      perform (Unary specNeg) [x]
  (*) operand1 operand2 =
    do
      x <- operand1
      y <- operand2
      perform (Nary specSum) [x, y]

instance Fractional (ComputeReverseM NodeID) where
  (/) operand1 operand2 = do
    x <- operand1
    y <- operand2
    perform (Binary specDiv) [x, y]

  fromRational r = error "N/A"

instance Floating (ComputeReverseM NodeID) where
  sqrt operand = do
    x <- operand
    perform (Unary specSqrt) [x]
  exp operand = do
    x <- operand
    perform (Unary specExp) [x]
  log operand = do
    x <- operand
    perform (Unary specLog) [x]
  sin operand = do
    x <- operand
    perform (Unary specSin) [x]
  cos operand = do
    x <- operand
    perform (Unary specCos) [x]
  tan operand = do
    x <- operand
    perform (Unary specTan) [x]
  asin operand = do
    x <- operand
    perform (Unary specAsin) [x]
  acos operand = do
    x <- operand
    perform (Unary specAcos) [x]
  atan operand = do
    x <- operand
    perform (Unary specAtan) [x]
  sinh operand = do
    x <- operand
    perform (Unary specSinh) [x]
  cosh operand = do
    x <- operand
    perform (Unary specCosh) [x]
  tanh operand = do
    x <- operand
    perform (Unary specTanh) [x]
  asinh operand = do
    x <- operand
    perform (Unary specAsinh) [x]
  acosh operand = do
    x <- operand
    perform (Unary specAcosh) [x]
  atanh operand = do
    x <- operand
    perform (Unary specAtanh) [x]

instance PowerOp (ComputeReverseM NodeID) Int where
  (^) operand alpha = do
    x <- operand
    perform (Unary (specPower alpha)) [x]

instance VectorSpaceOp (ComputeReverseM NodeID) (ComputeReverseM NodeID) where
  scale operand1 operand2 = do
    x <- operand1
    y <- operand2
    perform (Binary specScale) [x, y]

instance ComplexRealOp (ComputeReverseM NodeID) (ComputeReverseM NodeID) where
  (+:) operand1 operand2 = do
    x <- operand1
    y <- operand2
    perform (Binary specRealImag) [x, y]
  xRe operand1 = do
    x <- operand1
    perform (Unary specRealPart) [x]
  xIm operand1 = do
    x <- operand1
    perform (Unary specImagPart) [x]
  conjugate operand = do
    x <- operand
    perform (Unary specConjugate) [x]

instance InnerProductSpaceOp (ComputeReverseM NodeID) (ComputeReverseM NodeID) (ComputeReverseM NodeID) where
  (<.>) operand1 operand2 = do
    x <- operand1
    y <- operand2
    perform (Binary specInnerProd) [x, y]

instance RotateOp RotateAmount (ComputeReverseM NodeID) where
  rotate ra operand = do
    x <- operand
    perform (Unary (specRotate ra)) [x]

instance PiecewiseOp (ComputeReverseM NodeID) (ComputeReverseM NodeID) where
  piecewise marks condition branches = do
    conditionID <- condition
    branchIDs <- sequence branches
    perform (ConditionAry (specPiecewise marks)) $ conditionID : branchIDs

reFT :: ComputeReverseM NodeID -> ComputeReverseM NodeID
reFT operand = do
  x <- operand
  perform (Unary specReFT) [x]

imFT :: ComputeReverseM NodeID -> ComputeReverseM NodeID
imFT operand = do
  x <- operand
  perform (Unary specImFT) [x]
