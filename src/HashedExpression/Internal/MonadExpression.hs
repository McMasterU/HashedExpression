{-# OPTIONS_GHC -Wno-missing-methods #-}

-- |
-- Module      :  HashedExpression.Internal.MonadExpression
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
module HashedExpression.Internal.MonadExpression where

import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import GHC.Stack (HasCallStack)
import HashedExpression.Internal
import HashedExpression.Internal.Base
import HashedExpression.Internal.Hash
import HashedExpression.Internal.Node
import HashedExpression.Internal.OperationSpec

-- |
class (Monad m) => MonadExpression m where
  introduceNode :: Node -> m NodeID
  getContextMap :: m ExpressionMap

sum_ :: (MonadExpression m) => [m NodeID] -> m NodeID
sum_ ops = sequence ops >>= perform (Nary specSum)

product_ :: (MonadExpression m) => [m NodeID] -> m NodeID
product_ ops = sequence ops >>= perform (Nary specMul)

const_ :: (MonadExpression m) => Shape -> Double -> m NodeID
const_ shape val = introduceNode (shape, R, Const val)

instance (Monad m) => MonadExpression (StateT ExpressionMap m) where
  introduceNode node = do
    mp <- get
    let nID = hashNode (checkCollisionMap mp) node
    modify' $ IM.insert nID node
    return (NodeID nID)

  getContextMap = get

-- |
perform :: (MonadExpression m) => OperationSpec -> [NodeID] -> m NodeID
perform spec operandIDs = do
  mp <- getContextMap
  let operands = map (\nID -> (nID, retrieveNode nID mp)) operandIDs
  let node = createNode spec operands
  introduceNode node

instance (MonadExpression m) => Num (m NodeID) where
  (+) operand1 operand2 = do
    x <- operand1
    y <- operand2
    perform (Nary specSum) [x, y]
  negate operand = do
    x <- operand
    perform (Unary specNeg) [x]
  (*) :: m NodeID -> m NodeID -> m NodeID
  (*) operand1 operand2 = do
    x <- operand1
    y <- operand2
    perform (Nary specMul) [x, y]

instance (MonadExpression m) => Fractional (m NodeID) where
  (/) operand1 operand2 = do
    x <- operand1
    y <- operand2
    perform (Binary specDiv) [x, y]

  fromRational r = error "N/A"

instance (MonadExpression m) => Floating (m NodeID) where
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

instance (MonadExpression m) => PowerOp (m NodeID) Int where
  (^) operand alpha = do
    x <- operand
    perform (Unary (specPower alpha)) [x]

instance (MonadExpression m) => ScaleOp (m NodeID) (m NodeID) where
  scale operand1 operand2 = do
    x <- operand1
    y <- operand2
    perform (Binary specScale) [x, y]

instance (MonadExpression m) => ComplexRealOp (m NodeID) (m NodeID) where
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

instance (MonadExpression m) => InnerProductSpaceOp (m NodeID) (m NodeID) where
  (<.>) operand1 operand2 = do
    x <- operand1
    y <- operand2
    perform (Binary specInnerProd) [x, y]

instance (MonadExpression m) => RotateOp RotateAmount (m NodeID) where
  rotate ra operand = do
    x <- operand
    perform (Unary (specRotate ra)) [x]

instance (MonadExpression m) => PiecewiseOp (m NodeID) (m NodeID) where
  piecewise marks condition branches = do
    conditionID <- condition
    branchIDs <- sequence branches
    perform (ConditionAry (specPiecewise marks)) $ conditionID : branchIDs

instance (MonadExpression m) => FTOp (m NodeID) (m NodeID) where
  ft operand = do
    x <- operand
    perform (Unary specFT) [x]

  ift operand = do
    x <- operand
    perform (Unary specIFT) [x]

instance (MonadExpression m) => ProjectInjectOp [DimSelector] (m NodeID) (m NodeID) where
  project ss operand = do
    x <- operand
    perform (Unary (specProject ss)) [x]
  inject ss sub base = do
    x <- sub
    y <- base
    perform (Binary (specInject ss)) [x, y]

instance (MonadExpression m) => MatrixMulOp (m NodeID) (m NodeID) (m NodeID) where
  operand1 ** operand2 = do
    x <- operand1
    y <- operand2
    perform (Binary specMatMul) [x, y]

instance (MonadExpression m) => TransposeOp (m NodeID) (m NodeID) where
  transpose operand = do
    x <- operand
    perform (Unary specTranspose) [x]

coerceTo :: (MonadExpression m) => Shape -> m NodeID -> m NodeID
coerceTo shape operand = do
  x <- operand
  perform (Unary (specCoerce shape)) [x]
