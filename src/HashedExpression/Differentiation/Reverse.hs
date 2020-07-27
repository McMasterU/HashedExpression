-- |
-- Module      :  HashedExpression.Differentiation.Exterior.Collect
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- Compute differentiations using reverse accumulation method
-- https://en.wikipedia.org/wiki/Automatic_differentiation#Reverse_accumulation
module HashedExpression.Differentiation.Reverse where

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

--child2ParentsMap :: (ExpressionMap, NodeID) -> IM.IntMap [NodeID]
--child2ParentsMap (mp, rootID) =
--  let parent2ChildEdges = expressionEdges (mp, rootID)
--   in foldl' (\acc (parent, child) -> IM.insertWith (++) child [parent] acc) IM.empty parent2ChildEdges

data ComputeDState = ComputeDState
  { contextMap :: ExpressionMap,
    computedPartsByParents :: IM.IntMap [NodeID],
    partialDerivativeMap :: Map String NodeID
  }

type ComputeReverseM a = State ComputeDState a

modifyContextMap :: (ExpressionMap -> ExpressionMap) -> ComputeReverseM ()
modifyContextMap f = modify' $ \s -> s {contextMap = f (contextMap s)}

modifyComputedPartsByParents :: (IM.IntMap [NodeID] -> IM.IntMap [NodeID]) -> ComputeReverseM ()
modifyComputedPartsByParents f = modify' $ \s -> s {computedPartsByParents = f (computedPartsByParents s)}

modifyPartialDerivativeMap :: (Map String NodeID -> Map String NodeID) -> ComputeReverseM ()
modifyPartialDerivativeMap f = modify' $ \s -> s {partialDerivativeMap = f (partialDerivativeMap s)}

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

compute ::
  Expression Scalar R ->
  (ExpressionMap, Map String NodeID)
compute (Expression rootID mp) =
  let reverseTopoOrder = reverse $ topologicalSort (mp, rootID)
      init = ComputeDState mp IM.empty Map.empty
      -- Chain rule
      go :: ComputeReverseM ()
      go = forM_ reverseTopoOrder $ \nID -> do
        --- NodeID of derivative w.r.t to current node: d(f) / d(nID)
        dN <-
          if nID == rootID
            then introduceNode ([], R, Const 1)
            else do
              dPartsFromParent <- IM.lookup nID <$> gets computedPartsByParents
              -- Sum all the derivative parts incurred by its parents
              case dPartsFromParent of
                Just [d] -> pure d
                Just ds -> perform (Nary specSum) ds
        curMp <- gets contextMap
        let (shape, et, op) = retrieveNode nID curMp
        case op of
          Var name -> modifyPartialDerivativeMap (Map.insert name dN)
          Const _ -> return ()
          Sum args -> do
            forM_ args $ \x -> do
              let dX = dN
              modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Mul args -> do
            forM_ (removeEach args) $ \(x, rest) -> do
              productRest <- perform (Nary specMul) rest
              if et == R
                then do
                  dX <- perform (Nary specMul) [dN, productRest]
                  modifyComputedPartsByParents (IM.insertWith (++) x [dX])
                else do
                  conjugateRest <- perform (Unary specConjugate) [productRest]
                  dX <- perform (Nary specMul) [dN, conjugateRest]
                  modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Power alpha x -> do
            rest <- perform (Unary (specPower (alpha - 1))) [x]
            conjugateRest <- perform (Unary specConjugate) [rest]
            dX <- perform (Nary specMul) [dN, conjugateRest]
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Neg x -> do
            dX <- perform (Unary specNeg) [dN]
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Scale scalar scalee -> do
            case (retrieveElementType scalar curMp, retrieveElementType scalee curMp) of
              (R, R) -> do
                -- for scalar
                dScalar <- perform (Binary specInnerProd) [dN, scalee]
                modifyComputedPartsByParents (IM.insertWith (++) scalar [dScalar])
                -- for scalee
                dScalee <- perform (Binary specScale) [scalar, dN]
                modifyComputedPartsByParents (IM.insertWith (++) scalee [dScalee])
              (R, C) -> do
                -- for scalar
                reScalee <- perform (Unary specRealPart) [scalar]
                imScalee <- perform (Unary specImagPart) [scalar]
                reDN <- perform (Unary specRealPart) [dN]
                imDN <- perform (Unary specImagPart) [dN]
                sm1 <- perform (Binary specInnerProd) [reScalee, reDN]
                sm2 <- perform (Binary specInnerProd) [imScalee, imDN]
                dScalar <- perform (Nary specSum) [sm1, sm2]
                modifyComputedPartsByParents (IM.insertWith (++) scalar [dScalar])
                -- for scalee
                dScalee <- perform (Binary specScale) [scalar, dN]
                modifyComputedPartsByParents (IM.insertWith (++) scalee [dScalee])
              (C, C) -> do
                dScalar <- perform (Binary specInnerProd) [dN, scalee]
                modifyComputedPartsByParents (IM.insertWith (++) scalar [dScalar])
                conjugateScalar <- perform (Unary specConjugate) [dScalar]
                dScalee <- perform (Binary specScale) [conjugateScalar, dN]
                modifyComputedPartsByParents (IM.insertWith (++) scalee [dScalee])
          Div x y -> do
            dX <- perform (Binary specDiv) [dN, y]
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
            temp1 <- perform (Unary (specPower (-2))) [y]
            temp2 <- perform (Unary specNeg) [temp1]
            dY <- perform (Nary specMul) [dN, x, temp2]
            modifyComputedPartsByParents (IM.insertWith (++) y [dY])
          Sqrt {} -> undefined
          Sin {} -> undefined
          Cos {} -> undefined
          Tan {} -> undefined
          Exp {} -> undefined
          Log {} -> undefined
          Sinh {} -> undefined
          Cosh {} -> undefined
          Tanh {} -> undefined
          Asin {} -> undefined
          Acos {} -> undefined
          Atan {} -> undefined
          Asinh {} -> undefined
          Acosh {} -> undefined
          Atanh {} -> undefined
          --
          RealImag re im -> do
            dRe <- perform (Unary specRealPart) [dN]
            dIm <- perform (Unary specImagPart) [dN]
            modifyComputedPartsByParents (IM.insertWith (++) re [dRe])
            modifyComputedPartsByParents (IM.insertWith (++) im [dIm])
          RealPart reIm -> do
            zeroIm <- introduceNode (shape, R, Const 0)
            dReIm <- perform (Binary specRealImag) [dN, zeroIm]
            modifyComputedPartsByParents (IM.insertWith (++) reIm [dReIm])
          ImagPart reIm -> do
            zeroRe <- introduceNode (shape, R, Const 0)
            dReIm <- perform (Binary specRealImag) [zeroRe, dN]
            modifyComputedPartsByParents (IM.insertWith (++) reIm [dReIm])
          InnerProd x y -> do
            case et of
              R -> do
                dX <- perform (Binary specScale) [dN, y]
                modifyComputedPartsByParents (IM.insertWith (++) x [dX])
                dY <- perform (Binary specScale) [dN, x]
                modifyComputedPartsByParents (IM.insertWith (++) y [dY])
              C -> do
                dX <- perform (Binary specScale) [dN, y]
                modifyComputedPartsByParents (IM.insertWith (++) x [dX])
                conjugateDN <- perform (Unary specConjugate) [dN]
                dY <- perform (Binary specScale) [conjugateDN, x]
                modifyComputedPartsByParents (IM.insertWith (++) y [dY])
          Piecewise {} -> undefined
          Rotate amount x -> do
            dX <- perform (Unary (specRotate (map negate amount))) [dN]
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          ReFT {} -> undefined
          ImFT {} -> undefined
      (_, res) = runState go init
   in --      res = flip runStateT init $ do
      --        forM_ reverseTopoOrder $ \nID -> do
      --          undefined

      --      update :: (ExpressionMap, IM.IntMap [NodeID], IM.IntMap NodeID) -> NodeID -> (ExpressionMap, IM.IntMap [NodeID], IM.IntMap NodeID)
      --      update (accMp, accsByID, dByID) nID =
      --        let (shape, et, op) = retrieveNode nID accMp
      --            derivativeCurrent
      --              | nID == rootID = undefined
      --              | otherwise = case IM.lookup nID accsByID of
      --                Just [d] -> d
      --                Just ds -> undefined
      --         in undefined
      (contextMap res, partialDerivativeMap res)

---
--             Re                  Im
---           (a + bi) <.> (c + di)
--       (a <.> c + b <.> d) + (b <.> c - a <.> d)
--        (Re *. c - Im *. d)    (Re *. d + IM *. c)

--  (Re *. a + Im *. b) + (Re *. b - Im *. a)
--

-----------------------------------
