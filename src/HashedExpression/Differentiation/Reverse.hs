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
import HashedExpression.Differentiation.Reverse.State
import HashedExpression.Internal
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Hash
import HashedExpression.Internal.Context
import HashedExpression.Internal.Node
import HashedExpression.Internal.OperationSpec
import HashedExpression.Internal.Structure
import Prelude hiding ((^))

-- |
partialDerivativesMapByReverse ::
  Expression Scalar R ->
  (ExpressionMap, Map String NodeID)
partialDerivativesMapByReverse (Expression rootID mp) =
  let reverseTopoOrder = reverse $ topologicalSort (mp, rootID)
      init = ComputeDState mp IM.empty Map.empty
      -- Chain rule
      go :: ComputeReverseM ()
      go = forM_ reverseTopoOrder $ \nID -> do
        --- NodeID of derivative w.r.t to current node: d(f) / d(nID)
        dN <-
          if nID == rootID
            then sNum 1
            else do
              dPartsFromParent <- IM.lookup nID <$> gets computedPartsByParents
              -- Sum all the derivative parts incurred by its parents
              case dPartsFromParent of
                Just [d] -> from d
                Just ds -> perform (Nary specSum) ds
        curMp <- gets contextMap
        let (shape, et, op) = retrieveNode nID curMp
        let one = introduceNode (shape, R, Const 1)
        let zero = introduceNode (shape, R, Const 0)
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
                  dX <- from dN * from productRest
                  modifyComputedPartsByParents (IM.insertWith (++) x [dX])
                else do
                  dX <- from dN * conjugate (from productRest)
                  modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Power alpha x -> case et of
            R -> do
              dX <- sNum (fromIntegral alpha) *. (from dN * (from x ^ (alpha -1)))
              modifyComputedPartsByParents (IM.insertWith (++) x [dX])
            C -> do
              dX <- sNum (fromIntegral alpha) *. (from dN * conjugate (from x ^ (alpha - 1)))
              modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Neg x -> do
            dX <- negate $ from dN
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Scale scalar scalee -> do
            case (retrieveElementType scalar curMp, retrieveElementType scalee curMp) of
              (R, R) -> do
                -- for scalar
                dScalar <- from dN <.> from scalee
                modifyComputedPartsByParents (IM.insertWith (++) scalar [dScalar])
                -- for scalee
                dScalee <- from scalar *. from dN
                modifyComputedPartsByParents (IM.insertWith (++) scalee [dScalee])
              (R, C) -> do
                -- for scalar
                dScalar <- xRe (from scalee) <.> xRe (from dN) + xIm (from scalee) <.> xIm (from dN)
                modifyComputedPartsByParents (IM.insertWith (++) scalar [dScalar])
                -- for scalee
                dScalee <- from scalar *. from dN
                modifyComputedPartsByParents (IM.insertWith (++) scalee [dScalee])
              (C, C) -> do
                -- for scalar
                dScalar <- from dN <.> from scalee
                modifyComputedPartsByParents (IM.insertWith (++) scalar [dScalar])
                -- for scalee
                dScalee <- conjugate (from scalar) *. from dN
                modifyComputedPartsByParents (IM.insertWith (++) scalee [dScalee])
          Div x y -> do
            dX <- from dN / from y
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
            dY <- from dN * from x * (from y ^ (-2))
            modifyComputedPartsByParents (IM.insertWith (++) y [dY])
          Sqrt x -> do
            dX <- sNum 0.5 *. (from dN / sqrt (from x))
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Sin x -> do
            dX <- from dN * cos (from x)
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Cos x -> do
            dX <- from dN * (- sin (from x))
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Tan x -> do
            dX <- from dN * (cos (from x) ^ (-2))
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Exp x -> do
            dX <- from dN * exp (from x)
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Log x -> do
            dX <- from dN * (from x ^ (-1))
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Sinh x -> do
            dX <- from dN * cosh (from x)
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Cosh x -> do
            dX <- from dN * sinh (from x)
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Tanh x -> do
            dX <- from dN * (one - tanh (from x) ^ 2)
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Asin x -> do
            dX <- from dN * (one / sqrt (one - from x ^ 2))
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Acos x -> do
            dX <- from dN * (- one / sqrt (one - from x ^ 2))
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Atan x -> do
            dX <- from dN * (one / one + from x ^ 2)
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Asinh x -> do
            dX <- from dN * (one / sqrt (one + from x ^ 2))
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Acosh x -> do
            dX <- from dN * (one / sqrt (from x ^ 2 - one))
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          Atanh x -> do
            dX <- from dN * (one / sqrt (one - from x ^ 2))
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          RealImag re im -> do
            dRe <- xRe $ from dN
            modifyComputedPartsByParents (IM.insertWith (++) re [dRe])
            dIm <- xIm $ from dN
            modifyComputedPartsByParents (IM.insertWith (++) im [dIm])
          RealPart reIm -> do
            dReIm <- from dN +: zero
            modifyComputedPartsByParents (IM.insertWith (++) reIm [dReIm])
          ImagPart reIm -> do
            dReIm <- zero +: from dN
            modifyComputedPartsByParents (IM.insertWith (++) reIm [dReIm])
          InnerProd x y -> do
            case et of
              R -> do
                dX <- from dN *. from y
                modifyComputedPartsByParents (IM.insertWith (++) x [dX])
                dY <- from dN *. from x
                modifyComputedPartsByParents (IM.insertWith (++) y [dY])
              C -> do
                dX <- from dN *. from y
                modifyComputedPartsByParents (IM.insertWith (++) x [dX])
                dY <- conjugate (from dN) *. from x
                modifyComputedPartsByParents (IM.insertWith (++) y [dY])
          Piecewise marks condition branches -> do
            dCondition <- zero
            modifyComputedPartsByParents (IM.insertWith (++) condition [dCondition])
            let numBranches = length branches
            forM_ (zip branches [0 ..]) $ \(branch, idx) -> case et of
              R -> do
                associate <- piecewise marks (from condition) (replicate idx zero ++ [one] ++ replicate (numBranches - idx - 1) zero)
                dBranch <- from dN * from associate
                modifyComputedPartsByParents (IM.insertWith (++) branch [dBranch])
              C -> do
                let zeroC = zero +: zero
                let oneC = one +: zero
                associate <- piecewise marks (from condition) (replicate idx zeroC ++ [oneC] ++ replicate (numBranches - idx - 1) zeroC)
                dBranch <- from dN * conjugate (from associate)
                modifyComputedPartsByParents (IM.insertWith (++) branch [dBranch])
          Rotate amount x -> do
            dX <- perform (Unary (specRotate (map negate amount))) [dN]
            dX <- rotate (map negate amount) $ from dN
            modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          ReFT x
            | retrieveElementType x curMp == R -> do
              dX <- reFT (from dN)
              modifyComputedPartsByParents (IM.insertWith (++) x [dX])
            | otherwise -> do
              dX <- reFT (from dN) +: (- imFT (from dN))
              modifyComputedPartsByParents (IM.insertWith (++) x [dX])
          ImFT x
            | retrieveElementType x curMp == R -> do
              dX <- imFT (from dN)
              modifyComputedPartsByParents (IM.insertWith (++) x [dX])
            | otherwise -> do
              dX <- imFT (from dN) +: (- reFT (from dN))
              modifyComputedPartsByParents (IM.insertWith (++) x [dX])
      (_, res) = runState go init
   in (contextMap res, partialDerivativeMap res)
