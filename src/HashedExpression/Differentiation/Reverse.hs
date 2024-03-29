-- | Module      :  HashedExpression.Differentiation.Reverse
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
import Data.List.HT (removeEach)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import HashedExpression.Differentiation.Reverse.State
import HashedExpression.Internal
import HashedExpression.Internal.Base
import HashedExpression.Internal.MonadExpression
import HashedExpression.Internal.Node
import HashedExpression.Internal.OperationSpec
import Prelude hiding ((**), (^))

-- |
partialDerivativesMap :: IsScalarReal e => e -> (ExpressionMap, Map String NodeID)
partialDerivativesMap scalarRealExp =
  let (mp, rootID) = asScalarRealRawExpr scalarRealExp
      reverseTopoOrder = reverse $ topologicalSort (mp, rootID)
      init = ComputeDState mp Map.empty Map.empty
      num_ :: Double -> ComputeReverseM NodeID
      num_ = const_ []
      -- Chain rule
      go :: ComputeReverseM ()
      go = forM_ reverseTopoOrder $ \nID -> do
        --- NodeID of derivative w.r.t to current node: d(f) / d(nID)
        dN <-
          if nID == rootID
            then num_ 1
            else do
              derivativeParts <- Map.lookup nID <$> gets cumulativeDerivatives
              -- Sum all the derivative parts incurred by its parents
              case derivativeParts of
                Just [d] -> from d
                Just ds -> perform (Nary specSum) ds
        curMp <- gets contextMap
        let (shape, et, op) = retrieveNode nID curMp
        let one = introduceNode (shape, R, Const 1)
        let zero = introduceNode (shape, R, Const 0)
        case op of
          Var name -> setPartialDerivative (Map.insert name dN)
          Param _ -> return ()
          Const _ -> return ()
          Sum args -> do
            forM_ args $ \x -> do
              let dX = dN
              addDerivative x dX
          Mul args -> do
            forM_ (removeEach args) $ \(x, rest) -> do
              productRest <- product_ $ map from rest
              case et of
                R -> do
                  dX <- from dN * from productRest
                  addDerivative x dX
                C -> do
                  dX <- from dN * conjugate (from productRest)
                  addDerivative x dX
          Power alpha x -> case et of
            R -> do
              dX <- num_ (fromIntegral alpha) *. (from dN * (from x ^ (alpha -1)))
              addDerivative x dX
            C -> do
              dX <- num_ (fromIntegral alpha) *. (from dN * conjugate (from x ^ (alpha - 1)))
              addDerivative x dX
          Neg x -> do
            dX <- negate $ from dN
            addDerivative x dX
          Scale scalar scalee -> do
            case (retrieveElementType scalar curMp, retrieveElementType scalee curMp) of
              (R, R) -> do
                -- for scalar
                dScalar <- from dN <.> from scalee
                addDerivative scalar dScalar
                -- for scalee
                dScalee <- from scalar *. from dN
                addDerivative scalee dScalee
              (R, C) -> do
                -- for scalar
                dScalar <- xRe (from scalee) <.> xRe (from dN) + xIm (from scalee) <.> xIm (from dN)
                addDerivative scalar dScalar
                -- for scalee
                dScalee <- from scalar *. from dN
                addDerivative scalee dScalee
              (C, C) -> do
                -- for scalar
                dScalar <- from dN <.> from scalee
                addDerivative scalar dScalar
                -- for scalee
                dScalee <- conjugate (from scalar) *. from dN
                addDerivative scalee dScalee
          Div x y -> do
            dX <- from dN / from y
            addDerivative x dX
            dY <- from dN * from x * (from y ^ (-2))
            addDerivative y dY
          Sqrt x -> do
            dX <- num_ 0.5 *. (from dN / sqrt (from x))
            addDerivative x dX
          Sin x -> do
            dX <- from dN * cos (from x)
            addDerivative x dX
          Cos x -> do
            dX <- from dN * (- sin (from x))
            addDerivative x dX
          Tan x -> do
            dX <- from dN * (cos (from x) ^ (-2))
            addDerivative x dX
          Exp x -> do
            dX <- from dN * exp (from x)
            addDerivative x dX
          Log x -> do
            dX <- from dN * (from x ^ (-1))
            addDerivative x dX
          Sinh x -> do
            dX <- from dN * cosh (from x)
            addDerivative x dX
          Cosh x -> do
            dX <- from dN * sinh (from x)
            addDerivative x dX
          Tanh x -> do
            dX <- from dN * (one - tanh (from x) ^ 2)
            addDerivative x dX
          Asin x -> do
            dX <- from dN * (one / sqrt (one - from x ^ 2))
            addDerivative x dX
          Acos x -> do
            dX <- from dN * (- one / sqrt (one - from x ^ 2))
            addDerivative x dX
          Atan x -> do
            dX <- from dN * (one / one + from x ^ 2)
            addDerivative x dX
          Asinh x -> do
            dX <- from dN * (one / sqrt (one + from x ^ 2))
            addDerivative x dX
          Acosh x -> do
            dX <- from dN * (one / sqrt (from x ^ 2 - one))
            addDerivative x dX
          Atanh x -> do
            dX <- from dN * (one / sqrt (one - from x ^ 2))
            addDerivative x dX
          RealImag re im -> do
            dRe <- xRe $ from dN
            addDerivative re dRe
            dIm <- xIm $ from dN
            addDerivative im dIm
          Conjugate x -> do
            dX <- conjugate (from dN)
            addDerivative x dX
          RealPart reIm -> do
            dReIm <- from dN +: zero
            addDerivative reIm dReIm
          ImagPart reIm -> do
            dReIm <- zero +: from dN
            addDerivative reIm dReIm
          InnerProd x y -> do
            case et of
              R -> do
                dX <- from dN *. from y
                addDerivative x dX
                dY <- from dN *. from x
                addDerivative y dY
              C -> do
                dX <- from dN *. from y
                addDerivative x dX
                dY <- conjugate (from dN) *. from x
                addDerivative y dY
          Piecewise marks condition branches -> do
            dCondition <- zero
            addDerivative condition dCondition
            let numBranches = length branches
            forM_ (zip branches [0 ..]) $ \(branch, idx) -> case et of
              R -> do
                associate <- piecewise marks (from condition) (replicate idx zero ++ [one] ++ replicate (numBranches - idx - 1) zero)
                dBranch <- from dN * from associate
                addDerivative branch dBranch
              C -> do
                let zeroC = zero +: zero
                let oneC = one +: zero
                associate <- piecewise marks (from condition) (replicate idx zeroC ++ [oneC] ++ replicate (numBranches - idx - 1) zeroC)
                dBranch <- from dN * conjugate (from associate)
                addDerivative branch dBranch
          Rotate amount x -> do
            dX <- rotate (map negate amount) $ from dN
            addDerivative x dX
          FT x -> do
            let sz = fromIntegral $ product shape
            dX <- num_ sz *. ift (from dN)
            addDerivative x dX
          IFT x -> do
            let sz = fromIntegral $ product shape
            dX <- num_ (1.0 / sz) *. ft (from dN)
            addDerivative x dX
          Project dss x -> do
            let zeroX = introduceNode (retrieveShape x curMp, R, Const 0)
            case et of
              R -> do
                dX <- inject dss (from dN) zeroX
                addDerivative x dX
              C -> do
                dX <- inject dss (from dN) (zeroX +: zeroX)
                addDerivative x dX
          Inject dss x y -> do
            let zeroX = introduceNode (retrieveShape x curMp, R, Const 0)
            dX <- project dss (from dN)
            addDerivative x dX
            case et of
              R -> do
                dY <- inject dss zeroX (from dN)
                addDerivative y dY
              C -> do
                dY <- inject dss (zeroX +: zeroX) (from dN)
                addDerivative y dY
          MatMul x y ->
            case (retrieveShape x curMp, retrieveShape y curMp) of
              ([m, n], [_n, p]) -> do
                -- mn np mp
                dX <- from dN ** transpose (from y)
                addDerivative x dX
                dY <- transpose (from x) ** from dN
                addDerivative y dY
              ([m, n], [_n]) -> do
                -- mn n m
                dX <- coerceTo [m, 1] (from dN) ** coerceTo [1, n] (from y)
                addDerivative x dX
                dY <- transpose (from x) ** from dN
                addDerivative y dY
          Transpose x -> do
            dX <- transpose $ from dN
            addDerivative x dX
      (_, res) = runState go init
   in (contextMap res, partialDerivativeMap res)
