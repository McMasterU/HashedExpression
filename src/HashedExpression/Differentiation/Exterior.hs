-- |
-- Module      :  HashedExpression.Differentiation.Exterior.Derivative
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
module HashedExpression.Differentiation.Exterior where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import HashedExpression.Differentiation.Exterior.Collect
import HashedExpression.Differentiation.Exterior.Derivative
import HashedExpression.Internal
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Node

{-# DEPRECATED
  partialDerivativesMapByExterior
  "Support for computing partial derivative by exterior method is dropped, consider using reverse method"
  #-}
partialDerivativesMapByExterior :: Expression Scalar R -> (ExpressionMap, Map String NodeID)
partialDerivativesMapByExterior exp =
  let (mp, rootID) = unwrap . collectDifferentials . exteriorDerivative $ exp
   in (mp, partialDerivativesMap (mp, rootID))

-- | Return a map from variable name to the corresponding partial derivative node id
--   Partial derivatives in Expression Scalar Covector should be collected before passing to this function
partialDerivativesMap :: (ExpressionMap, NodeID) -> Map String NodeID
partialDerivativesMap (dfMp, dfId) =
  case retrieveOp dfId dfMp of
    Sum ns | retrieveElementType dfId dfMp == Covector -> Map.fromList $ mapMaybe getPartial ns
    _ -> Map.fromList $ mapMaybe getPartial [dfId]
  where
    getPartial :: NodeID -> Maybe (String, NodeID)
    getPartial nId
      | MulD partialId dId <- retrieveOp nId dfMp,
        DVar name <- retrieveOp dId dfMp =
        Just (name, partialId)
      | InnerProdD partialId dId <- retrieveOp nId dfMp,
        DVar name <- retrieveOp dId dfMp =
        Just (name, partialId)
      | otherwise = Nothing
