module HashedExpression.Derivative.Partial where

import qualified Data.Set as Set
import HashedExpression.Derivative
import HashedExpression.Internal.CollectDifferential
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Inner
import HashedExpression.Internal.Node
import HashedExpression.Internal.Utils

-- TODO expose in stack

-- | Compute partial derivative: ∂f / ∂x
--   The second argument must be a variable
partialDerivative :: DimensionType d => Expression Scalar R -> Expression d R -> Expression d R
partialDerivative f mx = case maybeVariable mx of
  Just (x, shape) ->
    let df = exteriorDerivative (Set.fromList [x]) f
        Expression nID mp = collectDifferentials df
     in case retrieveNode nID mp of
          Const 0 -> constWithShape shape 0
          Mul Covector [partialID, _] -> wrap (mp, partialID)
          InnerProd Covector partialID _ -> wrap (mp, partialID)
          node -> error $ "This should not happen: " ++ show node
  Nothing -> error "2nd argument is not a variable"
