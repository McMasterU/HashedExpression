module HashedExpression.Derivative.Partial where

import qualified Data.Set as Set
import HashedExpression
import HashedExpression.Derivative
import HashedExpression.Internal
import HashedExpression.Internal.CollectDifferential
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Node
import HashedExpression.Internal.Utils
import HashedExpression.Prettify

-- TODO move to Derivative

-- | Compute partial derivative: ∂f / ∂x.
--   Automatically performs 'exteriorDerivative' w.r.t a single variable, uses 'collectDifferentials' to
--   factor terms and extracts the term corresponding to the partial derivative w.r.t the given variable,
--   returning that term alone as a 'Expression'
partialDerivative ::
  DimensionType d =>
  -- | base Expression
  Expression Scalar R ->
  -- | variable to take partial w.r.t
  Expression d R ->
  -- | term corresponding to partial
  Expression d R
partialDerivative f mx = case maybeVariable mx of
  Just (x, shape) ->
    let df = exteriorDerivative (Set.fromList [x]) f
        Expression nID mp = collectDifferentials df
     in case retrieveOp nID mp of
          DZero -> constWithShape shape 0
          MulD partialID _ -> wrap (mp, partialID)
          InnerProdD partialID _ -> wrap (mp, partialID)
          node -> error $ "This should not happen: " ++ show node
  Nothing -> error "2nd argument is not a variable"
