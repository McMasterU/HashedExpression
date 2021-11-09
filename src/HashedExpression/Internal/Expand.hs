module HashedExpression.Internal.Expand where

import HashedExpression.Internal
import HashedExpression.Internal.Base
import HashedExpression.Internal.MonadExpression
import HashedExpression.Internal.Node
import HashedExpression.Internal.Pattern
import HashedExpression.Internal.Rewrite
import HashedExpression.Utils

import Control.Monad ((<=<))
import Control.Monad.HT (chain)
import Data.Maybe (fromMaybe)
import Control.Functor.HT (mapSnd)

expand :: RawExpr -> RawExpr
expand = apply -- TODO: Find out why some nodes are unreachable but still exist
  where
    apply = toRecursiveTransformation . chain $ [ distribMulOverPlus ]

distribMulOverPlus :: Modification
distribMulOverPlus nID = withExpressionMap $ \mp ->
  let shape = retrieveShape nID mp
  in case retrieveOp nID mp of
    Mul ns
      | length ns > 1, -- don't distribute if there's only one variable
        let total = pullConstants mp ns |> maybe 1 (sum . snd),
        let sums = map (pullSumOperands mp) . filter (isSum mp) $ ns,
        let vars = filter (\x -> not $ isConstant mp x || isSum mp x) ns,
        not (null sums) -> do -- don't distribute if there are no sum terms
          consts <- const_ shape total
          let constNode = [consts | total /= 1]
          -- the filter is necessary to remove empty lists if constNode and vars have no terms
          -- Otherwise, this will throw a nasty empty head exception in distribSumOfProducts
          distribSumOfProducts $ filter (/= []) $ (constNode ++ vars) : sums
    _ -> just nID

-- Was initially: join . fmap distribMulOverPlus . product_ . map just
distribSumOfProducts :: [[NodeID]] -> Rewrite NodeID
distribSumOfProducts = sum_ . map (distribMulOverPlus <=< (product_ . map just)) . sequenceA
