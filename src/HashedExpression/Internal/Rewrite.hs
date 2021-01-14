module HashedExpression.Internal.Rewrite
  ( Rewrite,
    runRewrite,
    just,
    sum_,
    product_,
    const_,
    num_,
    matchNode,
    matchOp,
    withExpressionMap,
    Modification,
    toRecursiveTransformation,
  )
where

import Control.Monad.State.Strict
import Data.Function ((&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import HashedExpression.Internal
import HashedExpression.Internal.Base
import HashedExpression.Internal.MonadExpression
import HashedExpression.Internal.Node
import Prelude hiding ((^))

--------------------------------------------------------------------------------
newtype Rewrite a = Rewrite {unRewrite :: State ExpressionMap a} deriving (Functor, Applicative, Monad)

runRewrite :: Rewrite NodeID -> ExpressionMap -> RawExpr
runRewrite (Rewrite rw) mp = swap $ runState rw mp

--------------------------------------------------------------------------------

type Modification = NodeID -> Rewrite NodeID

matchNode :: NodeID -> (Node -> Rewrite a) -> Rewrite a
matchNode nID f = getContextMap >>= (f . retrieveNode nID)

matchOp :: NodeID -> (Op -> Rewrite a) -> Rewrite a
matchOp nID f = getContextMap >>= (f . retrieveOp nID)

withExpressionMap :: (ExpressionMap -> Rewrite a) -> Rewrite a
withExpressionMap f = getContextMap >>= f

toRecursiveTransformation ::
  Modification ->
  -- | resulting rule applied to every 'Node' bottom-up
  Transformation
toRecursiveTransformation rewrite exp@(mp, headN) = (finalMap, fromJust $ Map.lookup headN finalSub)
  where
    -------------------------------------------------------------------------------
    topoOrder :: [NodeID]
    topoOrder = topologicalSort exp
    -------------------------------------------------------------------------------
    f :: Map NodeID NodeID -> NodeID -> Rewrite (Map NodeID NodeID)
    f sub nID = do
      curMp <- getContextMap
      let oldNode = retrieveNode nID curMp
          newNode = mapNode (toTotal sub) oldNode
      cID <- if newNode == oldNode then pure nID else introduceNode newNode
      appliedRuleNodeID <- rewrite cID
      return $ Map.insert nID appliedRuleNodeID sub
    (finalSub, finalMap) = runState (unRewrite (foldM f Map.empty topoOrder)) mp

instance MonadExpression Rewrite where
  introduceNode node = Rewrite (introduceNode node)
  getContextMap = Rewrite getContextMap

just :: NodeID -> Rewrite NodeID
just = return

num_ :: Double -> Rewrite NodeID
num_ = const_ []
