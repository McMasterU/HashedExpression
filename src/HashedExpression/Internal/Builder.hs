module HashedExpression.Internal.Builder where

import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import HashedExpression.Internal.Base
import HashedExpression.Internal.MonadExpression
import Prelude hiding ((^))

newtype Build a = Build {unBuild :: State ExpressionMap a} deriving (Functor, Applicative, Monad)

instance MonadExpression Build where
  introduceNode node = Build (introduceNode node)
  getContextMap = Build getContextMap

type ExprBuilder = Build NodeID

buildExpr :: ExprBuilder -> Expr
buildExpr (Build exB) =
  let (nID, mp) = runState exB IM.empty
   in (mp, nID)

instance Show ExprBuilder where
  show = show . buildExpr

instance Eq ExprBuilder where
  e1 == e2 = (buildExpr e1) == (buildExpr e2)

instance Ord ExprBuilder where
  compare e1 e2 = compare (buildExpr e1) (buildExpr e2)
