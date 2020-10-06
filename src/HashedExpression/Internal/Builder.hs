module HashedExpression.Internal.Builder (buildExpr, ExprBuilder) where

import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import HashedExpression.Internal.Base
import HashedExpression.Internal.MonadExpression
import Prelude hiding ((^))

newtype Builder a = Builder (State ExpressionMap a) deriving (Functor, Applicative, Monad)

instance MonadExpression Builder where
  introduceNode node = Builder (introduceNode node)
  getContextMap = Builder getContextMap

type ExprBuilder = Builder NodeID

buildExpr :: ExprBuilder -> RawExpr
buildExpr (Builder exB) =
  let (nID, mp) = runState exB IM.empty
   in (mp, nID)

instance Show ExprBuilder where
  show = show . buildExpr

instance Eq ExprBuilder where
  e1 == e2 = (buildExpr e1) == (buildExpr e2)

instance Ord ExprBuilder where
  compare e1 e2 = compare (buildExpr e1) (buildExpr e2)
