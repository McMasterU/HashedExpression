module CollectSpec where

import Commons
import Control.Applicative (liftA2)
import Control.Monad (replicateM_, unless)
import qualified Data.IntMap.Strict as IM
import Data.List (group, sort)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Tuple.Extra (thd3)
import Debug.Trace (traceShow)
import HashedExpression.Differentiation.Exterior.Collect
import HashedExpression.Differentiation.Exterior.Derivative
import HashedExpression.Internal (D_, ET_, unwrap)
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Node
import HashedExpression.Internal.Normalize
import HashedExpression.Internal.Structure
import HashedExpression.Internal.Utils
import HashedExpression.Interp
import HashedExpression.Operation hiding (product, sum)
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import Test.HUnit (assertBool)
import Test.Hspec
import Test.QuickCheck
import Var
import Prelude hiding ((^))
import qualified Prelude

allDVarNames :: ExpressionMap -> [String]
allDVarNames = concatMap (getDVarNames . thd3) . IM.elems
  where
    getDVarNames node
      | DVar name <- node = [name]
      | otherwise = []

prop_DVarStayAlone :: Expression Scalar R -> Expectation
prop_DVarStayAlone exp = do
  property
  where
    collectedExp@(Expression rootId mp) = collectDifferentials . exteriorDerivative allVars $ exp
    isDVarAlone nId
      | DZero <- retrieveOp nId mp = True
      | MulD _ cId <- retrieveOp nId mp,
        retrieveElementType nId mp == Covector,
        DVar _ <- retrieveOp cId mp =
        True
      | InnerProdD _ cId <- retrieveOp nId mp,
        retrieveElementType nId mp == Covector,
        DVar _ <- retrieveOp cId mp =
        True
      | otherwise = traceShow ("here" ++ show (retrieveOp nId mp)) False
    property =
      case retrieveOp rootId mp of
        Sum ns -> assertBool " " $ all isDVarAlone ns
        _ -> assertBool "" $ isDVarAlone rootId

prop_DVarAppearOnce :: Expression Scalar R -> Expectation
prop_DVarAppearOnce exp = do
  length (allDVarNames mp) `shouldBe` (Set.size . Set.fromList $ allDVarNames mp)
  where
    Expression rootId mp = collectDifferentials . exteriorDerivative allVars $ exp

prop_DVarStayAloneWithOneR ::
  Expression Default1D R -> Expression Default1D R -> Expectation
prop_DVarStayAloneWithOneR exp1 exp2 = prop_DVarStayAlone (exp1 <.> exp2)

prop_DVarAppearOnceWithOneR ::
  Expression Default1D R -> Expression Default1D R -> Expectation
prop_DVarAppearOnceWithOneR exp1 exp2 = do
  prop_DVarAppearOnce (exp1 <.> exp2)

spec :: Spec
spec =
  describe "Hashed collect differentials spec" $ do
    specify "DVar should stay by itself after collect differentials" $
      property prop_DVarStayAlone
    specify "Each DVar appears only once after collect differentials" $
      property prop_DVarAppearOnce
    specify "DVar should stay by itself after collect differentials (from dot product)" $
      property prop_DVarStayAloneWithOneR
    specify "Each DVar appears only once after collect differentials (from dot product)" $
      property prop_DVarAppearOnceWithOneR
