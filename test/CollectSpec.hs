module CollectSpec where

import Commons
import Control.Applicative (liftA2)
import Control.Monad (replicateM_, unless)
import qualified Data.IntMap.Strict as IM
import Data.List (group, sort)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Debug.Trace (traceShow)
import HashedExpression.Derivative
import HashedExpression.Internal.CollectDifferential
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Inner (D_, ET_, topologicalSort, unwrap)
import HashedExpression.Internal.Node
import HashedExpression.Internal.Normalize
import HashedExpression.Internal.Utils
import HashedExpression.Interp
import HashedExpression.Operation hiding (product, sum)
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import Test.Hspec
import Test.QuickCheck
import Var
import Prelude hiding ((^))
import qualified Prelude

prop_DVarStayAlone :: Expression Scalar R -> Bool
prop_DVarStayAlone exp = property
  where
    collectedExp@(Expression rootId mp) = collectDifferentials . exteriorDerivative allVars $ exp
    isDVarAlone nId
      | Const 0 <- retrieveNode nId mp = True
      | Mul Covector [_, cId] <- retrieveNode nId mp,
        DVar _ <- retrieveNode cId mp =
        True
      | InnerProd Covector _ cId <- retrieveNode nId mp,
        DVar _ <- retrieveNode cId mp =
        True
      | otherwise = False
    property =
      case retrieveNode rootId mp of
        Sum Covector ns -> all isDVarAlone ns
        _ -> isDVarAlone rootId

--        | DVar _ <- retrieveNode nId mp = True
prop_DVarAppearOnce :: Expression Scalar R -> Bool
prop_DVarAppearOnce exp = property
  where
    collectedExp@(Expression rootId mp) = collectDifferentials . exteriorDerivative allVars $ exp
    getDVarNames node
      | DVar name <- node = [name]
      | otherwise = []
    allDVarNames = concatMap (getDVarNames . snd) . IM.elems $ mp
    property = length allDVarNames == (Set.size . Set.fromList $ allDVarNames)

prop_DVarStayAloneWithOneR ::
  Expression Default1D R -> Expression Default1D R -> Bool
prop_DVarStayAloneWithOneR exp1 exp2 = property
  where
    exp = exp1 <.> exp2
    collectedExp@(Expression rootId mp) =
      collectDifferentials . exteriorDerivative allVars $ exp
    isDVarAlone nId
      | Const 0 <- retrieveNode nId mp = True
      | Mul Covector [_, cId] <- retrieveNode nId mp,
        DVar _ <- retrieveNode cId mp =
        True
      | InnerProd Covector _ cId <- retrieveNode nId mp,
        DVar _ <- retrieveNode cId mp =
        True
      | otherwise = False
    property =
      case retrieveNode rootId mp of
        Sum Covector ns -> all isDVarAlone ns
        _ -> isDVarAlone rootId

prop_DVarAppearOnceWithOneR ::
  Expression Default1D R -> Expression Default1D R -> Bool
prop_DVarAppearOnceWithOneR exp1 exp2 = property
  where
    exp = exp1 <.> exp2
    collectedExp@(Expression rootId mp) = collectDifferentials . exteriorDerivative allVars $ exp
    getDVarNames node
      | DVar name <- node = [name]
      | otherwise = []
    allDVarNames = concatMap (getDVarNames . snd) . IM.elems $ mp
    property = length allDVarNames == (Set.size . Set.fromList $ allDVarNames)

spec :: Spec
spec =
  describe "Hashed collect differentials spec" $ do
    specify "DVar should stay by itself after collect differentials" $
      property prop_DVarStayAlone
    specify "Each DVar appears only once after collect differentials" $
      property prop_DVarAppearOnce
    specify
      "DVar should stay by itself after collect differentials (from dot product)"
      $ property prop_DVarStayAloneWithOneR
    specify
      "Each DVar appears only once after collect differentials (from dot product)"
      $ property prop_DVarAppearOnceWithOneR
