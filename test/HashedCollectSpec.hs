module HashedCollectSpec where

import Commons
import Control.Applicative (liftA2)
import Control.Monad (replicateM_, unless)
import qualified Data.IntMap.Strict as IM
import Data.List (group, sort)
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)
import HashedCollect
import HashedDerivative
import HashedExpression
import HashedInner (D_, ET_, topologicalSort, unwrap)
import HashedInterp
import HashedNode
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
import HashedSimplify
import HashedUtils
import HashedVar
import qualified Prelude
import Prelude hiding
    ( (*)
    , (+)
    , (-)
    , (/)
    , acos
    , acosh
    , asin
    , asinh
    , atan
    , atanh
    , const
    , cos
    , cosh
    , exp
    , log
    , negate
    , product
    , sin
    , sinh
    , sqrt
    , sum
    , sum
    , tan
    , tanh
    )
import Test.Hspec
import Test.QuickCheck

prop_DVarStayAlone :: Expression Zero R -> IO ()
prop_DVarStayAlone exp =
    unless property $ do
        showExpDebug exp
        showExpDebug collectedExp
        error "DVar not standing alone"
  where
    collectedExp@(Expression rootId mp) =
        collectDifferentials . exteriorDerivative allVars $ exp
    isDVarAlone nId
        | Const 0 <- retrieveNode nId mp = True
        | DVar _ <- retrieveNode nId mp = True
        | Mul Covector [_, cId] <- retrieveNode nId mp
        , DVar _ <- retrieveNode cId mp = True
        | InnerProd Covector _ cId <- retrieveNode nId mp
        , DVar _ <- retrieveNode cId mp = True
        | otherwise = traceShow (retrieveNode nId mp) False
    property =
        case retrieveNode rootId mp of
            Sum Covector ns -> all isDVarAlone ns
            _ -> isDVarAlone rootId

spec :: Spec
spec =
    describe "Hashed collect differentials spec" $ do
        specify "DVar should stay by itself after collect differentials" $ do
            property prop_DVarStayAlone
