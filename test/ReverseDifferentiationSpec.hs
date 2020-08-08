module ReverseDifferentiationSpec where

import CSimpleSpec (evaluateCodeC)
import Commons
import Control.Applicative (liftA2)
import qualified Data.IntMap.Strict as IM
import Data.List (group, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Tuple.Extra (thd3)
import Debug.Trace (traceShow)
import HashedExpression.Differentiation.Exterior
import HashedExpression.Differentiation.Exterior.Derivative
import HashedExpression.Differentiation.Reverse
import HashedExpression.Internal (D_, ET_, removeUnreachable, unwrap)
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Node
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

spec :: Spec
spec =
  describe "Reverse differentiation spec" $ do
    specify "TODO" $ do
      pending
