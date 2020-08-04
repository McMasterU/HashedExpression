module CollisionSpec where

import Control.Monad (foldM, forM_, replicateM)
import Control.Monad.HT (nest)
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate, sort)
import qualified Data.Set as Set
import HashedExpression.Internal
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Hash
import HashedExpression.Prettify
import Test.HUnit (assertBool, assertEqual, (@=?))
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Handle hash collision spec" $
  specify "safe merge diff should resolve collision and preserve diff's semantic" $ do
    putStrLn ""
