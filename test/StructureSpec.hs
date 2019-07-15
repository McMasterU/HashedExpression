module StructureSpec where

import Commons
import Data.Map.Strict
import Data.Maybe (fromJust)
import HashedExpression
import HashedInterp
import HashedNode
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
import HashedSimplify
import HashedUtils
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

-- |
--
prop_StructureZeroC :: Expression Zero C -> Bool
prop_StructureZeroC exp
    | RealImag _ _ <- retrieveNode n mp = True
    | otherwise = False
  where
    (Expression n mp) = simplify exp

prop_StructureOneC :: Expression One C -> Bool
prop_StructureOneC exp
    | RealImag _ _ <- retrieveNode n mp = True
    | otherwise = False
  where
    (Expression n mp) = simplify exp

spec :: Spec
spec =
    describe "Structure spec" $ do
        specify "Simplify a Zero C would give the form x +: y" $
            property prop_StructureZeroC
        specify "Simplify a One C would give the form x +: y" $
            property prop_StructureOneC
