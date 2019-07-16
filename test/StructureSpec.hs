module StructureSpec where

import Commons
import Control.Monad (replicateM_)
import Data.IntMap.Strict as IM
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
--        specify "Simplify a One C would give the form x +: y" $
--            property prop_StructureOneC
--        specify "Check size" $
--            replicateM_ 10 $ do
--                let sz = IM.size . exMap
--                exp1 <- generate (arbitrary :: Gen (Expression Zero C))
--                exp2 <- generate (arbitrary :: Gen (Expression Zero C))
--                measureTime $ do
--                    putStrLn "----------------------------"
--                    putStrLn $
--                        "Generate exp1 -> " ++
--                        show (sz exp1) ++ " subexpressions"
--                    putStrLn $
--                        "Generate exp2 -> " ++
--                        show (sz exp2) ++ " subexpressions"
--                    putStrLn $
--                        "Simplifing (exp1 * exp2) -> " ++
--                        show (sz $ simplify (exp1 * exp2)) ++ " subexpressions"
