module EvalSpec where

import Data.Array
import Test.Hspec
import HashedInterp
import HashedExpression
import HashedOperation
import Prelude hiding ((+), (*))
import qualified Prelude as P
import Data.Complex

spec :: Spec
spec = do
    describe "eval basics" $ do
        specify "real scalar 01" $ do
            let x = var "x"
                y = var "y"
                valMap = subs [("x", 1), ("y", 3)] []
            eval valMap x `shouldBe` 1
            eval valMap y `shouldBe` 3
            eval valMap (x + y) `shouldBe` 4
            eval valMap (x * y) `shouldBe` 3
            eval valMap (x .* y) `shouldBe` 3
            eval valMap (x <.> y) `shouldBe` 3
        specify "real scalar 02" $ do
            let x = var "x"
                y = var "y"
                valMap = subs [("x", 100), ("y", 300)] []
            eval valMap x `shouldBe` 100
            eval valMap y `shouldBe` 300
            eval valMap (x + y) `shouldBe` 400
            eval valMap (x * y) `shouldBe` 30000
            eval valMap (x .* y) `shouldBe` 30000
            eval valMap (x <.> y) `shouldBe` 30000
        specify "complex scalar 01" $ do
            let x = var "x"
                y = var "y"
                z1 = x +: y
                z2 = y +: x
                valMap = subs [("x", 1), ("y", 3)] []
            eval valMap z1 `shouldBe` (1 :+ 3)
            eval valMap z2 `shouldBe` (3 :+ 1)
            eval valMap (y * z1) `shouldBe` 3 P.* (1 :+ 3)
            eval valMap (z2 * z1) `shouldBe` (3 :+ 1) P.* (1 :+ 3)
            eval valMap (z2 .* z1) `shouldBe` (3 :+ 1) P.* (1 :+ 3)
            eval valMap (z2 <.> z1) `shouldBe` (3 :+ 1) P.* (1 :+ 3)
        specify "real 1d 01" $ do
            let x = var1d 4 "x"
                y = var1d 4 "y"
                s = var "s"
                valMap = subs [("s", 3)] [("x", listArray (0, 3) [1, 1, 1, 1]), ("y", listArray (0, 3) [1, 1, 1, 1])]
            elems (eval valMap x) `shouldBe` [1, 1, 1, 1]
            elems (eval valMap (x + y)) `shouldBe` [2, 2, 2, 2]
            elems (eval valMap (s * x)) `shouldBe` [3, 3, 3, 3]
            elems (eval valMap (x .* y)) `shouldBe` [1, 1, 1, 1]
            eval valMap (x <.> y) `shouldBe` 4
        specify "complex 1d 01" $ do
            let x = var1d 4 "x"
                y = var1d 4 "y"
                s = var "s"
                valMap = subs [("s", 3)] [("x", listArray (0, 3) [1, 1, 1, 1]), ("y", listArray (0, 3) [2, 2, 2, 2])]
                z = x +: y
                listZ = [1 :+ 2, 1 :+ 2, 1 :+ 2, 1 :+ 2]
            elems (eval valMap z) `shouldBe` listZ
            elems (eval valMap (z + z)) `shouldBe` zipWith (P.+) listZ listZ
            elems (eval valMap (s * z)) `shouldBe` map (P.* 3) listZ
            elems (eval valMap (z .* z)) `shouldBe` zipWith (P.*) listZ listZ
            eval valMap (z <.> z) `shouldBe` P.sum (zipWith (P.*) listZ listZ)
