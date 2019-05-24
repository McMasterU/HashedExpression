module SimplifySpec where

import HashedDerivative
import HashedExpression
import HashedInstances
import HashedInterp
import Test.Hspec
import HashedSimplify
import HashedConvZip
import HashedDot
import qualified Polynomials as P
import Control.Monad
import Data.Array.Unboxed as U
import qualified Data.ByteString.Char8 as C
import Data.Complex
import qualified Data.IntMap as I
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import Data.Maybe
import Test.QuickCheck hiding (scale)

--import System.IO.Unsafe
--import HashedExamples
import Debug.Trace

[x, y, z, u, v, w] = map var ["x", "y", "z", "u", "v", "w"]

spec :: Spec
spec = describe "simplify expression tests" $ do
    specify "simplify 0" $ do
        simplify (x * 1) `shouldBe` x
        simplify (1 * x) `shouldBe` x
        {-
        $x*0$ and $0*x$ follow the same pattern as $x*1$ and $1*x$.
        -}
        simplify (x * 0) `shouldBe` 0
        simplify (0 * x) `shouldBe` 0
        {-

        These tests show how the instances of (*) and product work.
        -}
        product [x, y, z] `shouldNotBe` (x * y * z)
        product [x, y, z] `shouldBe` (1 * x * y * z)
        pretty (unScalar (product [x, y, z])) `shouldBe` "(((1.0*x)*y)*z)"
        pretty (unScalar (x * y * z)) `shouldBe` "((x*y)*z)"
        {-

        Now looking at some simplifications.
        -}
        pretty (unScalar (simplify ((x * y) * z))) `shouldBe` "(x*y*z)"
        pretty (unScalar (simplify (product [x, y, z]))) `shouldBe` "(x*y*z)"
        {-

        Use applyOne here.
        -}
        simplify ((x * 0) * y) `shouldBe` 0
        simplify (y * (x * 0)) `shouldBe` 0
        simplify (0 * (x * 1)) `shouldBe` 0
        simplify (0 * x * 1) `shouldBe` 0
        simplify (0 * (x * y)) `shouldBe` 0
        simplify ((x * y) * 0) `shouldBe` 0
        simplify ((x * 0) * 1) `shouldBe` 0
        simplify ((x * y) * 1) `shouldBe` (x * y)
        simplify (x * y * z * 1) `shouldBe` simplify (x * y * z)

    specify "simply 0" $ do
        pendingWith "Test not implemented"




