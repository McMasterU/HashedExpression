module EvalShiftSpec where

import Control.Monad
import Data.Array.Unboxed as U
import qualified Data.ByteString.Char8 as C
import Data.Complex
import qualified Data.IntMap as I
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import Data.Maybe
import HashedConvZip
import HashedDerivative
import HashedDot
import HashedExpression
import HashedInstances
import HashedInterp
import HashedSimplify
import qualified Polynomials as P
import Test.QuickCheck hiding (scale)

--import System.IO.Unsafe
--import HashedExamples
import Debug.Trace
import Test.Hspec

normalShiftList :: Int -> [Double] -> [Double]
normalShiftList offset xs =
    if offset > 0
        then take (length xs) . (replicate offset 0 ++) $ xs
        else drop (-offset) . (++ replicate (-offset) 0) $ xs

oneD_0 :: [Double] -> Int -> Double -> Bool
oneD_0 lst offset c =
    U.elems evalRes == (normalShiftList offset . map (* c) $ lst)
  where
    size = length lst
    x1 = var1d size "x1"
    e = shiftScale offset c x1
    evalRes =
        evalOneD
            (simplify e)
            (subs ([], [("x1", U.listArray (0, size - 1) lst)], [], [], []))

spec :: Spec
spec =
    describe "eval shift test" $ do
        specify "test normalShiftList" $ do
            normalShiftList (-2) [1, 2, 3, 4, 5] `shouldBe` [3, 4, 5, 0, 0]
            normalShiftList 2 [1, 2, 3, 4, 5] `shouldBe` [0, 0, 1, 2, 3]
        specify "oneD 0" $ do
            let size = 5
                x1 = var1d size "x1"
                e = shift 1 x1
            evalOneD
                (simplify e)
                (subs
                     ( []
                     , [("x1", U.listArray (0, size - 1) [1, 2, 3, 4, 5])]
                     , []
                     , []
                     , [])) `shouldBe`
                U.listArray (0, size - 1) [0, 1, 2, 3, 4]
        specify "oneD 1" $ property oneD_0
