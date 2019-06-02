module EvalShiftSpec where

import Control.Monad
import Data.Array.Unboxed as U
import qualified Data.ByteString.Char8 as C
import qualified Data.Complex as DC
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
import HashedUtils (fromReal)
import Test.Hspec

class Matrix f where
    toList :: f a -> [a]

{-
  2-D Matrix
-}
data TwoDMatrix a =
    TwoDMatrix
        { dim1 :: Int
        , dim2 :: Int
        , unTwoDMatrix :: [[a]]
        }
    deriving (Eq, Show)

instance Functor TwoDMatrix where
    fmap f (TwoDMatrix d1 d2 mat) = TwoDMatrix d1 d2 (map (map f) mat)

instance Matrix TwoDMatrix where
    toList = concat . unTwoDMatrix

instance Arbitrary a => Arbitrary (TwoDMatrix a) where
    arbitrary = do
        dim1 <- choose (1, 30)
        dim2 <- choose (1, 30)
        mat <- vectorOf dim1 . vectorOf dim2 $ arbitrary
        return $ TwoDMatrix dim1 dim2 mat

{-
  3-D Matrix
-}
data ThreeDMatrix a =
    ThreeDMatrix
        { dim1_3d :: Int
        , dim2_3d :: Int
        , dim3_3d :: Int
        , unThreeDMatrix :: [TwoDMatrix a]
        }
    deriving (Eq, Show)

instance Functor ThreeDMatrix where
    fmap f (ThreeDMatrix d1 d2 d3 mat) =
        ThreeDMatrix d1 d2 d3 . map (fmap f) $ mat

instance Matrix ThreeDMatrix where
    toList = concat . map toList . unThreeDMatrix

instance Arbitrary a => Arbitrary (ThreeDMatrix a) where
    arbitrary = do
        dim1 <- choose (1, 10)
        dim2 <- choose (1, 10)
        dim3 <- choose (1, 10)
        mat <-
            vectorOf dim1 .
            fmap (TwoDMatrix dim2 dim3) . vectorOf dim2 . vectorOf dim3 $
            arbitrary
        return $ ThreeDMatrix dim1 dim2 dim3 mat

{-
  Done classes and instances
-}
normalShift1dWithZero :: Int -> a -> [a] -> [a]
normalShift1dWithZero offset zero xs =
    if offset > 0
        then take (length xs) . (replicate offset zero ++) $ xs
        else drop (-offset) . (++ replicate (-offset) zero) $ xs

normalShift1d :: Num a => Int -> [a] -> [a]
normalShift1d offset xs =
    if offset > 0
        then take (length xs) . (replicate offset zero ++) $ xs
        else drop (-offset) . (++ replicate (-offset) zero) $ xs

normalShift2d :: Num a => (Int, Int) -> TwoDMatrix a -> TwoDMatrix a
normalShift2d (offset1, offset2) twoDMat =
    TwoDMatrix d1 d2 .
    List.transpose .
    map (normalShift1d offset1) .
    List.transpose . map (normalShift1d offset2) . unTwoDMatrix $
    twoDMat
  where
    d1 = dim1 twoDMat
    d2 = dim2 twoDMat

normalShift3d :: Num a => (Int, Int, Int) -> ThreeDMatrix a -> ThreeDMatrix a
normalShift3d (offset1, offset2, offset3) threeDMat =
    ThreeDMatrix d1 d2 d3 .
    normalShift1dWithZero offset1 zeroMatrix .
    map (normalShift2d (offset2, offset3)) . unThreeDMatrix $
    threeDMat
  where
    d1 = dim1_3d threeDMat
    d2 = dim2_3d threeDMat
    d3 = dim3_3d threeDMat
    zeroMatrix = TwoDMatrix d2 d3 $ replicate d2 (replicate d3 0)

oneD_0 :: [Double] -> Int -> Double -> Bool
oneD_0 lst offset c =
    U.elems evalRes == (normalShift1d offset . map (* c) $ lst)
  where
    size = length lst
    x1 = var1d size "x1"
    e = shiftScale offset c x1
    evalRes =
        evalOneD
            (simplify e)
            (subs ([], [("x1", U.listArray (0, size - 1) lst)], [], [], []))

oneDC_0 :: [(Double, Double)] -> Int -> Double -> Bool
oneDC_0 lst offset c =
    U.elems evalRes == (normalShift1d offset . map (* fromReal c) $ complexLst)
  where
    size = length lst
    complexLst = map (uncurry (DC.:+)) lst
    x1 = var1d size "x1"
    x2 = var1d size "x2"
    v = x1 +: x2
    e = shiftScale offset c v
    evalRes =
        evalOneDC
            (simplify e)
            (subs
                 ( []
                 , [ ("x1", U.listArray (0, size - 1) (map fst lst))
                   , ("x2", U.listArray (0, size - 1) (map snd lst))
                   ]
                 , []
                 , []
                 , []))

twoD_0 :: TwoDMatrix Double -> (Int, Int) -> Double -> Bool
twoD_0 twoDMat@(TwoDMatrix d1 d2 _) offset c =
    U.elems evalRes == toList normalRes &&
    (last . U.indices $ evalRes) == (d1 - 1, d2 - 1)
  where
    normalRes = normalShift2d offset . fmap (* c) $ twoDMat
    x1 = var2d (d1, d2) "x1"
    e = shiftScale offset c x1
    evalRes =
        evalTwoD
            (simplify e)
            (subs
                 ( []
                 , []
                 , [ ( "x1"
                     , U.listArray ((0, 0), (d1 - 1, d2 - 1)) $ toList twoDMat)
                   ]
                 , []
                 , []))

twoDC_0 :: TwoDMatrix (Double, Double) -> (Int, Int) -> Double -> Bool
twoDC_0 twoDMat@(TwoDMatrix d1 d2 _) offset c =
    U.elems evalRes == toList normalRes &&
    (last . U.indices $ evalRes) == (d1 - 1, d2 - 1)
  where
    complexMat = fmap (uncurry (DC.:+)) twoDMat
    normalRes = normalShift2d offset . fmap (* fromReal c) $ complexMat
    x1 = var2d (d1, d2) "x1"
    x2 = var2d (d1, d2) "x2"
    v = x1 +: x2
    e = shiftScale offset c v
    evalRes =
        evalTwoDC
            (simplify e)
            (subs
                 ( []
                 , []
                 , [ ( "x1"
                     , U.listArray
                           ((0, 0), (d1 - 1, d2 - 1))
                           (map fst $ toList twoDMat))
                   , ( "x2"
                     , U.listArray
                           ((0, 0), (d1 - 1, d2 - 1))
                           (map snd $ toList twoDMat))
                   ]
                 , []
                 , []))

threeD_0 :: ThreeDMatrix Double -> (Int, Int, Int) -> Double -> Bool
threeD_0 threeDMat@(ThreeDMatrix d1 d2 d3 _) offset c =
    U.elems evalRes == toList normalRes &&
    (last . U.indices $ evalRes) == (d1 - 1, d2 - 1, d3 - 1)
  where
    normalRes = normalShift3d offset . fmap (* c) $ threeDMat
    x1 = var3d (d1, d2, d3) "x1"
    e = shiftScale offset c x1
    evalRes =
        evalThreeD
            (simplify e)
            (subs
                 ( []
                 , []
                 , []
                 , [ ( "x1"
                     , U.listArray ((0, 0, 0), (d1 - 1, d2 - 1, d3 - 1)) $
                       toList threeDMat)
                   ]
                 , []))

threeDC_0 :: ThreeDMatrix (Double, Double) -> (Int, Int, Int) -> Double -> Bool
threeDC_0 threeDMat@(ThreeDMatrix d1 d2 d3 _) offset c =
    U.elems evalRes == toList normalRes &&
    (last . U.indices $ evalRes) == (d1 - 1, d2 - 1, d3 - 1)
  where
    complexMat = fmap (uncurry (DC.:+)) threeDMat
    normalRes = normalShift3d offset . fmap (* fromReal c) $ complexMat
    x1 = var3d (d1, d2, d3) "x1"
    x2 = var3d (d1, d2, d3) "x2"
    v = x1 +: x2
    e = shiftScale offset c v
    evalRes =
        evalThreeDC
            (simplify e)
            (subs
                 ( []
                 , []
                 , []
                 , [ ( "x1"
                     , U.listArray ((0, 0, 0), (d1 - 1, d2 - 1, d3 - 1)) $
                       (map fst . toList $ threeDMat))
                   , ( "x2"
                     , U.listArray ((0, 0, 0), (d1 - 1, d2 - 1, d3 - 1)) $
                       (map snd . toList $ threeDMat))
                   ]
                 , []))

spec :: Spec
spec = do
    describe "eval shift tests 1d" $ do
        specify "test normalShiftList" $ do
            normalShift1d (-2) [1, 2, 3, 4, 5] `shouldBe` [3, 4, 5, 0, 0]
            normalShift1d 2 [1, 2, 3, 4, 5] `shouldBe` [0, 0, 1, 2, 3]
        specify "test normalShiftList 1d with empty list" $
            property $ \n -> normalShift1d n [] `shouldBe` []
        specify "normalShiftList with zero-shift" $
            property $ \lst -> (normalShift1d 0 lst) == (lst :: [Double])
        specify "normalShiftList with zero-shift complex" $
            property $ \lst ->
                normalShift1d 0 lst == (lst :: [DC.Complex Double])
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
        specify "oneDC 0" $ property oneDC_0
    describe "eval shift tests 2d" $ do
        specify "test normalShiftList 2d" $ do
            normalShift2d (0, -2) (TwoDMatrix 1 5 [[1, 2, 3, 4, 5]]) `shouldBe`
                (TwoDMatrix 1 5 [[3, 4, 5, 0, 0]])
            normalShift2d (0, 2) (TwoDMatrix 1 5 [[1, 2, 3, 4, 5]]) `shouldBe`
                (TwoDMatrix 1 5 [[0, 0, 1, 2, 3]])
            normalShift2d (1, 0) (TwoDMatrix 1 5 [[1, 2, 3, 4, 5]]) `shouldBe`
                (TwoDMatrix 1 5 [[0, 0, 0, 0, 0]])
        specify "normalShiftList2d with zero-shift" $
            property $ \twoDMat ->
                (normalShift2d (0, 0) twoDMat) == (twoDMat :: TwoDMatrix Double)
        specify "normalShiftList2d with zero-shift complex" $
            property $ \twoDMat ->
                (normalShift2d (0, 0) twoDMat) ==
                (twoDMat :: TwoDMatrix (DC.Complex Double))
        specify "twoD 0" $ property twoD_0
        specify "twoDC 0" $ property twoDC_0
    describe "eval shift tests 3d" $ do
        specify "threeD 0" $ property threeD_0
        specify "threeDC 0" $ property threeDC_0
