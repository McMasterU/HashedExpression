-- |
-- Module      :  HashedExpression.Interp
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- Evaluate expressions. Mainly useful for testings.
module HashedExpression.Interp1 where

import Data.Array
import Data.Complex
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Debug.Trace (traceId, traceShowId)
import GHC.TypeLits (KnownNat)
import HashedExpression.Internal.Expression
  ( D1,
    D2,
    D3,
    DimSelector (..),
    ElementType (..),
    Expression (..),
    ExpressionMap,
    NodeID,
    Op (..),
    Scalar,
  )
import HashedExpression.Internal.Node
import HashedExpression.Internal.Utils
import HashedExpression.Prettify (prettify, showExp)
import HashedExpression.Value

-- | Choose branch base on condition value.
-- In Decision tree, there are 2 possible outcomes, Head and Tail.
-- The decision of being Head or Tail is made based on the the condition value.
chooseBranch :: [Double] -> Double -> [a] -> a
chooseBranch marks val branches
  | val < head marks = head branches
  | otherwise =
    snd . last . filter ((val >=) . fst) $ zip marks (tail branches)

-- |  eval is our built-in interpreter which serves to verify semantic preservation of rewriting

zipWithA :: Ix x => (a -> b -> c) -> Array x a -> Array x b -> Array x c
zipWithA f xs ys = listArray (bounds xs) $ zipWith f (elems xs) (elems ys)

foldrElementwise :: Ix ix => (a -> a -> a) -> [Array ix a] -> Array ix a
foldrElementwise f [x] = x
foldrElementwise f (x : xs) = zipWithA f x (foldrElementwise f xs)

-- | Helper functions

-- NOTE: `mod` in Haskell with negative number, e.g, (-5) `mod` 3 = 1

-- | One dimension rotation.
--   The elemnts falling off of the length of the 1D array will appear at the beginning of the array
rotate1D ::
  -- | Size of the input array
  Int ->
  -- | amount of rotation
  Int ->
  -- | Input array
  Array Int a ->
  -- | Rotated array
  Array Int a
rotate1D size amount arr =
  listArray
    (0, size - 1)
    [arr ! ((i - amount) `mod` size) | i <- [0 .. size - 1]]

-- | Two dimension rotation.
--   The elements falling off of the length of the 2D array will appear at the beginning of the each row or column of the array
rotate2D ::
  -- | Size of the 2d input array
  (Int, Int) ->
  -- | amount of rotation for 2d array
  (Int, Int) ->
  -- | Input 2d array
  Array (Int, Int) a ->
  -- | Rotated 2d array
  Array (Int, Int) a
rotate2D (size1, size2) (amount1, amount2) arr =
  listArray
    ((0, 0), (size1 - 1, size2 - 1))
    [ arr ! ((i - amount1) `mod` size1, (j - amount2) `mod` size2)
      | i <- [0 .. size1 - 1],
        j <- [0 .. size2 - 1]
    ]

-- | Three dimension rotation
rotate3D ::
  -- | Size of 3d input array
  (Int, Int, Int) ->
  -- | Amount of Rotation for 3d array
  (Int, Int, Int) ->
  -- | Input 3d array
  Array (Int, Int, Int) a ->
  -- | Rotated 3d array
  Array (Int, Int, Int) a
rotate3D (size1, size2, size3) (amount1, amount2, amount3) arr =
  listArray
    ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
    [ arr
        ! ( (i - amount1) `mod` size1,
            (j - amount2) `mod` size2,
            (k - amount3) `mod` size3
          )
      | i <- [0 .. size1 - 1],
        j <- [0 .. size2 - 1],
        k <- [0 .. size3 - 1]
    ]

data FTMode = FT_FORWARD | FT_BACKWARD deriving (Eq, Ord)

-- | Fourier Transform in 1D.
--  Frequency is just in one dimension.
--  Consider a real-valued function, S(x),
--  that is integrable on an interval of P, which will be the period of the Fourier series.
--  number of cycles is n.
--  length of cycle is P/n, and frequency is n/P.
--  so for input i the frequency is (2*pi*i*n)/P
fourierTransform1D ::
  FTMode -> Int -> Array Int (Complex Double) -> Array Int (Complex Double)
fourierTransform1D mode size arr =
  listArray (0, size - 1) [computeX i | i <- [0 .. size - 1]]
  where
    s = if mode == FT_BACKWARD then fromIntegral size else 1
    computeX i = (sum $ zipWithA (*) arr (fourierBasis i)) / s
    fourierBasis i =
      let frequency n = (2 * pi * fromIntegral (i * n) / fromIntegral size) * (if mode == FT_BACKWARD then -1 else 1)
       in listArray
            (0, size - 1)
            [ cos (frequency n) :+ (- sin (frequency n))
              | n <- [0 .. size - 1]
            ]

-- | Fourier Transform in 2D
--  the frequency should be calculated in 2D
--  Consider a real-valued function, S(x),
--  that is integrable on an interval of P, which will be the period of the Fourier series.
--  numbber of cycles is n.
--  length of cycle is P/n, and frequency is n/P.
--  so for input i the frequency is (2*pi*i*n)/P
--  the frequency should be calculated in both dimensions for i and j
fourierTransform2D ::
  FTMode ->
  (Int, Int) ->
  Array (Int, Int) (Complex Double) ->
  Array (Int, Int) (Complex Double)
fourierTransform2D mode (size1, size2) arr =
  listArray
    ((0, 0), (size1 - 1, size2 - 1))
    [computeX i j | i <- [0 .. size1 - 1], j <- [0 .. size2 - 1]]
  where
    s = if mode == FT_BACKWARD then fromIntegral (size1 * size2) else 1
    computeX i j = (sum $ zipWithA (*) arr (fourierBasis i j)) / s
    fourierBasis i j =
      let frequency m n =
            ( 2 * pi * fromIntegral (i * m) / fromIntegral size1
                + 2 * pi * fromIntegral (j * n) / fromIntegral size2
            )
              * (if mode == FT_BACKWARD then -1 else 1)
       in listArray
            ((0, 0), (size1 - 1, size2 - 1))
            [ cos (frequency m n) :+ (- sin (frequency m n))
              | m <- [0 .. size1 - 1],
                n <- [0 .. size2 - 1]
            ]

-- | Fourier Transform in 3D
--   the frequency should be calculated in 3D
--   Consider a real-valued function, S(x),
--   that is integrable on an interval of P, which will be the period of the Fourier series.
--   numbber of cycles is n.
--   length of cycle is P/n, and frequency is n/P.
--   so for input i the frequency is (2*pi*i*n)/P
--   the frequency should be calculated for all dimensions, i , j , k
fourierTransform3D ::
  FTMode ->
  (Int, Int, Int) ->
  Array (Int, Int, Int) (Complex Double) ->
  Array (Int, Int, Int) (Complex Double)
fourierTransform3D mode (size1, size2, size3) arr =
  listArray
    ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
    [ computeX i j k
      | i <- [0 .. size1 - 1],
        j <- [0 .. size2 - 1],
        k <- [0 .. size3 - 1]
    ]
  where
    s = if mode == FT_BACKWARD then fromIntegral (size1 * size2) else 1
    computeX i j k = (sum $ zipWithA (*) arr (fourierBasis i j k)) / s
    fourierBasis i j k =
      let frequency m n p =
            ( 2 * pi * fromIntegral (i * m) / fromIntegral size1
                + 2 * pi * fromIntegral (j * n) / fromIntegral size2
                + 2 * pi * fromIntegral (k * p) / fromIntegral size3
            )
              * (if mode == FT_BACKWARD then -1 else 1)
       in listArray
            ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
            [ cos (frequency m n p) :+ (- sin (frequency m n p))
              | m <- [0 .. size1 - 1],
                n <- [0 .. size2 - 1],
                p <- [0 .. size3 - 1]
            ]
