-- |
-- Module      :  HashedExpression.Interp
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- Evaluate expressions. Mainly useful for testings.
module HashedExpression.Interp where

import Data.Array
import Data.Complex hiding (conjugate)
import Data.Function ((&))
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import qualified Data.List as List
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Debug.Trace (traceId, traceShowId)
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.TypeLits (KnownNat)
import HashedExpression.Internal.Base hiding ((**))
import HashedExpression.Internal.Expression hiding ((**))
import HashedExpression.Internal.Node
import HashedExpression.Utils
import HashedExpression.Prettify (prettify, showExp)
import HashedExpression.Value

data InterpValue
  = VR Double
  | VC (Complex Double)
  | V1DR (Array Int Double)
  | V1DC (Array Int (Complex Double))
  | V2DR (Array (Int, Int) Double)
  | V2DC (Array (Int, Int) (Complex Double))
  | V3DR (Array (Int, Int, Int) Double)
  | V3DC (Array (Int, Int, Int) (Complex Double))
  deriving (Show, Eq)

eval :: ValMap -> Expression d et -> InterpValue
eval valMap (Expression nID mp) =
  let (shape, et, op) = retrieveNode nID mp
      eval' :: NodeID -> InterpValue
      eval' x = eval valMap (Expression x mp)
      -------------------------------------------------------------------------------
      -- Partial helper functions
      constructR :: [Double] -> InterpValue
      constructR vs = case (shape, vs) of
        ([], [v]) -> VR v
        ([size], _) -> V1DR $ listArray (0, size - 1) vs
        ([size1, size2], _) -> V2DR $ listArray ((0, 0), (size1 - 1, size2 - 1)) vs
        ([size1, size2, size3], _) -> V3DR $ listArray ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1)) vs
      extractR :: InterpValue -> [Double]
      extractR val = case val of
        VR v -> [v]
        V1DR arr -> elems arr
        V2DR arr -> elems arr
        V3DR arr -> elems arr
      constructC :: [Complex Double] -> InterpValue
      constructC vs = case (shape, vs) of
        ([], [v]) -> VC v
        ([size], _) -> V1DC $ listArray (0, size - 1) vs
        ([size1, size2], _) -> V2DC $ listArray ((0, 0), (size1 - 1, size2 - 1)) vs
        ([size1, size2, size3], _) -> V3DC $ listArray ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1)) vs
      extractC :: InterpValue -> [Complex Double]
      extractC val = case val of
        VC v -> [v]
        V1DC arr -> elems arr
        V2DC arr -> elems arr
        V3DC arr -> elems arr
      unaryR op arg = arg |> eval' |> extractR |> map op |> constructR
      unaryC op arg = arg |> eval' |> extractC |> map op |> constructC
      binaryR op arg1 arg2 = zipWith op (extractR $ eval' arg1) (extractR $ eval' arg2) |> constructR
      binaryC op arg1 arg2 = zipWith op (extractC $ eval' arg1) (extractC $ eval' arg2) |> constructC
   in case op of
        Var name -> case (shape, Map.lookup name valMap) of
          ([], Just (VScalar val)) -> VR val
          ([_], Just (V1D arr)) -> V1DR arr
          ([_, _], Just (V2D arr)) -> V2DR arr
          ([_, _, _], Just (V3D arr)) -> V3DR arr
        Param name -> case (shape, Map.lookup name valMap) of
          ([], Just (VScalar val)) -> VR val
          ([_], Just (V1D arr)) -> V1DR arr
          ([_, _], Just (V2D arr)) -> V2DR arr
          ([_, _, _], Just (V3D arr)) -> V3DR arr
        Const v -> constructR $ replicate (product shape) v
        Sum args
          | et == R -> args |> map eval' |> map extractR |> foldl1 (zipWith (+)) |> constructR
          | et == C -> args |> map eval' |> map extractC |> foldl1 (zipWith (+)) |> constructC
        Mul args
          | et == R -> args |> map eval' |> map extractR |> foldl1 (zipWith (*)) |> constructR
          | et == C -> args |> map eval' |> map extractC |> foldl1 (zipWith (*)) |> constructC
        Power x arg
          | et == R -> unaryR (** fromIntegral x) arg
          | et == C -> unaryC (** fromIntegral x) arg
        Neg arg
          | et == R -> unaryR negate arg
          | et == C -> unaryC negate arg
        Scale arg1 arg2 -> case (retrieveElementType arg1 mp, retrieveElementType arg2 mp) of
          (R, R) ->
            let VR v = eval' arg1
             in unaryR (v *) arg2
          (R, C) ->
            let VR v = eval' arg1
             in unaryC ((v :+ 0) *) arg2
          (C, C) ->
            let VC v = eval' arg1
             in unaryC (v *) arg2
        Div arg1 arg2
          | et == R -> binaryR (/) arg1 arg2
          | et == C -> binaryC (/) arg1 arg2
        -------------------------------------------------------------------------------
        Sqrt arg -> unaryR sqrt arg
        Sin arg -> unaryR sin arg
        Cos arg -> unaryR cos arg
        Tan arg -> unaryR tan arg
        Exp arg -> unaryR exp arg
        Log arg -> unaryR log arg
        Sinh arg -> unaryR sinh arg
        Cosh arg -> unaryR cosh arg
        Tanh arg -> unaryR tanh arg
        Asin arg -> unaryR asin arg
        Acos arg -> unaryR acos arg
        Atan arg -> unaryR atan arg
        Asinh arg -> unaryR asinh arg
        Acosh arg -> unaryR acosh arg
        Atanh arg -> unaryR atanh arg
        -------------------------------------------------------------------------------
        RealImag arg1 arg2 ->
          let re = extractR (eval' arg1)
              im = extractR (eval' arg2)
           in constructC $ zipWith (:+) re im
        RealPart arg -> extractC (eval' arg) |> map realPart |> constructR
        ImagPart arg -> extractC (eval' arg) |> map imagPart |> constructR
        Conjugate arg -> unaryC conjugate arg
        -------------------------------------------------------------------------------
        InnerProd arg1 arg2
          | et == R ->
            let x = extractR (eval' arg1)
                y = extractR (eval' arg2)
             in VR $ sum $ zipWith (*) x y
          | et == C ->
            let x = extractC (eval' arg1)
                y = extractC (eval' arg2)
             in VC $ sum $ zipWith (*) x (map conjugate y)
        Piecewise marks conditionArg branchArgs
          | et == R ->
            let condition = extractR (eval' conditionArg)
                branches = map (extractR . eval') branchArgs
             in constructR $ zipWith (chooseBranch marks) condition (List.transpose branches)
          | et == C ->
            let condition = extractR (eval' conditionArg)
                branches = map (extractC . eval') branchArgs
             in constructC $ zipWith (chooseBranch marks) condition (List.transpose branches)
        Rotate rotateAmount arg
          | et == R -> case (rotateAmount, shape) of
            ([amount], [size]) ->
              let V1DR arr = eval' arg
               in V1DR $ rotate1D size amount arr
            ([amount1, amount2], [size1, size2]) ->
              let V2DR arr = eval' arg
               in V2DR $ rotate2D (size1, size2) (amount1, amount2) arr
            ([amount1, amount2, amount3], [size1, size2, size3]) ->
              let V3DR arr = eval' arg
               in V3DR $ rotate3D (size1, size2, size3) (amount1, amount2, amount3) arr
          | et == C -> case (rotateAmount, shape) of
            ([amount], [size]) ->
              let V1DC arr = eval' arg
               in V1DC $ rotate1D size amount arr
            ([amount1, amount2], [size1, size2]) ->
              let V2DC arr = eval' arg
               in V2DC $ rotate2D (size1, size2) (amount1, amount2) arr
            ([amount1, amount2, amount3], [size1, size2, size3]) ->
              let V3DC arr = eval' arg
               in V3DC $ rotate3D (size1, size2, size3) (amount1, amount2, amount3) arr
        FT arg -> case shape of
          [] -> eval' arg
          [size] ->
            let V1DC arr = eval' arg
             in V1DC $ fourierTransform1D FT_FORWARD size arr
          [size1, size2] ->
            let V2DC arr = eval' arg
             in V2DC $ fourierTransform2D FT_FORWARD (size1, size2) arr
          [size1, size2, size3] ->
            let V3DC arr = eval' arg
             in V3DC $ fourierTransform3D FT_FORWARD (size1, size2, size3) arr
        IFT arg -> case shape of
          [] -> eval' arg
          [size] ->
            let V1DC arr = eval' arg
             in V1DC $ fourierTransform1D FT_BACKWARD size arr
          [size1, size2] ->
            let V2DC arr = eval' arg
             in V2DC $ fourierTransform2D FT_BACKWARD (size1, size2) arr
          [size1, size2, size3] ->
            let V3DC arr = eval' arg
             in V3DC $ fourierTransform3D FT_BACKWARD (size1, size2, size3) arr
        Project dss arg -> case shape of
          [] -> case (retrieveShape arg mp, dss) of
            ([size], [At i]) -> case eval' arg of
              V1DR base -> VR $ base ! i
              V1DC base -> VC $ base ! i
            ([size1, size2], [At i, At j]) -> case eval' arg of
              V2DR base -> VR $ base ! (i, j)
              V2DC base -> VC $ base ! (i, j)
            ([size1, size2, size3], [At i, At j, At k]) -> case eval' arg of
              V3DR base -> VR $ base ! (i, j, k)
              V3DC base -> VC $ base ! (i, j, k)
          [size] -> case (retrieveShape arg mp, dss) of
            ([bSize], [ds]) -> case eval' arg of
              V1DR base -> V1DR $ listArray (0, size - 1) [base ! i | i <- mkIndices ds bSize]
              V1DC base -> V1DC $ listArray (0, size - 1) [base ! i | i <- mkIndices ds bSize]
            ([bSize1, bSize2], [ds1, ds2]) -> case eval' arg of
              V2DR base ->
                V1DR $
                  listArray (0, size - 1) $
                    [base ! (i, j) | i <- mkIndices ds1 bSize1, j <- mkIndices ds2 bSize2]
              V2DC base ->
                V1DC $
                  listArray (0, size - 1) $
                    [base ! (i, j) | i <- mkIndices ds1 bSize1, j <- mkIndices ds2 bSize2]
            ([bSize1, bSize2, bSize3], [ds1, ds2, ds3]) -> case eval' arg of
              V3DR base ->
                V1DR $
                  listArray (0, size - 1) $
                    [base ! (i, j, k) | i <- mkIndices ds1 bSize1, j <- mkIndices ds2 bSize2, k <- mkIndices ds3 bSize3]
              V3DC base ->
                V1DC $
                  listArray (0, size - 1) $
                    [base ! (i, j, k) | i <- mkIndices ds1 bSize1, j <- mkIndices ds2 bSize2, k <- mkIndices ds3 bSize3]
          [size1, size2] -> case (retrieveShape arg mp, dss) of
            ([bSize1, bSize2], [ds1, ds2]) -> case eval' arg of
              V2DR base ->
                V2DR $
                  listArray
                    ((0, 0), (size1 - 1, size2 - 1))
                    [base ! (i, j) | i <- mkIndices ds1 bSize1, j <- mkIndices ds2 bSize2]
              V2DC base ->
                V2DC $
                  listArray
                    ((0, 0), (size1 - 1, size2 - 1))
                    [base ! (i, j) | i <- mkIndices ds1 bSize1, j <- mkIndices ds2 bSize2]
            ([bSize1, bSize2, bSize3], [ds1, ds2, ds3]) -> case eval' arg of
              V3DR base ->
                V2DR $
                  listArray
                    ((0, 0), (size1 - 1, size2 - 1))
                    [base ! (i, j, k) | i <- mkIndices ds1 bSize1, j <- mkIndices ds2 bSize2, k <- mkIndices ds3 bSize3]
              V3DC base ->
                V2DC $
                  listArray
                    ((0, 0), (size1 - 1, size2 - 1))
                    [base ! (i, j, k) | i <- mkIndices ds1 bSize1, j <- mkIndices ds2 bSize2, k <- mkIndices ds3 bSize3]
          [size1, size2, size3] -> case (retrieveShape arg mp, dss) of
            ([bSize1, bSize2, bSize3], [ds1, ds2, ds3]) -> case eval' arg of
              V3DR base ->
                V3DR $
                  listArray
                    ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
                    [base ! (i, j, k) | i <- mkIndices ds1 bSize1, j <- mkIndices ds2 bSize2, k <- mkIndices ds3 bSize3]
              V3DC base ->
                V3DC $
                  listArray
                    ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
                    [base ! (i, j, k) | i <- mkIndices ds1 bSize1, j <- mkIndices ds2 bSize2, k <- mkIndices ds3 bSize3]
        Inject dss subArg baseArg
          | et == R ->
            let injectingElements = extractR $ eval' subArg
             in case (eval' baseArg, dss, shape) of
                  (V1DR base, [ds], [size]) ->
                    let indices = mkIndices ds size
                     in V1DR $ base // zip indices injectingElements
                  (V2DR base, [ds1, ds2], [size1, size2]) ->
                    let indices = [(i, j) | i <- mkIndices ds1 size1, j <- mkIndices ds2 size2]
                     in V2DR $ base // zip indices injectingElements
                  (V3DR base, [ds1, ds2, ds3], [size1, size2, size3]) ->
                    let indices = [(i, j, k) | i <- mkIndices ds1 size1, j <- mkIndices ds2 size2, k <- mkIndices ds3 size3]
                     in V3DR $ base // zip indices injectingElements
          | et == C ->
            let injectingElements = extractC $ eval' subArg
             in case (eval' baseArg, dss, shape) of
                  (V1DC base, [ds], [size]) ->
                    let indices = mkIndices ds size
                     in V1DC $ base // zip indices injectingElements
                  (V2DC base, [ds1, ds2], [size1, size2]) ->
                    let indices = [(i, j) | i <- mkIndices ds1 size1, j <- mkIndices ds2 size2]
                     in V2DC $ base // zip indices injectingElements
                  (V3DC base, [ds1, ds2, ds3], [size1, size2, size3]) ->
                    let indices = [(i, j, k) | i <- mkIndices ds1 size1, j <- mkIndices ds2 size2, k <- mkIndices ds3 size3]
                     in V3DC $ base // zip indices injectingElements
        MatMul arg1 arg2
          | et == R ->
            case (shape, retrieveShape arg1 mp, retrieveShape arg2 mp) of
              ([_m], [m, _n], [n]) ->
                let V2DR x = eval' arg1
                    V1DR y = eval' arg2
                 in V1DR $
                      listArray
                        (0, m - 1)
                        [ sum [(x ! (i, j)) * (y ! j) | j <- [0 .. n - 1]]
                          | i <- [0 .. m - 1]
                        ]
              ([_m, _p], [m, _n], [n, p]) ->
                let V2DR x = eval' arg1
                    V2DR y = eval' arg2
                 in V2DR $
                      listArray
                        ((0, 0), (m - 1, p - 1))
                        [ sum [(x ! (i, k)) * (y ! (k, j)) | k <- [0 .. n - 1]]
                          | i <- [0 .. m - 1],
                            j <- [0 .. p - 1]
                        ]
          | et == C ->
            case (shape, retrieveShape arg1 mp, retrieveShape arg2 mp) of
              ([_m], [m, _n], [n]) ->
                let V2DC x = eval' arg1
                    V1DC y = eval' arg2
                 in V1DC $
                      listArray
                        (0, m - 1)
                        [ sum [(x ! (i, j)) * (y ! j) | j <- [0 .. n - 1]]
                          | i <- [0 .. m - 1]
                        ]
              ([_m, _p], [m, _n], [n, p]) ->
                let V2DC x = eval' arg1
                    V2DC y = eval' arg2
                 in V2DC $
                      listArray
                        ((0, 0), (m - 1, p - 1))
                        [ sum [(x ! (i, k)) * (y ! (k, j)) | k <- [0 .. n - 1]]
                          | i <- [0 .. m - 1],
                            j <- [0 .. p - 1]
                        ]
        Transpose arg
          | et == R ->
            case (retrieveShape arg mp, shape) of
              -- ([m], [1, _m]) ->
              --   let V1DR x = eval' arg
              --    in V2DR $ listArray ((0, 0), (1, m - 1)) $ elems x
              ([m, n], [_n, _m]) ->
                let V2DR x = eval' arg
                 in V2DR $ listArray ((0, 0), (n - 1, m - 1)) [x ! (j, i) | i <- [0 .. n - 1], j <- [0 .. m - 1]]
          | et == C ->
            case (retrieveShape arg mp, shape) of
              -- ([m], [1, _m]) ->
              --   let V1DC x = eval' arg
              --    in V2DC $ listArray ((0, 0), (1, m - 1)) $ elems x
              ([m, n], [_n, _m]) ->
                let V2DC x = eval' arg
                 in V2DC $ listArray ((0, 0), (n - 1, m - 1)) [x ! (j, i) | i <- [0 .. n - 1], j <- [0 .. m - 1]]
        Coerce _ arg
          | et == R -> constructR . extractR $ eval' arg
          | et == C -> constructC . extractC $ eval' arg
        node -> error $ show node

zipWithA :: Ix x => (a -> b -> c) -> Array x a -> Array x b -> Array x c
zipWithA f xs ys = listArray (bounds xs) $ zipWith f (elems xs) (elems ys)

-- | Choose branch base on condition value.
-- In Decision tree, there are 2 possible outcomes, Head and Tail.
-- The decision of being Head or Tail is made based on the the condition value.
chooseBranch :: [Double] -> Double -> [a] -> a
chooseBranch marks val branches
  | val < head marks = head branches
  | otherwise =
    snd . last . filter ((val >=) . fst) $ zip marks (tail branches)

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
