-- |
-- Module      :  HashedExpression.Interp
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- This module is for approximation and evaluation of the expressions regarding to the precision
module HashedExpression.Interp
  ( Evaluable (..),
    Approximable (..),
  )
where

import Data.Array
import Data.Complex
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Debug.Trace (traceId, traceShowId)
import GHC.TypeLits (KnownNat)
import HashedExpression.Internal.Expression
  ( C,
    ET (..),
    Expression (..),
    ExpressionMap,
    NodeID,
    Op (..),
    R,
    Scalar,
  )
import HashedExpression.Internal.Node
import HashedExpression.Internal.Utils
import HashedExpression.Prettify (prettify, showExp)
import HashedExpression.Value
import Text.Printf

expZeroR :: ExpressionMap -> NodeID -> Expression Scalar R
expZeroR = flip Expression

expZeroC :: ExpressionMap -> NodeID -> Expression Scalar C
expZeroC = flip Expression

-- | Choose branch base on condition value.
-- In Decision tree, there are 2 possible outcomes, Head and Tail.
-- The decision of being Head or Tail is made based on the the condition value.
chooseBranch :: [Double] -> Double -> [a] -> a
chooseBranch marks val branches
  | val < head marks = head branches
  | otherwise =
    snd . last . filter ((val >=) . fst) $ zip marks (tail branches)

-- | Approximable class
class Show a => Approximable a where
  -- | (~=) checks if the two values are within acceptable numerical error
  (~=) :: a -> a -> Bool

  -- | Prettyprint a string produced by show
  prettifyShow :: a -> String

infix 4 ~=

{-
   Calculating the reletive error for two double-precision input arguments.
   It returns the error calculating by |a-b|/ max(|a|.|b|), where a and b are double-precision numbers, so it returns the relative
   error as double.
-}
relativeError :: Double -> Double -> Double
relativeError a b = abs (a - b) / max (abs a) (abs b)

{-
Instance which belongs to approximable class for Double precision inputs.
It takes 2 double precision inputs and return whether are within acceptable numerical error.
Returns true if the error is less than the condition value
-}
instance Approximable Double where
  (~=) :: Double -> Double -> Bool
  a ~= b
    | abs (a - b) < 1.0e-5 = True
    | a == b = True
    | otherwise = relativeError a b < 0.01
  prettifyShow a
    | abs a < 1e-10 = "0" --  in prettify, inputs less than a small condition value consider as zero,
    | otherwise = printf "%.2f" a --  otherwise, it shows the exact input.
    --  [TODO] Is it a specific condition value for prettify?

{-
Instance which belongs to approximable class for Complex Double precision inputs.
It takes 2 Complex double precision inputs and apply ~= operation on them.
calculating the error in real and imaginary parts of the complex input seperately.
Returns true if the error is less than the condition value.
-}
instance Approximable (Complex Double) where
  (~=) :: Complex Double -> Complex Double -> Bool -- Using approximable operation ~= for two Complex Double input and returns Bool
  a ~= b = (realPart a ~= realPart b) && (imagPart a ~= imagPart b) --  check for real and imaginary parts seperately
  prettifyShow a =
    prettifyShow (realPart a) ++ " + " ++ prettifyShow (imagPart a) ++ "i" --  Prettyprint a string produced by show

{-
Instance which belongs to approximable class for 1D array with Double-precision elements as an input.
Returns true if the dimentions are the same and the input arrays be elementwisely within acceptable numerical error.
-}
instance Approximable (Array Int Double) where
  (~=) :: Array Int Double -> Array Int Double -> Bool
  a ~= b = (bounds a == bounds b) && and (zipWith (~=) (elems a) (elems b))
  prettifyShow a =
    "[" ++ (intercalate ", " . map prettifyShow . elems $ a) ++ "]" --  concatenate "," to seperate the elements of array in prettify

{-
Instance which belongs to approximable class for 1D array with complex double-precision elements as an input.
Returns true if the dimentions are the same and "~=" be elementwisely true for input arrays.
-}
instance Approximable (Array Int (Complex Double)) where
  (~=) :: Array Int (Complex Double) -> Array Int (Complex Double) -> Bool
  a ~= b = (bounds a == bounds b) && and (zipWith (~=) (elems a) (elems b))
  prettifyShow a =
    "[" ++ (intercalate ", " . map prettifyShow . elems $ a) ++ "]" --  concatenate "," to seperate the elements of array in prettify

{-
Instance which belongs to approximable class for 2D array with double-precision elements as an input.
Returns true if the dimentions are the same and "~=" be elementwisely true for input arrays.
-}
instance Approximable (Array (Int, Int) Double) where
  (~=) :: Array (Int, Int) Double -> Array (Int, Int) Double -> Bool
  a ~= b = (bounds a == bounds b) && and (zipWith (~=) (elems a) (elems b))
  prettifyShow a =
    "[" ++ (intercalate ", " . map prettifyShow . elems $ a) ++ "]" --  concatenate "," to seperate the elements of array in prettify

{-
Instance which belongs to approximable class for 2D array with complex double-precision elements as an input.
Returns true if the dimentions are the same and "~=" be elementwisely true for input arrays.
-}
instance Approximable (Array (Int, Int) (Complex Double)) where
  (~=) ::
    Array (Int, Int) (Complex Double) ->
    Array (Int, Int) (Complex Double) ->
    Bool
  a ~= b = (bounds a == bounds b) && and (zipWith (~=) (elems a) (elems b))
  prettifyShow a =
    "[" ++ (intercalate ", " . map prettifyShow . elems $ a) ++ "]" --  concatenate "," to seperate the elements of array in prettify

{-
Instance which belongs to approximable class for 3D array with double-precision elements as an input.
Returns true if the dimentions are the same and "~=" be elementwisely true for input arrays.
-}
instance Approximable (Array (Int, Int, Int) Double) where
  (~=) :: Array (Int, Int, Int) Double -> Array (Int, Int, Int) Double -> Bool
  a ~= b = (bounds a == bounds b) && and (zipWith (~=) (elems a) (elems b))
  prettifyShow a =
    "[" ++ (intercalate ", " . map prettifyShow . elems $ a) ++ "]" --  concatenate "," to seperate the elements of array in prettify

{-
Instance which belongs to approximable class for 3D array with complex double-precision elements as an input.
Returns true if the dimentions are the same and "~=" be elementwisely true for input arrays.
-}
instance Approximable (Array (Int, Int, Int) (Complex Double)) where
  (~=) ::
    Array (Int, Int, Int) (Complex Double) ->
    Array (Int, Int, Int) (Complex Double) ->
    Bool
  a ~= b = (bounds a == bounds b) && and (zipWith (~=) (elems a) (elems b))
  prettifyShow a =
    "[" ++ (intercalate ", " . map prettifyShow . elems $ a) ++ "]" --  concatenate "," to seperate the elements of array in prettify

-- |  eval is our built-in interpreter which serves to verify semantic preservation of rewriting
class Evaluable d rc output | d rc -> output where
  eval :: ValMaps -> Expression d rc -> output

instance Evaluable Scalar R Double where
  eval :: ValMaps -> Expression Scalar R -> Double
  eval valMap e@(Expression n mp)
    | [] <- retrieveShape n mp =
      case retrieveOp n mp of
        Var name ->
          case Map.lookup name valMap of
            Just (VScalar val) -> val
            _ -> error "no value associated with the variable"
        Const val -> val
        Sum args -> sum . map (eval valMap . expZeroR mp) $ args --  sum of a scalar is of the type of R
        Mul args -> product . map (eval valMap . expZeroR mp) $ args --  mul of a scalar is of the type of R
        Neg arg -> - (eval valMap $ expZeroR mp arg) --  Unary minus
        Scale arg1 arg2 ->
          eval valMap (expZeroR mp arg1)
            * eval valMap (expZeroR mp arg2)
        Power x arg -> eval valMap (expZeroR mp arg) ^ x --  power operator with 2 inputs, power and base
        Div arg1 arg2 ->
          --  division operator with 2 inputs
          eval valMap (expZeroR mp arg1)
            / eval valMap (expZeroR mp arg2)
        Sqrt arg -> sqrt (eval valMap (expZeroR mp arg)) --  square root
        -- trigonometric functions
        Sin arg -> sin (eval valMap (expZeroR mp arg))
        Cos arg -> cos (eval valMap (expZeroR mp arg))
        Tan arg -> tan (eval valMap (expZeroR mp arg))
        Exp arg -> exp (eval valMap (expZeroR mp arg))
        Log arg -> log (eval valMap (expZeroR mp arg))
        Sinh arg -> sinh (eval valMap (expZeroR mp arg))
        Cosh arg -> cosh (eval valMap (expZeroR mp arg))
        Tanh arg -> tanh (eval valMap (expZeroR mp arg))
        Asin arg -> asin (eval valMap (expZeroR mp arg))
        Acos arg -> acos (eval valMap (expZeroR mp arg))
        Atan arg -> atan (eval valMap (expZeroR mp arg))
        Asinh arg -> asinh (eval valMap (expZeroR mp arg))
        Acosh arg -> acosh (eval valMap (expZeroR mp arg))
        Atanh arg -> atanh (eval valMap (expZeroR mp arg))
        RealPart arg -> realPart (eval valMap (expZeroC mp arg))
        ImagPart arg -> imagPart (eval valMap (expZeroC mp arg))
        --  Inner product is associating each pair of vectors with a scalar
        InnerProd arg1 arg2 ->
          case retrieveShape arg1 mp of
            [] ->
              --  inner product of Scalar
              eval valMap (expZeroR mp arg1)
                * eval valMap (expZeroR mp arg2)
            [size] ->
              --  inner product of vector that returns the sum over elementwise product of vector elements
              let res1 = evaluate1DReal valMap $ (mp, arg1)
                  res2 = evaluate1DReal valMap $ (mp, arg2)
               in sum
                    [ x * y
                      | i <- [0 .. size - 1],
                        let x = res1 ! i,
                        let y = res2 ! i
                    ]
            [size1, size2] ->
              --  inner product returns the sum over elementwise product of 2D matrix elements
              let res1 = evaluate2DReal valMap $ (mp, arg1)
                  res2 = evaluate2DReal valMap $ (mp, arg2)
               in sum
                    [ x * y
                      | i <- [0 .. size1 - 1],
                        j <- [0 .. size2 - 1],
                        let x = res1 ! (i, j),
                        let y = res2 ! (i, j)
                    ]
            [size1, size2, size3] ->
              --  inner product returns the sum over elementwise product of 3D matrix elements
              let res1 = evaluate3DReal valMap $ (mp, arg1)
                  res2 = evaluate3DReal valMap $ (mp, arg2)
               in sum
                    [ x * y
                      | i <- [0 .. size1 - 1],
                        j <- [0 .. size2 - 1],
                        k <- [0 .. size3 - 1],
                        let x = res1 ! (i, j, k),
                        let y = res2 ! (i, j, k)
                    ]
            _ -> error "4D shape?" --  returns error for more than 3D
        Piecewise marks conditionArg branchArgs ->
          let cdt = eval valMap $ expZeroR mp conditionArg
              branches = map (eval valMap . expZeroR mp) branchArgs
           in chooseBranch marks cdt branches
        _ ->
          error
            ("expression structure Scalar R is wrong " ++ prettify e)
    | otherwise = error "one r but shape is not [] ??"

instance Evaluable Scalar C (Complex Double) where
  eval :: ValMaps -> Expression Scalar C -> Complex Double
  eval valMap e@(Expression n mp)
    | [] <- retrieveShape n mp =
      case retrieveOp n mp of
        Sum args -> sum . map (eval valMap . expZeroC mp) $ args --  sum of a scalar is of the type C
        Mul args -> product . map (eval valMap . expZeroC mp) $ args --  Multiplication of a scalar is of the type C
        Power x arg -> eval valMap (expZeroC mp arg) ^ x --  power evaluation of arg to the power of x
        Neg arg -> - (eval valMap $ expZeroC mp arg)
        Scale arg1 arg2 ->
          case retrieveElementType arg1 mp of
            R ->
              fromR (eval valMap (expZeroR mp arg1))
                * eval valMap (expZeroC mp arg2)
            C ->
              eval valMap (expZeroC mp arg1)
                * eval valMap (expZeroC mp arg2)
        RealImag arg1 arg2 ->
          --  show the real and imaginary part of complex as x + i y
          eval valMap (expZeroR mp arg1)
            :+ eval valMap (expZeroR mp arg2)
        InnerProd arg1 arg2 ->
          --  evaluate the inner product in C
          case retrieveShape arg1 mp of
            [] ->
              --  evaluation for dimention of zero
              eval valMap (expZeroC mp arg1)
                * conjugate (eval valMap (expZeroC mp arg2))
            [size] ->
              --  inner product evaluation for a vector
              let res1 = evaluate1DComplex valMap $ (mp, arg1)
                  res2 = evaluate1DComplex valMap $ (mp, arg2)
               in sum
                    [ x * conjugate y
                      | i <- [0 .. size - 1],
                        let x = res1 ! i,
                        let y = res2 ! i
                    ]
            [size1, size2] ->
              --  inner product evaluation for 2D matrix
              let res1 = evaluate2DComplex valMap $ (mp, arg1)
                  res2 = evaluate2DComplex valMap $ (mp, arg2)
               in sum
                    [ x * conjugate y
                      | i <- [0 .. size1 - 1],
                        j <- [0 .. size2 - 1],
                        let x = res1 ! (i, j),
                        let y = res2 ! (i, j)
                    ]
            [size1, size2, size3] ->
              --  inner product evaluation for 3D matrix
              let res1 = evaluate3DComplex valMap $ (mp, arg1)
                  res2 = evaluate3DComplex valMap $ (mp, arg2)
               in sum
                    [ x * conjugate y
                      | i <- [0 .. size1 - 1],
                        j <- [0 .. size2 - 1],
                        k <- [0 .. size3 - 1],
                        let x = res1 ! (i, j, k),
                        let y = res2 ! (i, j, k)
                    ]
            _ -> error "4D shape?" --  returns errors for more than 3 dimension
        Piecewise marks conditionArg branchArgs ->
          let cdt = eval valMap $ expZeroR mp conditionArg
              branches = map (eval valMap . expZeroC mp) branchArgs
           in chooseBranch marks cdt branches
        _ ->
          error
            ("expression structure Scalar C is wrong " ++ prettify e)
    | otherwise = error "One C but shape is not [] ??"

zipWithA :: Ix x => (a -> b -> c) -> Array x a -> Array x b -> Array x c
zipWithA f xs ys = listArray (bounds xs) $ zipWith f (elems xs) (elems ys)

foldrElementwise :: Ix ix => (a -> a -> a) -> [Array ix a] -> Array ix a
foldrElementwise f [x] = x
foldrElementwise f (x : xs) = zipWithA f x (foldrElementwise f xs)

-- | the expression is undefined, here undefined 1D Real expressions are evaluated
evaluate1DReal :: ValMaps -> (ExpressionMap, NodeID) -> Array Int Double
evaluate1DReal valMap (mp, n)
  | [size] <- retrieveShape n mp =
    case retrieveOp n mp of
      Var name ->
        case Map.lookup name valMap of
          Just (V1D val) -> val
          _ -> error "no value associated with the variable"
      Const val -> listArray (0, size - 1) $ replicate size val
      Sum args ->
        --  evaluate the sum over undefined input arguments
        foldrElementwise (+) . map (evaluate1DReal valMap . (mp,)) $
          args
      Mul args ->
        --  evaluate the Mul over undefined input arguments
        foldrElementwise (*) . map (evaluate1DReal valMap . (mp,)) $
          args
      Power x arg -> fmap (^ x) (evaluate1DReal valMap $ (mp, arg)) --  evaluate the power over undefined input arguments
      Neg arg -> fmap negate . evaluate1DReal valMap $ (mp, arg)
      Scale arg1 arg2 ->
        let scalar = eval valMap $ expZeroR mp arg1
         in fmap (scalar *) . evaluate1DReal valMap $ (mp, arg2)
      Div arg1 arg2 ->
        --  evaluate the Division over undefined input arguments
        zipWithA
          (/)
          (evaluate1DReal valMap $ (mp, arg2))
          (evaluate1DReal valMap $ (mp, arg2))
      --  trigonometric functions
      Sqrt arg -> fmap sqrt . evaluate1DReal valMap $ (mp, arg)
      Sin arg -> fmap sin . evaluate1DReal valMap $ (mp, arg)
      Cos arg -> fmap cos . evaluate1DReal valMap $ (mp, arg)
      Tan arg -> fmap tan . evaluate1DReal valMap $ (mp, arg)
      Exp arg -> fmap exp . evaluate1DReal valMap $ (mp, arg)
      Log arg -> fmap log . evaluate1DReal valMap $ (mp, arg)
      Sinh arg -> fmap sinh . evaluate1DReal valMap $ (mp, arg)
      Cosh arg -> fmap cosh . evaluate1DReal valMap $ (mp, arg)
      Tanh arg -> fmap tanh . evaluate1DReal valMap $ (mp, arg)
      Asin arg -> fmap asin . evaluate1DReal valMap $ (mp, arg)
      Acos arg -> fmap acos . evaluate1DReal valMap $ (mp, arg)
      Atan arg -> fmap atan . evaluate1DReal valMap $ (mp, arg)
      Asinh arg -> fmap asinh . evaluate1DReal valMap $ (mp, arg)
      Acosh arg -> fmap acosh . evaluate1DReal valMap $ (mp, arg)
      Atanh arg -> fmap atanh . evaluate1DReal valMap $ (mp, arg)
      RealPart arg -> fmap realPart . evaluate1DComplex valMap $ (mp, arg)
      ImagPart arg -> fmap imagPart . evaluate1DComplex valMap $ (mp, arg)
      -- Rotate rA arg ->
      Piecewise marks conditionArg branchArgs ->
        let cdt = evaluate1DReal valMap $ (mp, conditionArg)
            branches = map (evaluate1DReal valMap . (mp,)) branchArgs
         in listArray
              (0, size - 1)
              [ chosen ! i
                | i <- [0 .. size - 1],
                  let chosen = chooseBranch marks (cdt ! i) branches
              ]
      Rotate [amount] arg ->
        rotate1D size amount (evaluate1DReal valMap $ (mp, arg))
      TwiceReFT arg ->
        let innerRes = evaluate1DReal valMap $ (mp, arg)
            scaleFactor = fromIntegral size / 2
         in listArray
              (0, size - 1)
              [ scaleFactor
                  * (innerRes ! i + innerRes ! ((size - i) `mod` size))
                | i <- [0 .. size - 1]
              ]
      TwiceImFT arg ->
        let innerRes = evaluate1DReal valMap $ (mp, arg)
            scaleFactor = fromIntegral size / 2
         in listArray
              (0, size - 1)
              [ scaleFactor
                  * (innerRes ! i - innerRes ! ((size - i) `mod` size))
                | i <- [0 .. size - 1]
              ]
      ReFT arg ->
        case retrieveElementType arg mp of
          R ->
            let inner =
                  fmap (:+ 0) . evaluate1DReal valMap $ (mp, arg)
                ftResult = fourierTransform1D size inner
             in fmap realPart ftResult
          C ->
            let inner = evaluate1DComplex valMap $ (mp, arg)
                ftResult = fourierTransform1D size inner
             in fmap realPart ftResult
      ImFT arg ->
        case retrieveElementType arg mp of
          R ->
            let inner =
                  fmap (:+ 0) . evaluate1DReal valMap $ (mp, arg)
                ftResult = fourierTransform1D size inner
             in fmap imagPart ftResult
          C ->
            let inner = evaluate1DComplex valMap $ (mp, arg)
                ftResult = fourierTransform1D size inner
             in fmap imagPart ftResult
      _ -> error "expression structure One R is wrong"
  | otherwise = error "one r but shape is not [size] ??"

instance (KnownNat n) => Evaluable n R (Array Int Double) where
  eval :: ValMaps -> Expression n R -> Array Int Double
  eval valMap (Expression n mp) = evaluate1DReal valMap (mp, n)

-- | evaluate undefined 1D complex expression
evaluate1DComplex ::
  ValMaps -> (ExpressionMap, NodeID) -> Array Int (Complex Double)
evaluate1DComplex valMap (mp, n)
  | [size] <- retrieveShape n mp =
    case retrieveOp n mp of
      Sum args ->
        --  evaluate the sum over undefined input arguments
        foldrElementwise (+) . map (evaluate1DComplex valMap . (mp,)) $
          args
      Mul args ->
        --  evaluate the Mul over undefined input arguments
        foldrElementwise (*) . map (evaluate1DComplex valMap . (mp,)) $
          args
      Power x arg -> fmap (^ x) (evaluate1DComplex valMap $ (mp, arg)) --  evaluate the power over undefined input arguments
      Neg arg -> fmap negate . evaluate1DComplex valMap $ (mp, arg)
      Scale arg1 arg2 ->
        case retrieveElementType arg1 mp of
          R ->
            let scalar = fromR . eval valMap $ expZeroR mp arg1
             in fmap (scalar *) . evaluate1DComplex valMap $
                  (mp, arg2)
          C ->
            let scalar = eval valMap $ expZeroC mp arg1
             in fmap (scalar *) . evaluate1DComplex valMap $
                  (mp, arg2)
      RealImag arg1 arg2 ->
        zipWithA
          (:+)
          (evaluate1DReal valMap $ (mp, arg1))
          (evaluate1DReal valMap $ (mp, arg2))
      Piecewise marks conditionArg branchArgs ->
        let cdt = evaluate1DReal valMap $ (mp, conditionArg)
            branches =
              map (evaluate1DComplex valMap . (mp,)) branchArgs
         in listArray
              (0, size - 1)
              [ chosen ! i
                | i <- [0 .. size - 1],
                  let chosen = chooseBranch marks (cdt ! i) branches
              ]
      Rotate [amount] arg ->
        rotate1D size amount (evaluate1DComplex valMap $ (mp, arg))
      _ -> error "expression structure One C is wrong"
  | otherwise = error "one C but shape is not [size] ??"

instance (KnownNat n) => Evaluable n C (Array Int (Complex Double)) where
  eval :: ValMaps -> Expression n C -> Array Int (Complex Double)
  eval valMap (Expression n mp) = evaluate1DComplex valMap (mp, n)

-- | Evaluate 2D undefined input expressions
evaluate2DReal :: ValMaps -> (ExpressionMap, NodeID) -> Array (Int, Int) Double
evaluate2DReal valMap (mp, n)
  | [size1, size2] <- retrieveShape n mp =
    case retrieveOp n mp of
      Var name ->
        case Map.lookup name valMap of
          Just (V2D val) -> val
          _ -> error $ "no value associated with the variable" ++ name
      Const val ->
        listArray ((0, 0), (size1 - 1, size2 - 1)) $
          replicate (size1 * size2) val
      Sum args ->
        --  evaluate the sum over undefined input arguments
        foldrElementwise (+) . map (evaluate2DReal valMap . (mp,)) $
          args
      Mul args ->
        --  evaluate the Mul over undefined input arguments
        foldrElementwise (*) . map (evaluate2DReal valMap . (mp,)) $
          args
      Power x arg -> fmap (^ x) (evaluate2DReal valMap $ (mp, arg)) --  evaluate the power over undefined input arguments
      Neg arg -> fmap negate . evaluate2DReal valMap $ (mp, arg)
      Scale arg1 arg2 ->
        let scalar = eval valMap $ expZeroR mp arg1
         in fmap (scalar *) . evaluate2DReal valMap $ (mp, arg2)
      Div arg1 arg2 ->
        zipWithA
          (/)
          (evaluate2DReal valMap $ (mp, arg2))
          (evaluate2DReal valMap $ (mp, arg2))
      Sqrt arg -> fmap sqrt . evaluate2DReal valMap $ (mp, arg)
      Sin arg -> fmap sin . evaluate2DReal valMap $ (mp, arg)
      Cos arg -> fmap cos . evaluate2DReal valMap $ (mp, arg)
      Tan arg -> fmap tan . evaluate2DReal valMap $ (mp, arg)
      Exp arg -> fmap exp . evaluate2DReal valMap $ (mp, arg)
      Log arg -> fmap log . evaluate2DReal valMap $ (mp, arg)
      Sinh arg -> fmap sinh . evaluate2DReal valMap $ (mp, arg)
      Cosh arg -> fmap cosh . evaluate2DReal valMap $ (mp, arg)
      Tanh arg -> fmap tanh . evaluate2DReal valMap $ (mp, arg)
      Asin arg -> fmap asin . evaluate2DReal valMap $ (mp, arg)
      Acos arg -> fmap acos . evaluate2DReal valMap $ (mp, arg)
      Atan arg -> fmap atan . evaluate2DReal valMap $ (mp, arg)
      Asinh arg -> fmap asinh . evaluate2DReal valMap $ (mp, arg)
      Acosh arg -> fmap acosh . evaluate2DReal valMap $ (mp, arg)
      Atanh arg -> fmap atanh . evaluate2DReal valMap $ (mp, arg)
      RealPart arg -> fmap realPart . evaluate2DComplex valMap $ (mp, arg)
      ImagPart arg -> fmap imagPart . evaluate2DComplex valMap $ (mp, arg)
      Piecewise marks conditionArg branchArgs ->
        let cdt = evaluate2DReal valMap $ (mp, conditionArg)
            branches = map (evaluate2DReal valMap . (mp,)) branchArgs
         in listArray
              ((0, 0), (size1 - 1, size2 - 1))
              [ chosen ! (i, j)
                | i <- [0 .. size1 - 1],
                  j <- [0 .. size2 - 1],
                  let chosen =
                        chooseBranch marks (cdt ! (i, j)) branches
              ]
      Rotate [amount1, amount2] arg ->
        rotate2D
          (size1, size2)
          (amount1, amount2)
          (evaluate2DReal valMap $ (mp, arg))
      TwiceReFT arg ->
        let innerRes = evaluate2DReal valMap $ (mp, arg)
            scaleFactor = fromIntegral size1 * fromIntegral size2 / 2
         in listArray
              ((0, 0), (size1 - 1, size2 - 1))
              [ scaleFactor
                  * ( innerRes ! (i, j)
                        + innerRes
                        ! ((size1 - i) `mod` size1, (size2 - j) `mod` size2)
                    )
                | i <- [0 .. size1 - 1],
                  j <- [0 .. size2 - 1]
              ]
      TwiceImFT arg ->
        let innerRes = evaluate2DReal valMap $ (mp, arg)
            scaleFactor = fromIntegral size1 * fromIntegral size2 / 2
         in listArray
              ((0, 0), (size1 - 1, size2 - 1))
              [ scaleFactor
                  * ( innerRes ! (i, j)
                        - innerRes
                        ! ((size1 - i) `mod` size1, (size2 - j) `mod` size2)
                    )
                | i <- [0 .. size1 - 1],
                  j <- [0 .. size2 - 1]
              ]
      ReFT arg ->
        case retrieveElementType arg mp of
          R ->
            let inner =
                  fmap (:+ 0) . evaluate2DReal valMap $ (mp, arg)
                ftResult = fourierTransform2D (size1, size2) inner
             in fmap realPart ftResult
          C ->
            let inner = evaluate2DComplex valMap $ (mp, arg)
                ftResult = fourierTransform2D (size1, size2) inner
             in fmap realPart ftResult
      ImFT arg ->
        case retrieveElementType arg mp of
          R ->
            let inner =
                  fmap (:+ 0) . evaluate2DReal valMap $ (mp, arg)
                ftResult = fourierTransform2D (size1, size2) inner
             in fmap imagPart ftResult
          C ->
            let inner = evaluate2DComplex valMap $ (mp, arg)
                ftResult = fourierTransform2D (size1, size2) inner
             in fmap imagPart ftResult
      _ -> error "expression structure Two R is wrong"
  | otherwise = error "Two r but shape is not [size1, size2] ??"

instance
  (KnownNat m, KnownNat n) =>
  Evaluable '(m, n) R (Array (Int, Int) Double)
  where
  eval :: ValMaps -> Expression '(m, n) R -> Array (Int, Int) Double
  eval valMap (Expression n mp) = evaluate2DReal valMap (mp, n)

-- | Evaluate 2D undefined complex expression
evaluate2DComplex ::
  ValMaps -> (ExpressionMap, NodeID) -> Array (Int, Int) (Complex Double)
evaluate2DComplex valMap (mp, n)
  | [size1, size2] <- retrieveShape n mp =
    case retrieveOp n mp of
      Sum args ->
        --  evaluate the sum over undefined input arguments
        foldrElementwise (+) . map (evaluate2DComplex valMap . (mp,)) $
          args
      Mul args ->
        --  evaluate the Mul over undefined input arguments
        foldrElementwise (*) . map (evaluate2DComplex valMap . (mp,)) $
          args
      Power x arg -> fmap (^ x) (evaluate2DComplex valMap $ (mp, arg)) --  evaluate the power over undefined input arguments
      Neg arg -> fmap negate . evaluate2DComplex valMap $ (mp, arg)
      Scale arg1 arg2 ->
        case retrieveElementType arg1 mp of
          R ->
            let scalar = fromR . eval valMap $ expZeroR mp arg1
             in fmap (scalar *) . evaluate2DComplex valMap $
                  (mp, arg2)
          C ->
            let scalar = eval valMap $ expZeroC mp arg1
             in fmap (scalar *) . evaluate2DComplex valMap $
                  (mp, arg2)
      RealImag arg1 arg2 ->
        zipWithA
          (:+)
          (evaluate2DReal valMap $ (mp, arg1))
          (evaluate2DReal valMap $ (mp, arg2))
      Piecewise marks conditionArg branchArgs ->
        let cdt = evaluate2DReal valMap $ (mp, conditionArg)
            branches =
              map (evaluate2DComplex valMap . (mp,)) branchArgs
         in listArray
              ((0, 0), (size1 - 1, size2 - 1))
              [ chosen ! (i, j)
                | i <- [0 .. size1 - 1],
                  j <- [0 .. size2 - 1],
                  let chosen =
                        chooseBranch marks (cdt ! (i, j)) branches
              ]
      Rotate [amount1, amount2] arg ->
        rotate2D
          (size1, size2)
          (amount1, amount2)
          (evaluate2DComplex valMap $ (mp, arg))
      _ -> error "expression structure Two C is wrong"
  | otherwise = error "Two C but shape is not [size1, size2] ??"

instance
  (KnownNat m, KnownNat n) =>
  Evaluable '(m, n) C (Array (Int, Int) (Complex Double))
  where
  eval ::
    ValMaps -> Expression '(m, n) C -> Array (Int, Int) (Complex Double)
  eval valMap (Expression n mp) = evaluate2DComplex valMap (mp, n)

-- | Evaluate 3D undefined complex expression
evaluate3DReal ::
  ValMaps -> (ExpressionMap, NodeID) -> Array (Int, Int, Int) Double
evaluate3DReal valMap (mp, n)
  | [size1, size2, size3] <- retrieveShape n mp =
    case retrieveOp n mp of
      Var name ->
        case Map.lookup name valMap of
          Just (V3D val) -> val
          _ -> error "no value associated with the variable"
      Const val ->
        listArray ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1)) $
          replicate (size1 * size2 * size3) val
      Sum args ->
        foldrElementwise (+) . map (evaluate3DReal valMap . (mp,)) $
          args
      Mul args ->
        foldrElementwise (*) . map (evaluate3DReal valMap . (mp,)) $
          args
      Power x arg -> fmap (^ x) (evaluate3DReal valMap $ (mp, arg)) --  evaluate the power over undefined input arguments
      Neg arg -> fmap negate . evaluate3DReal valMap $ (mp, arg)
      Scale arg1 arg2 ->
        let scalar = eval valMap $ expZeroR mp arg1
         in fmap (scalar *) . evaluate3DReal valMap $ (mp, arg2)
      Div arg1 arg2 ->
        zipWithA
          (/)
          (evaluate3DReal valMap $ (mp, arg2))
          (evaluate3DReal valMap $ (mp, arg2))
      Sqrt arg -> fmap sqrt . evaluate3DReal valMap $ (mp, arg)
      Sin arg -> fmap sin . evaluate3DReal valMap $ (mp, arg)
      Cos arg -> fmap cos . evaluate3DReal valMap $ (mp, arg)
      Tan arg -> fmap tan . evaluate3DReal valMap $ (mp, arg)
      Exp arg -> fmap exp . evaluate3DReal valMap $ (mp, arg)
      Log arg -> fmap log . evaluate3DReal valMap $ (mp, arg)
      Sinh arg -> fmap sinh . evaluate3DReal valMap $ (mp, arg)
      Cosh arg -> fmap cosh . evaluate3DReal valMap $ (mp, arg)
      Tanh arg -> fmap tanh . evaluate3DReal valMap $ (mp, arg)
      Asin arg -> fmap asin . evaluate3DReal valMap $ (mp, arg)
      Acos arg -> fmap acos . evaluate3DReal valMap $ (mp, arg)
      Atan arg -> fmap atan . evaluate3DReal valMap $ (mp, arg)
      Asinh arg -> fmap asinh . evaluate3DReal valMap $ (mp, arg)
      Acosh arg -> fmap acosh . evaluate3DReal valMap $ (mp, arg)
      Atanh arg -> fmap atanh . evaluate3DReal valMap $ (mp, arg)
      RealPart arg -> fmap realPart . evaluate3DComplex valMap $ (mp, arg)
      ImagPart arg -> fmap imagPart . evaluate3DComplex valMap $ (mp, arg)
      Piecewise marks conditionArg branchArgs ->
        let cdt = evaluate3DReal valMap $ (mp, conditionArg)
            branches = map (evaluate3DReal valMap . (mp,)) branchArgs
         in listArray
              ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
              [ chosen ! (i, j, k)
                | i <- [0 .. size1 - 1],
                  j <- [0 .. size2 - 1],
                  k <- [0 .. size3 - 1],
                  let chosen =
                        chooseBranch marks (cdt ! (i, j, k)) branches
              ]
      Rotate [amount1, amount2, amount3] arg ->
        rotate3D
          (size1, size2, size3)
          (amount1, amount2, amount3)
          (evaluate3DReal valMap $ (mp, arg))
      TwiceReFT arg ->
        let innerRes = evaluate3DReal valMap $ (mp, arg)
            scaleFactor =
              fromIntegral size1 * fromIntegral size2
                * fromIntegral size3
                / 2
         in listArray
              ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
              [ scaleFactor
                  * ( innerRes ! (i, j, k)
                        + innerRes
                        ! ( (size1 - i) `mod` size1,
                            (size2 - j) `mod` size2,
                            (size3 - k) `mod` size3
                          )
                    )
                | i <- [0 .. size1 - 1],
                  j <- [0 .. size2 - 1],
                  k <- [0 .. size3 - 1]
              ]
      TwiceImFT arg ->
        let innerRes = evaluate3DReal valMap $ (mp, arg)
            scaleFactor =
              fromIntegral size1 * fromIntegral size2
                * fromIntegral size3
                / 2
         in listArray
              ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
              [ scaleFactor
                  * ( innerRes ! (i, j, k)
                        - innerRes
                        ! ( (size1 - i) `mod` size1,
                            (size2 - j) `mod` size2,
                            (size3 - k) `mod` size3
                          )
                    )
                | i <- [0 .. size1 - 1],
                  j <- [0 .. size2 - 1],
                  k <- [0 .. size3 - 1]
              ]
      ReFT arg ->
        case retrieveElementType arg mp of
          R ->
            let inner =
                  fmap (:+ 0) . evaluate3DReal valMap $ (mp, arg)
                ftResult =
                  fourierTransform3D (size1, size2, size3) inner
             in fmap realPart ftResult
          C ->
            let inner = evaluate3DComplex valMap $ (mp, arg)
                ftResult =
                  fourierTransform3D (size1, size2, size3) inner
             in fmap realPart ftResult
      ImFT arg ->
        case retrieveElementType arg mp of
          R ->
            let inner =
                  fmap (:+ 0) . evaluate3DReal valMap $ (mp, arg)
                ftResult =
                  fourierTransform3D (size1, size2, size3) inner
             in fmap imagPart ftResult
          C ->
            let inner = evaluate3DComplex valMap $ (mp, arg)
                ftResult =
                  fourierTransform3D (size1, size2, size3) inner
             in fmap imagPart ftResult
      _ -> error "expression structure Three R is wrong"
  | otherwise = error "Three r but shape is not [size1, size2, size3] ??"

instance
  (KnownNat m, KnownNat n, KnownNat p) =>
  Evaluable '(m, n, p) R (Array (Int, Int, Int) Double)
  where
  eval :: ValMaps -> Expression '(m, n, p) R -> Array (Int, Int, Int) Double
  eval valMap (Expression n mp) = evaluate3DReal valMap (mp, n)

-- | Evaluate the 3D undefined expression
evaluate3DComplex ::
  ValMaps -> (ExpressionMap, NodeID) -> Array (Int, Int, Int) (Complex Double)
evaluate3DComplex valMap (mp, n)
  | [size1, size2, size3] <- retrieveShape n mp =
    case retrieveOp n mp of
      Sum args ->
        --  evaluate the sum over undefined input arguments
        foldrElementwise (+) . map (evaluate3DComplex valMap . (mp,)) $
          args
      Mul args ->
        --  evaluate the Mul over undefined input arguments
        foldrElementwise (*) . map (evaluate3DComplex valMap . (mp,)) $
          args
      Power x arg -> fmap (^ x) (evaluate3DComplex valMap $ (mp, arg)) --  evaluate the power over undefined input arguments
      Neg arg -> fmap negate . evaluate3DComplex valMap $ (mp, arg)
      Scale arg1 arg2 ->
        case retrieveElementType arg1 mp of
          R ->
            let scalar = fromR . eval valMap $ expZeroR mp arg1
             in fmap (scalar *) . evaluate3DComplex valMap $
                  (mp, arg2)
          C ->
            let scalar = eval valMap $ expZeroC mp arg1
             in fmap (scalar *) . evaluate3DComplex valMap $
                  (mp, arg2)
      RealImag arg1 arg2 ->
        zipWithA
          (:+)
          (evaluate3DReal valMap $ (mp, arg1))
          (evaluate3DReal valMap $ (mp, arg2))
      Piecewise marks conditionArg branchArgs ->
        let cdt = evaluate3DReal valMap $ (mp, conditionArg)
            branches =
              map (evaluate3DComplex valMap . (mp,)) branchArgs
         in listArray
              ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
              [ chosen ! (i, j, k)
                | i <- [0 .. size1 - 1],
                  j <- [0 .. size2 - 1],
                  k <- [0 .. size3 - 1],
                  let chosen =
                        chooseBranch marks (cdt ! (i, j, k)) branches
              ]
      Rotate [amount1, amount2, amount3] arg ->
        rotate3D
          (size1, size2, size3)
          (amount1, amount2, amount3)
          (evaluate3DComplex valMap $ (mp, arg))
      _ -> error "expression structure Three C is wrong"
  | otherwise = error "Three C but shape is not [size1, size2, size3] ??"

instance
  (KnownNat m, KnownNat n, KnownNat p) =>
  Evaluable '(m, n, p) C (Array (Int, Int, Int) (Complex Double))
  where
  eval ::
    ValMaps ->
    Expression '(m, n, p) C ->
    Array (Int, Int, Int) (Complex Double)
  eval valMap (Expression n mp) = evaluate3DComplex valMap (mp, n)

-- | Helper functions

-- NOTE: `mod` in Haskell works with negative number, e.g, (-5) `mod` 3 = 1

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

-- | Fourier Transform in 1D.
--  Frequency is just in one dimension.
--  Consider a real-valued function, S(x),
--  that is integrable on an interval of P, which will be the period of the Fourier series.
--  number of cycles is n.
--  length of cycle is P/n, and frequency is n/P.
--  so for input i the frequency is (2*pi*i*n)/P
fourierTransform1D ::
  Int -> Array Int (Complex Double) -> Array Int (Complex Double)
fourierTransform1D size arr =
  listArray (0, size - 1) [computeX i | i <- [0 .. size - 1]]
  where
    computeX i = sum $ zipWithA (*) arr (fourierBasis i)
    fourierBasis i =
      let frequency n = 2 * pi * fromIntegral (i * n) / fromIntegral size
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
  (Int, Int) ->
  Array (Int, Int) (Complex Double) ->
  Array (Int, Int) (Complex Double)
fourierTransform2D (size1, size2) arr =
  listArray
    ((0, 0), (size1 - 1, size2 - 1))
    [computeX i j | i <- [0 .. size1 - 1], j <- [0 .. size2 - 1]]
  where
    computeX i j = sum $ zipWithA (*) arr (fourierBasis i j)
    fourierBasis i j =
      let frequency m n =
            2 * pi * fromIntegral (i * m) / fromIntegral size1
              + 2 * pi * fromIntegral (j * n) / fromIntegral size2
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
  (Int, Int, Int) ->
  Array (Int, Int, Int) (Complex Double) ->
  Array (Int, Int, Int) (Complex Double)
fourierTransform3D (size1, size2, size3) arr =
  listArray
    ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
    [ computeX i j k
      | i <- [0 .. size1 - 1],
        j <- [0 .. size2 - 1],
        k <- [0 .. size3 - 1]
    ]
  where
    computeX i j k = sum $ zipWithA (*) arr (fourierBasis i j k)
    fourierBasis i j k =
      let frequency m n p =
            2 * pi * fromIntegral (i * m) / fromIntegral size1
              + 2 * pi * fromIntegral (j * n) / fromIntegral size2
              + 2 * pi * fromIntegral (k * p) / fromIntegral size3
       in listArray
            ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
            [ cos (frequency m n p) :+ (- sin (frequency m n p))
              | m <- [0 .. size1 - 1],
                n <- [0 .. size2 - 1],
                p <- [0 .. size3 - 1]
            ]
