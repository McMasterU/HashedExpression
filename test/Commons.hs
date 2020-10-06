{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Commons where

import Control.Applicative (liftA2)
import Control.Monad (foldM)
import Data.Array hiding (range)
import Data.Complex (Complex (..), imagPart, realPart)
import Data.Function ((&))
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate, sort)
import qualified Data.Map.Strict as Map
import Data.Set (fromList, toList)
import HashedExpression.Internal
import HashedExpression.Internal.Base
import HashedExpression.Internal.OperationSpec
import HashedExpression.Interp
import HashedExpression.Modeling.Typed
import HashedExpression.Value
import Test.HUnit
import Test.Hspec
import Test.QuickCheck
import Text.Printf
import Var
import Prelude hiding ((^))

sizeReduceFactor :: Int
sizeReduceFactor = 3

shapeToString :: Shape -> String
shapeToString = intercalate "x" . map show

removeDuplicate :: (Ord a) => [a] -> [a]
removeDuplicate = toList . fromList

-------------------------------------------------------------------------------

genDouble :: Gen Double
genDouble = arbitrary `suchThat` inSmallRange
  where
    inSmallRange x = x >= 0 && x <= 10

vectorOfDifferent :: Eq a => Int -> Gen a -> Gen [a]
vectorOfDifferent sz gen = foldM f [] [1 .. sz]
  where
    f acc _ = (: acc) <$> gen `suchThat` (not . flip elem acc)

-------------------------------------------------------------------------------
genDimSelector :: Int -> Gen DimSelector
genDimSelector size = do
  let id = elements [0 .. size - 1]
  let step = elements [1 .. size]
  oneof [Range <$> id <*> id <*> step, At <$> id]

genAtSelector :: Int -> Gen DimSelector
genAtSelector size = do
  let id = elements [0 .. size - 1]
  oneof [At <$> id]

genValMapFor :: ExpressionMap -> Gen ValMap
genValMapFor mp = do
  let genVal :: Shape -> Gen Val
      genVal shape = do
        let dbls = vectorOf (product shape) genDouble
        case shape of
          [] -> VScalar <$> genDouble
          [sz] -> V1D . listArray (0, sz - 1) <$> dbls
          [sz1, sz2] -> V2D . listArray ((0, 0), (sz1 - 1, sz2 - 1)) <$> dbls
          [sz1, sz2, sz3] -> V3D . listArray ((0, 0, 0), (sz1 - 1, sz2 - 1, sz3 - 1)) <$> dbls
  vals <-
    (varNodes mp ++ paramNodes mp)
      & mapM
        ( \(name, shape, _) -> do
            val <- genVal shape
            return (name, val)
        )
  return $ Map.fromList vals

primitiveRUntyped :: Shape -> Gen Expr
primitiveRUntyped shape = do
  dbl <- genDouble
  name <- elements $ map pure ['a' .. 'z']
  let varName = name ++ shapeToString shape
  let parName = "p" ++ name ++ shapeToString shape
  elements
    [ fromNodeUnwrapped (shape, R, Var varName),
      fromNodeUnwrapped (shape, R, Param parName),
      fromNodeUnwrapped (shape, R, Const dbl)
    ]

primitiveCUntyped :: Shape -> Gen Expr
primitiveCUntyped shape = do
  re <- primitiveRUntyped shape
  im <- primitiveRUntyped shape
  return $ apply (Binary specRealImag) [re, im]

genExpUntyped :: Int -> Shape -> ElementType -> Gen Expr
genExpUntyped qc shape et
  | qc == 0 = case et of
    R -> primitiveRUntyped shape
    C -> primitiveCUntyped shape
  | otherwise =
    let sub = genExpUntyped (qc `div` sizeReduceFactor) shape et
        subScalarR = genExpUntyped (qc `div` sizeReduceFactor) [] R
        subScalarC = genExpUntyped (qc `div` sizeReduceFactor) [] C
        subR = genExpUntyped (qc `div` sizeReduceFactor) shape R
        subC = genExpUntyped (qc `div` sizeReduceFactor) shape C
        subOf shape et = genExpUntyped (qc `div` sizeReduceFactor) shape et
        fromPiecewise = do
          numBranches <- elements [2, 3]
          branches <- vectorOf numBranches sub
          condition <- subR
          marks <- sort <$> vectorOfDifferent (numBranches - 1) arbitrary
          let exp = apply (ConditionAry (specPiecewise marks)) (condition : branches)
          return exp
        binary spec = do
          x <- sub
          y <- sub
          return $ apply spec [x, y]
        unary spec = do
          x <- sub
          return $ apply spec [x]
        commonPossibilities =
          [ fromPiecewise,
            binary (Nary specSum),
            binary (Nary specMul),
            liftA2 (\x y -> apply (Binary specScale) [x, y]) subScalarR sub,
            unary (Unary specNeg),
            unary (Unary (specPower 2))
          ]
        specificElementTypePosibilities
          | et == R =
            [ (\x -> apply (Unary specRealPart) [x]) <$> subC,
              (\x -> apply (Unary specImagPart) [x]) <$> subC
            ]
          | et == C =
            [ liftA2 (\x y -> apply (Binary specRealImag) [x, y]) subR subR,
              unary (Unary specFT),
              unary (Unary specIFT),
              liftA2 (\x y -> apply (Binary specScale) [x, y]) subScalarC sub
            ]
        specificShapePossibilities = case shape of
          [] ->
            let fromProjection = do
                  size <- elements [3 .. 9]
                  exp <- subOf [size] et
                  ds <- genAtSelector size
                  return $ apply (Unary (specProject [ds])) [exp]
             in [ do
                    size <- elements [3 .. 9]
                    x <- subOf [size] et
                    y <- subOf [size] et
                    return $ apply (Binary specInnerProd) [x, y],
                  do
                    size1 <- elements [3 .. 9]
                    size2 <- elements [3 .. 9]
                    x <- subOf [size1, size2] et
                    y <- subOf [size1, size2] et
                    return $ apply (Binary specInnerProd) [x, y],
                  fromProjection
                ]
          [n] ->
            let fromRotate = do
                  amount <- elements [- n .. n]
                  exp <- sub
                  return $ apply (Unary (specRotate [amount])) [exp]
                fromProjectInject = do
                  exp1 <- sub
                  exp2 <- sub
                  ds <- genDimSelector n
                  return $
                    apply
                      (Binary (specInject [ds]))
                      [ apply (Unary (specProject [ds])) [exp1],
                        exp2
                      ]
                fromMatrixMul = do
                  m <- elements [3 .. 9]
                  x <- subOf [n, m] et
                  y <- subOf [m] et
                  return $ apply (Binary specMatMul) [x, y]
             in [ fromRotate,
                  fromProjectInject,
                  fromMatrixMul
                ]
          [m, n] ->
            let fromRotate = do
                  amount1 <- elements [- m .. m]
                  amount2 <- elements [- n .. n]
                  exp <- sub
                  return $ apply (Unary (specRotate [amount1, amount2])) [exp]
                fromProjectInject = do
                  exp1 <- sub
                  exp2 <- sub
                  ds1 <- genDimSelector m
                  ds2 <- genDimSelector n
                  return $
                    apply
                      (Binary (specInject [ds1, ds2]))
                      [ apply (Unary (specProject [ds1, ds2])) [exp1],
                        exp2
                      ]
                fromMatrixMul = do
                  p <- elements [3 .. 9]
                  x <- subOf [m, p] et
                  y <- subOf [p, n] et
                  return $ apply (Binary specMatMul) [x, y]
                fromTranspose = do
                  x <- subOf [n, m] et
                  return $ apply (Unary specTranspose) [x]
             in [ fromRotate,
                  fromProjectInject,
                  fromMatrixMul,
                  fromTranspose
                ]
     in oneof $ commonPossibilities ++ specificShapePossibilities ++ specificElementTypePosibilities

genExp :: forall d et. (Dimension d, IsElementType et) => Int -> Gen (Expression d et)
genExp size = wrapExpression <$> genExpUntyped size (toShape @d) (toElementType @et)

-------------------------------------------------------------------------------
instance (Dimension d, IsElementType et) => Arbitrary (Expression d et) where
  arbitrary = sized genExp

-------------------------------------------------------------------------------
data Suite d et
  = Suite (Expression d et) ValMap
  deriving (Show)

instance (Dimension d, IsElementType et) => Arbitrary (Suite d et) where
  arbitrary = do
    exp <- arbitrary
    valMap <- genValMapFor (exMap exp)
    return $ Suite exp valMap

-------------------------------------------------------------------------------
type SuiteScalarR = Suite Scalar R

type SuiteScalarC = Suite Scalar C

type SuiteOneR = Suite (D1 Default1D) R

type SuiteOneC = Suite (D1 Default1D) C

type SuiteTwoR = Suite (D2 Default2D1 Default2D2) R

type SuiteTwoC = Suite (D2 Default2D1 Default2D2) C

-------------------------------------------------------------------------------
newtype ArbitraryExpresion = ArbitraryExpresion {unArbitraryExpression :: Expr}
  deriving (Show, Ord, Eq)

instance Arbitrary ArbitraryExpresion where
  arbitrary =
    ArbitraryExpresion
      <$> oneof
        [ sized (\sz -> genExpUntyped sz [] R),
          sized (\sz -> genExpUntyped sz [] C),
          sized (\sz -> genExpUntyped sz [defaultDim1D] R),
          sized (\sz -> genExpUntyped sz [defaultDim1D] C),
          sized (\sz -> genExpUntyped sz [default1stDim2D, default2ndDim2D] R),
          sized (\sz -> genExpUntyped sz [default1stDim2D, default2ndDim2D] C)
        ]

data XSuite = XSuite Expr ValMap deriving (Show, Eq, Ord)

instance Arbitrary XSuite where
  arbitrary = do
    ArbitraryExpresion exp <- arbitrary
    valMap <- genValMapFor (fst exp)
    return $ XSuite exp valMap

-------------------------------------------------------------------------------

-- |
sz :: Expression d et -> Int
sz = IM.size . exMap

-------------------------------------------------------------------------------
instance (Ix i, Num a) => Num (Array i a) where
  (+) arr1 arr2 = listArray (bounds arr1) $ zipWith (+) (elems arr1) (elems arr2)
  (*) arr1 arr2 = listArray (bounds arr1) $ zipWith (*) (elems arr1) (elems arr2)

instance (Ix i) => ComplexRealOp (Array i Double) (Array i (Complex Double)) where
  (+:) arr1 arr2 = listArray (bounds arr1) $ zipWith (+:) (elems arr1) (elems arr2)
  xRe arr = listArray (bounds arr) $ map xRe (elems arr)
  xIm arr = listArray (bounds arr) $ map xIm (elems arr)
  conjugate arr = listArray (bounds arr) $ map conjugate (elems arr)

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

instance Approximable Double where
  (~=) :: Double -> Double -> Bool
  a ~= b
    | abs (a - b) < 1.0e-5 = True
    | a == b = True
    | otherwise = relativeError a b < 0.01
  prettifyShow a
    | abs a < 1e-10 = "0" --  in prettify, inputs less than a small condition value consider as zero,
    | otherwise = printf "%.2f" a --  otherwise, it shows the exact input.

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

instance Approximable (Array Int Double) where
  (~=) :: Array Int Double -> Array Int Double -> Bool
  a ~= b = (bounds a == bounds b) && and (zipWith (~=) (elems a) (elems b))
  prettifyShow a =
    "[" ++ (intercalate ", " . map prettifyShow . elems $ a) ++ "]" --  concatenate "," to seperate the elements of array in prettify

instance Approximable (Array Int (Complex Double)) where
  (~=) :: Array Int (Complex Double) -> Array Int (Complex Double) -> Bool
  a ~= b = (bounds a == bounds b) && and (zipWith (~=) (elems a) (elems b))
  prettifyShow a =
    "[" ++ (intercalate ", " . map prettifyShow . elems $ a) ++ "]" --  concatenate "," to seperate the elements of array in prettify

instance Approximable (Array (Int, Int) Double) where
  (~=) :: Array (Int, Int) Double -> Array (Int, Int) Double -> Bool
  a ~= b = (bounds a == bounds b) && and (zipWith (~=) (elems a) (elems b))
  prettifyShow a =
    "[" ++ (intercalate ", " . map prettifyShow . elems $ a) ++ "]" --  concatenate "," to seperate the elements of array in prettify

instance Approximable (Array (Int, Int) (Complex Double)) where
  (~=) ::
    Array (Int, Int) (Complex Double) ->
    Array (Int, Int) (Complex Double) ->
    Bool
  a ~= b = (bounds a == bounds b) && and (zipWith (~=) (elems a) (elems b))
  prettifyShow a =
    "[" ++ (intercalate ", " . map prettifyShow . elems $ a) ++ "]" --  concatenate "," to seperate the elements of array in prettify

instance Approximable (Array (Int, Int, Int) Double) where
  (~=) :: Array (Int, Int, Int) Double -> Array (Int, Int, Int) Double -> Bool
  a ~= b = (bounds a == bounds b) && and (zipWith (~=) (elems a) (elems b))
  prettifyShow a =
    "[" ++ (intercalate ", " . map prettifyShow . elems $ a) ++ "]" --  concatenate "," to seperate the elements of array in prettify

instance Approximable (Array (Int, Int, Int) (Complex Double)) where
  (~=) ::
    Array (Int, Int, Int) (Complex Double) ->
    Array (Int, Int, Int) (Complex Double) ->
    Bool
  a ~= b = (bounds a == bounds b) && and (zipWith (~=) (elems a) (elems b))
  prettifyShow a =
    "[" ++ (intercalate ", " . map prettifyShow . elems $ a) ++ "]" --  concatenate "," to seperate the elements of array in prettify

instance Approximable InterpValue where
  VR x ~= VR y = x ~= y
  V1DR x ~= V1DR y = x ~= y
  V2DR x ~= V2DR y = x ~= y
  V3DR x ~= V3DR y = x ~= y
  VC x ~= VC y = x ~= y
  V1DC x ~= V1DC y = x ~= y
  V2DC x ~= V2DC y = x ~= y
  V3DC x ~= V3DC y = x ~= y
  _ ~= _ = False

  prettifyShow a = case a of
    VR x -> prettifyShow x
    V1DR x -> prettifyShow x
    V2DR x -> prettifyShow x
    V3DR x -> prettifyShow x
    VC x -> prettifyShow x
    V1DC x -> prettifyShow x
    V2DC x -> prettifyShow x
    V3DC x -> prettifyShow x

-------------------------------------------------------------------------------
shouldApprox :: (HasCallStack, Approximable a) => a -> a -> Expectation
shouldApprox x y = assertBool msg (x ~= y)
  where
    msg = "Expected: " ++ prettifyShow y ++ "\nGot: " ++ prettifyShow x

infix 1 `shouldApprox`
