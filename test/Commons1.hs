{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Commons where

import Control.Applicative (liftA2)
import Control.Monad (foldM, forM)
import Data.Array hiding (range)
import Data.Complex (Complex (..), imagPart, realPart)
import Data.Function ((&))
import Data.Function.HT (nest)
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Set (Set, fromList, toList)
import Data.Time (diffUTCTime, getCurrentTime)
import Debug.Trace (traceShowId)
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.TypeLits (KnownNat, Nat, type (+), type (-))
import HashedExpression.Internal
import HashedExpression.Internal.Expression
import HashedExpression.Internal.OperationSpec
import HashedExpression.Internal.Utils
import HashedExpression.Interp
import HashedExpression.Operation
import qualified HashedExpression.Operation
import HashedExpression.Prettify
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

genDouble :: Gen Double
genDouble = arbitrary `suchThat` inSmallRange
  where
    inSmallRange x = x >= 0 && x <= 10

vectorOfDifferent :: Eq a => Int -> Gen a -> Gen [a]
vectorOfDifferent sz gen = foldM f [] [1 .. sz]
  where
    f acc _ = (: acc) <$> gen `suchThat` (not . flip elem acc)

-------------------------------------------------------------------------------
unsafeProject :: [DimSelector] -> Expression d1 et1 -> Expression d2 et2
unsafeProject ds e = wrap $ apply (Unary (specProject ds)) [unwrap e]

unsafeInject :: [DimSelector] -> Expression d1 et1 -> Expression d2 et2 -> Expression d2 et2
unsafeInject ds sub base = wrap $ apply (Binary (specInject ds)) [unwrap sub, unwrap base]

unsafeRotate :: RotateAmount -> Expression d1 et1 -> Expression d2 et2
unsafeRotate amount e = wrap $ apply (Unary (specRotate amount)) [unwrap e]

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

primitiveR :: forall d. Dimension d => Gen (Expression d R)
primitiveR = do
  let shape = extractShape @d
  dbl <- genDouble
  name <- elements $ map pure ['a' .. 'z']
  let varName = name ++ shapeToString shape
  let parName = "p" ++ name ++ shapeToString shape
  elements
    [ gvariable @d varName,
      gparam @d parName,
      gconstant @d dbl
    ]

primitiveC :: forall d. Dimension d => Gen (Expression d C)
primitiveC = liftA2 (+:) primitiveR primitiveR

genValMapFor :: Expression d et -> Gen ValMaps
genValMapFor (Expression nID mp) = do
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

genExpC :: forall d. (Dimension d) => Int -> Gen (Expression d C)
genExpC size
  | size == 0 = primitiveC @d
  | otherwise =
    let sub = genExpC @d (size `div` sizeReduceFactor)
        subScalarR = genExpR @Scalar (size `div` sizeReduceFactor)
        subScalarC = genExpC @Scalar (size `div` sizeReduceFactor)
        subR = genExpR @d (size `div` sizeReduceFactor)
        fromPiecewise = do
          numBranches <- elements [2, 3]
          branches <- vectorOf numBranches sub
          condition <- subR
          marks <- sort <$> vectorOfDifferent (numBranches - 1) arbitrary
          let exp = piecewise marks condition branches
          return exp
        binary op = liftA2 op sub sub
        unary op = op <$> sub
        commonPossibilities =
          [ fromPiecewise,
            binary (+),
            binary (*),
            liftA2 (*.) subScalarR sub,
            unary negate,
            unary (^ 2),
            liftA2 (+:) subR subR
          ]
        unsafeConvert :: Gen (Expression mustBe_d C) -> Gen (Expression d C)
        unsafeConvert = fmap (wrap . unwrap)
        specificShapePossibilities = case extractShape @d of
          [] ->
            let sub1D = genExpC @(D1 Default1D) (size `div` sizeReduceFactor)
                sub2D = genExpC @(D2 Default2D1 Default2D2) (size `div` sizeReduceFactor)
                fromProjection = do
                  exp <- sub1D
                  ds <- genAtSelector (nat @Default1D)
                  return $ unsafeProject [ds] exp
             in map unsafeConvert $
                  [ liftA2 (<.>) sub1D sub1D,
                    liftA2 (<.>) sub2D sub2D,
                    fromProjection
                  ]
          [size] ->
            let fromRotate = do
                  amount <- elements [- size .. size]
                  unsafeRotate [amount] <$> sub
                fromProjectInject = do
                  exp1 <- sub
                  exp2 <- sub
                  ds <- genDimSelector size
                  return $ unsafeInject [ds] (unsafeProject [ds] exp1) exp2
             in map unsafeConvert $
                  [ fromRotate,
                    fromProjectInject
                  ]
          [size1, size2] ->
            let fromRotate = do
                  amount1 <- elements [- size1 .. size1]
                  amount2 <- elements [- size2 .. size2]
                  unsafeRotate [amount1, amount2] <$> sub
                fromProjectInject = do
                  exp1 <- sub
                  exp2 <- sub
                  ds1 <- genDimSelector size1
                  ds2 <- genDimSelector size2
                  return $ unsafeInject [ds1, ds2] (unsafeProject [ds1, ds2] exp1) exp2
             in map unsafeConvert $
                  [ fromRotate,
                    fromProjectInject
                  ]
     in oneof $ commonPossibilities ++ specificShapePossibilities

genExpR :: forall d. (Dimension d) => Int -> Gen (Expression d R)
genExpR size
  | size == 0 = primitiveR @d
  | otherwise =
    let sub = genExpR @d (size `div` sizeReduceFactor)
        subScalarR = genExpR @Scalar (size `div` sizeReduceFactor)
        subC = genExpC @d (size `div` sizeReduceFactor)
        fromPiecewise = do
          numBranches <- elements [2, 3]
          branches <- vectorOf numBranches sub
          condition <- sub
          marks <- sort <$> vectorOfDifferent (numBranches - 1) arbitrary
          let exp = piecewise marks condition branches
          return exp
        binary op = liftA2 op sub sub
        unary op = op <$> sub
        commonPossibilities =
          [ fromPiecewise,
            binary (+),
            binary (*),
            liftA2 (*.) subScalarR sub,
            unary negate,
            unary (^ 2),
            xRe <$> subC,
            xIm <$> subC
          ]
        unsafeConvert :: Gen (Expression mustBe_d R) -> Gen (Expression d R)
        unsafeConvert = fmap (wrap . unwrap)
        specificShapePossibilities = case extractShape @d of
          [] ->
            let sub1D = genExpR @(D1 Default1D) (size `div` sizeReduceFactor)
                sub2D = genExpR @(D2 Default2D1 Default2D2) (size `div` sizeReduceFactor)
                fromProjection = do
                  exp <- sub1D
                  ds <- genAtSelector (nat @Default1D)
                  return $ unsafeProject [ds] exp
             in map unsafeConvert $
                  [ liftA2 (<.>) sub1D sub1D,
                    liftA2 (<.>) sub2D sub2D,
                    fromProjection
                  ]
          [size] ->
            let fromRotate = do
                  amount <- elements [- size .. size]
                  unsafeRotate [amount] <$> sub
                fromProjectInject = do
                  exp1 <- sub
                  exp2 <- sub
                  ds <- genDimSelector size
                  return $ unsafeInject [ds] (unsafeProject [ds] exp1) exp2
             in map unsafeConvert $
                  [ fromRotate,
                    fromProjectInject
                  ]
          [size1, size2] ->
            let fromRotate = do
                  amount1 <- elements [- size1 .. size1]
                  amount2 <- elements [- size2 .. size2]
                  unsafeRotate [amount1, amount2] <$> sub
                fromProjectInject = do
                  exp1 <- sub
                  exp2 <- sub
                  ds1 <- genDimSelector size1
                  ds2 <- genDimSelector size2
                  return $ unsafeInject [ds1, ds2] (unsafeProject [ds1, ds2] exp1) exp2
             in map unsafeConvert $
                  [ fromRotate,
                    fromProjectInject
                  ]
     in oneof $ commonPossibilities ++ specificShapePossibilities

-------------------------------------------------------------------------------
instance Dimension d => Arbitrary (Expression d R) where
  arbitrary = sized genExpR

instance Dimension d => Arbitrary (Expression d C) where
  arbitrary = sized genExpC

-------------------------------------------------------------------------------
data Suite d et
  = Suite (Expression d et) ValMaps
  deriving (Show)

instance Dimension d => Arbitrary (Suite d R) where
  arbitrary = do
    exp <- arbitrary
    valMap <- genValMapFor exp
    return $ Suite exp valMap

instance Dimension d => Arbitrary (Suite d C) where
  arbitrary = do
    exp <- arbitrary
    valMap <- genValMapFor exp
    return $ Suite exp valMap
-------------------------------------------------------------------------------
type SuiteScalarR = Suite Scalar R

type SuiteScalarC = Suite Scalar C

type SuiteOneR = Suite (D1 Default1D) R

type SuiteOneC = Suite (D1 Default1D) C

type SuiteTwoR = Suite (D2 Default2D1 Default2D2) R

type SuiteTwoC = Suite (D2 Default2D1 Default2D2) C
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
data ArbitraryExpresion = forall d et. (Dimension d) => ArbitraryExpresion (Expression d et)

instance Show ArbitraryExpresion where
  show (ArbitraryExpresion exp) = show exp

instance Arbitrary ArbitraryExpresion where
  arbitrary =
    let option1 =
          fmap ArbitraryExpresion (arbitrary :: Gen (Expression Scalar R))
        option2 =
          fmap ArbitraryExpresion (arbitrary :: Gen (Expression Scalar C))
        option3 =
          fmap
            ArbitraryExpresion
            (arbitrary :: Gen (Expression (D1 Default1D) R))
        option4 =
          fmap
            ArbitraryExpresion
            (arbitrary :: Gen (Expression (D1 Default1D) C))
        option5 =
          fmap
            ArbitraryExpresion
            (arbitrary :: Gen (Expression (D2 Default2D1 Default2D2) R))
        option6 =
          fmap
            ArbitraryExpresion
            (arbitrary :: Gen (Expression (D2 Default2D1 Default2D2) C))
     in oneof [option1, option2, option3, option4, option5, option6]

-- |
getWrappedExp :: ArbitraryExpresion -> (ExpressionMap, NodeID)
getWrappedExp (ArbitraryExpresion (Expression n mp)) = (mp, n)

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
