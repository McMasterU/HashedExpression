{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Commons where

import Control.Applicative (liftA2)
import Control.Monad (forM)
import Data.Array
import Data.Complex
import Data.Function.HT (nest)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Set (Set, fromList, toList)
import Data.Typeable (Typeable)
import GHC.IO.Unsafe (unsafePerformIO)
import HashedExpression
import HashedInterp
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
import HashedSimplify
import HashedUtils ((|>))
import HashedVar
import Prelude hiding
    ( (*)
    , (+)
    , (-)
    , (/)
    , (^)
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
    , tan
    , tanh
    )
import Test.Hspec
import Test.QuickCheck

-- |
--
-- | Remove duplicate but also sort
--
removeDuplicate :: (Ord a) => [a] -> [a]
removeDuplicate = toList . fromList

-- | Format
--
format :: [(String, String)] -> String
format = intercalate "\n" . map oneLine
  where
    oneLine (f, s) = f ++ ": " ++ s

-- |
--
relativeError :: Double -> Double -> Double
relativeError a b = abs (a - b) / max (abs a) (abs b)

-- |
--
sum :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
sum = fromJust . HashedOperation.sum

product :: (DimensionType d, NumType et) => [Expression d et] -> Expression d et
product = fromJust . HashedOperation.product

-- |
--
inspect :: (Typeable d, Typeable rc) => Expression d rc -> Expression d rc
inspect x =
    unsafePerformIO $ do
        showExp x
        return x

-- |
--
-- | Approximable class
--
class Approximable a where
    (~=) :: a -> a -> Bool

infix 4 ~=

instance Approximable Double where
    (~=) :: Double -> Double -> Bool
    a ~= b
        | a == b = True
        | otherwise = relativeError a b < 0.01

instance Approximable (Complex Double) where
    (~=) :: Complex Double -> Complex Double -> Bool
    a ~= b = (realPart a ~= realPart b) && (imagPart a ~= imagPart b)

instance Approximable (Array Int Double) where
    (~=) :: Array Int Double -> Array Int Double -> Bool
    a ~= b = (indices a == indices b) && and (zipWith (~=) (elems a) (elems b))

instance Approximable (Array Int (Complex Double)) where
    (~=) :: Array Int (Complex Double) -> Array Int (Complex Double) -> Bool
    a ~= b = (indices a == indices b) && and (zipWith (~=) (elems a) (elems b))

instance Approximable (Array (Int, Int) Double) where
    (~=) :: Array (Int, Int) Double -> Array (Int, Int) Double -> Bool
    a ~= b = (indices a == indices b) && and (zipWith (~=) (elems a) (elems b))

instance Approximable (Array (Int, Int) (Complex Double)) where
    (~=) ::
           Array (Int, Int) (Complex Double)
        -> Array (Int, Int) (Complex Double)
        -> Bool
    a ~= b = (indices a == indices b) && and (zipWith (~=) (elems a) (elems b))

instance Approximable (Array (Int, Int, Int) Double) where
    (~=) :: Array (Int, Int, Int) Double -> Array (Int, Int, Int) Double -> Bool
    a ~= b = (indices a == indices b) && and (zipWith (~=) (elems a) (elems b))

instance Approximable (Array (Int, Int, Int) (Complex Double)) where
    (~=) ::
           Array (Int, Int, Int) (Complex Double)
        -> Array (Int, Int, Int) (Complex Double)
        -> Bool
    a ~= b = (indices a == indices b) && and (zipWith (~=) (elems a) (elems b))

-- | MARK: Gen functions R
--
primitiveZeroR :: Gen (Expression Zero R, [String])
primitiveZeroR = do
    name <- elements . map pure $ ['a' .. 'z']
    dbl <- arbitrary
    elements [(var name, [name]), (const dbl, [])]

operandR :: Gen (Expression Zero R, [String])
operandR
    -- 80% getting a primitive, 20% get a nested
 = oneof $ replicate 8 primitiveZeroR ++ replicate 2 genZeroR

fromNaryZeroR ::
       ([Expression Zero R] -> Expression Zero R)
    -> Gen (Expression Zero R, [String])
fromNaryZeroR f = do
    numOperands <- elements [3 .. 6]
    ons <- vectorOf numOperands operandR
    let exp = f . map fst $ ons
        names = removeDuplicate . concatMap snd $ ons
    return (exp, names)

fromUnaryZeroR ::
       (Expression Zero R -> Expression Zero R)
    -> Gen (Expression Zero R, [String])
fromUnaryZeroR f = do
    on <- operandR
    let exp = f . fst $ on
        names = snd on
    return (exp, names)

fromBinaryZeroR ::
       (Expression Zero R -> Expression Zero R -> Expression Zero R)
    -> Gen (Expression Zero R, [String])
fromBinaryZeroR f = do
    on1 <- operandR
    on2 <- operandR
    let exp = f (fst on1) (fst on2)
        names = removeDuplicate $ snd on1 ++ snd on2
    return (exp, names)

genZeroR :: Gen (Expression Zero R, [String])
genZeroR = do
    let nary = map fromNaryZeroR [sum, product]
        binary = map fromBinaryZeroR [(*.), (+), (-), (<.>)]
        unary = map fromUnaryZeroR [negate, (^ 2), (^ 3)]
    oneof ([primitiveZeroR] ++ nary ++ binary ++ unary)

instance Arbitrary (Expression Zero R) where
    arbitrary = fmap fst genZeroR

-- | MARK: Gen functions C
--
primitiveZeroC :: Gen (Expression Zero C, [String])
primitiveZeroC = do
    name1 <- elements . map pure $ ['a' .. 'z']
    name2 <- elements . map pure $ ['a' .. 'z']
    dbl <- arbitrary
    elements
        [(var name1 +: var name2, [name1, name2]), (const dbl +: const 0, [])]

operandC :: Gen (Expression Zero C, [String])
operandC
    -- 80% getting a primitive, 20% get a nested
 = oneof $ replicate 8 primitiveZeroC ++ replicate 2 genZeroC

fromNaryZeroC ::
       ([Expression Zero C] -> Expression Zero C)
    -> Gen (Expression Zero C, [String])
fromNaryZeroC f = do
    numOperands <- elements [3 .. 6]
    ons <- vectorOf numOperands operandC
    let exp = f . map fst $ ons
        names = removeDuplicate . concatMap snd $ ons
    return (exp, names)

fromUnaryZeroC ::
       (Expression Zero C -> Expression Zero C)
    -> Gen (Expression Zero C, [String])
fromUnaryZeroC f = do
    on <- operandC
    let exp = f . fst $ on
        names = snd on
    return (exp, names)

fromBinaryZeroC ::
       (Expression Zero C -> Expression Zero C -> Expression Zero C)
    -> Gen (Expression Zero C, [String])
fromBinaryZeroC f = do
    on1 <- operandC
    on2 <- operandC
    let exp = f (fst on1) (fst on2)
        names = removeDuplicate $ snd on1 ++ snd on2
    return (exp, names)

fromRealImagZeroC :: Gen (Expression Zero C, [String])
fromRealImagZeroC = do
    on1 <- genZeroR
    on2 <- genZeroR
    let exp = fst on1 +: fst on2
        names = removeDuplicate $ snd on1 ++ snd on2
    return (exp, names)

genZeroC :: Gen (Expression Zero C, [String])
genZeroC = do
    let nary = map fromNaryZeroC [sum, product]
        binary = map fromBinaryZeroC [(*.), (+), (-), (<.>)]
        unary = map fromUnaryZeroC [negate, (^ 2), (^ 3)]
    oneof ([fromRealImagZeroC, primitiveZeroC] ++ nary ++ binary ++ unary)

instance Arbitrary (Expression Zero C) where
    arbitrary = fmap fst genZeroC
