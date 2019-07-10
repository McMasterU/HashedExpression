{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Commons where

import Control.Applicative (liftA2)
import Control.Monad (forM)
import Data.Array
import Data.Complex
import Data.Function.HT (nest)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
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
import HashedUtils
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
vectorSize :: Int
vectorSize = 10

-- | Approximable class
--
class Approximable a where
    (~=) :: a -> a -> Bool

infix 4 ~=

shouldApprox :: Approximable a => a -> a -> Expectation
shouldApprox x y = x ~= y `shouldBe` True

infix 1 `shouldApprox`

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

-- | Vars list
--
type Vars = [[String]] -- Vars 0D, 1D, 2D, 3D, ..

mergeVars :: [Vars] -> Vars
mergeVars = foldl f [[], [], [], []]
  where
    f x y = map removeDuplicate $ zipWith (++) x y

-------------------------------------------------------------------------------
-- | MARK: Gen functions R
--
--
-------------------------------------------------------------------------------
primitiveZeroR1 :: Gen (Expression Zero R, Vars)
primitiveZeroR1 = do
    name <- elements . map pure $ ['a' .. 'z']
    dbl <- arbitrary
    elements $
        replicate 6 (var name, [[name], [], [], []]) ++
        replicate 4 (const dbl, [[], [], [], []])

operandR1 :: Gen (Expression Zero R, Vars)
operandR1
    -- 80% getting a primitive, 20% get a nested
 = oneof $ replicate 8 primitiveZeroR1 ++ replicate 2 genZeroR1

fromNaryZeroR1 ::
       ([Expression Zero R] -> Expression Zero R)
    -> Gen (Expression Zero R, Vars)
fromNaryZeroR1 f = do
    numOperands <- elements [3 .. 6]
    ons <- vectorOf numOperands operandR1
    let exp = f . map fst $ ons
        vars = mergeVars . map snd $ ons
    return (exp, vars)

fromUnaryZeroR1 ::
       (Expression Zero R -> Expression Zero R) -> Gen (Expression Zero R, Vars)
fromUnaryZeroR1 f = do
    on <- operandR1
    let exp = f . fst $ on
        names = snd on
    return (exp, names)

fromBinaryZeroR1 ::
       (Expression Zero R -> Expression Zero R -> Expression Zero R)
    -> Gen (Expression Zero R, Vars)
fromBinaryZeroR1 f = do
    on1 <- operandR1
    on2 <- operandR1
    let exp = f (fst on1) (fst on2)
        vars = mergeVars [snd on1, snd on2]
    return (exp, vars)

genZeroR1 :: Gen (Expression Zero R, Vars)
genZeroR1 = do
    let nary = map fromNaryZeroR1 [sum, product]
        binary = map fromBinaryZeroR1 [(*.), (+), (-), (<.>)]
        unary = map fromUnaryZeroR1 [negate, (^ 2), (^ 3)]
    oneof ([primitiveZeroR1] ++ nary ++ binary ++ unary)

primitiveZeroR :: Gen (Expression Zero R, [String])
primitiveZeroR = do
    name <- elements . map pure $ ['a' .. 'z']
    dbl <- arbitrary
    elements $ replicate 6 (var name, [name]) ++ replicate 4 (const dbl, [])

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

-- |
--
data SuiteZeroR =
    SuiteZeroR (Expression Zero R) ValMaps

instance Show SuiteZeroR where
    show (SuiteZeroR e valMaps) =
        format
            [ ("Expr", exp)
            , ("Simplified", simplifiedExp)
            , ("ValMap", show valMaps)
            ]
      where
        exp = prettify e
        simplifiedExp = prettify . simplify $ e
        evalExp = eval valMaps e
        evalSimplified = eval valMaps $ simplify e

-- |
--
genValMaps :: Vars -> Gen ValMaps
genValMaps vars = do
    let [names0d, names1d, names2d, names3d] = vars
        -- vm0
    list0d <- vectorOf (length names0d) arbitrary
    let vm0 = Map.fromList $ zip names0d list0d
        -- vm1
    list1d <- vectorOf (length names1d) . vectorOf vectorSize $ arbitrary
    let vm1 =
            Map.fromList . zip names1d . map (listArray (0, vectorSize - 1)) $
            list1d
    list2d <-
        vectorOf (length names1d) . vectorOf (vectorSize * vectorSize) $
        arbitrary
    let vm2 =
            Map.fromList .
            zip names2d .
            map (listArray ((0, 0), (vectorSize - 1, vectorSize - 1))) $
            list2d
    list3d <-
        vectorOf (length names1d) .
        vectorOf (vectorSize * vectorSize * vectorSize) $
        arbitrary
    let vm3 =
            Map.fromList .
            zip names3d .
            map
                (listArray
                     ( (0, 0, 0)
                     , (vectorSize - 1, vectorSize - 1, vectorSize - 1))) $
            list3d
    return (ValMaps vm0 vm1 vm2 vm3)

-- |
--
instance Arbitrary SuiteZeroR where
    arbitrary = do
        (exp, vars) <- genZeroR1
        valMaps <- genValMaps vars
        return $ SuiteZeroR exp valMaps

-------------------------------------------------------------------------------
-- | MARK: Gen functions C
--
--
-------------------------------------------------------------------------------
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

data SuiteZeroC =
    SuiteZeroC (Expression Zero C) (Map String Double)

instance Show SuiteZeroC where
    show (SuiteZeroC e valMap) =
        format
            [ ("Expr", exp)
            , ("Simplified", simplifiedExp)
            , ("ValMap", show valMap)
            ]
      where
        exp = prettify e
        simplifiedExp = prettify . simplify $ e
        evalExp = eval (emptyVms |> withVm0 valMap) e
        evalSimplified = eval (emptyVms |> withVm0 valMap) $ simplify e

instance Arbitrary SuiteZeroC where
    arbitrary = do
        (exp, names) <- genZeroC
        doubles <- vectorOf (length names) arbitrary
        let valMap = Map.fromList $ zip names doubles
        return $ SuiteZeroC exp valMap
