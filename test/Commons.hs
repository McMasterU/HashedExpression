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
import Data.Time (diffUTCTime, getCurrentTime)
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
measureTime :: IO a -> IO ()
measureTime action = do
    beforeTime <- getCurrentTime
    action
    afterTime <- getCurrentTime
    putStrLn $ "Took " ++ show (diffUTCTime afterTime beforeTime) ++ " seconds"

-- |
--
vectorSize :: Int
vectorSize = 10

-- | Vars list
--
type Vars = [[String]] -- Vars 0D, 1D, 2D, 3D, ..

mergeVars :: [Vars] -> Vars
mergeVars = foldl f [[], [], [], []]
  where
    f x y = map removeDuplicate $ zipWith (++) x y

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

shouldApprox :: Approximable a => a -> a -> Expectation
shouldApprox x y = x ~= y `shouldBe` True

infix 1 `shouldApprox`

-------------------------------------------------------------------------------
-- | MARK: Gen functions Zero R
--
--
-------------------------------------------------------------------------------
primitiveZeroR :: Gen (Expression Zero R, Vars)
primitiveZeroR = do
    name <- elements . map pure $ ['a' .. 'z']
    dbl <- arbitrary
    elements $
        replicate 6 (var name, [[name], [], [], []]) ++
        replicate 4 (const dbl, [[], [], [], []])

operandZeroR :: Gen (Expression Zero R, Vars)
operandZeroR = oneof $ replicate 9 primitiveZeroR ++ replicate 2 genZeroR -- ratio 9 / 2

fromNaryZeroR ::
       ([Expression Zero R] -> Expression Zero R)
    -> Gen (Expression Zero R, Vars)
fromNaryZeroR f = do
    numOperands <- elements [3 .. 4]
    ons <- vectorOf numOperands operandZeroR
    let exp = f . map fst $ ons
        vars = mergeVars . map snd $ ons
    return (exp, vars)

fromUnaryZeroR ::
       (Expression Zero R -> Expression Zero R) -> Gen (Expression Zero R, Vars)
fromUnaryZeroR f = do
    on <- operandZeroR
    let exp = f . fst $ on
        names = snd on
    return (exp, names)

fromBinaryZeroR ::
       (Expression Zero R -> Expression Zero R -> Expression Zero R)
    -> Gen (Expression Zero R, Vars)
fromBinaryZeroR f = do
    on1 <- operandZeroR
    on2 <- operandZeroR
    let exp = f (fst on1) (fst on2)
        vars = mergeVars [snd on1, snd on2]
    return (exp, vars)

genZeroR :: Gen (Expression Zero R, Vars)
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
instance Arbitrary SuiteZeroR where
    arbitrary = do
        (exp, vars) <- genZeroR
        valMaps <- genValMaps vars
        return $ SuiteZeroR exp valMaps

-------------------------------------------------------------------------------
-- | MARK: Gen functions Zero C
--
--
-------------------------------------------------------------------------------
primitiveZeroC :: Gen (Expression Zero C, Vars)
primitiveZeroC = do
    name1 <- elements . map pure $ ['a' .. 'z']
    name2 <- elements . map pure $ ['a' .. 'z']
    dbl <- arbitrary
    elements
        [ (var name1 +: var name2, [[name1, name2], [], [], []])
        , (const dbl +: const 0, [[], [], [], []])
        ]

operandZeroC :: Gen (Expression Zero C, Vars)
operandZeroC = oneof $ replicate 9 primitiveZeroC ++ replicate 1 genZeroC -- ratio 9 / 1

fromNaryZeroC ::
       ([Expression Zero C] -> Expression Zero C)
    -> Gen (Expression Zero C, Vars)
fromNaryZeroC f = do
    numOperands <- elements [3]
    ons <- vectorOf numOperands operandZeroC
    let exp = f . map fst $ ons
        vars = mergeVars . map snd $ ons
    return (exp, vars)

fromUnaryZeroC ::
       (Expression Zero C -> Expression Zero C) -> Gen (Expression Zero C, Vars)
fromUnaryZeroC f = do
    on <- operandZeroC
    let exp = f . fst $ on
        vars = snd on
    return (exp, vars)

fromBinaryZeroC ::
       (Expression Zero C -> Expression Zero C -> Expression Zero C)
    -> Gen (Expression Zero C, Vars)
fromBinaryZeroC f = do
    on1 <- operandZeroC
    on2 <- operandZeroC
    let exp = f (fst on1) (fst on2)
        vars = mergeVars [snd on1, snd on2]
    return (exp, vars)

fromRealImagZeroC :: Gen (Expression Zero C, Vars)
fromRealImagZeroC = do
    on1 <- genZeroR
    on2 <- genZeroR
    let exp = fst on1 +: fst on2
        vars = mergeVars [snd on1, snd on2]
    return (exp, vars)

genZeroC :: Gen (Expression Zero C, Vars)
genZeroC = do
    let nary = map fromNaryZeroC [sum, product]
        binary = map fromBinaryZeroC [(*.), (+), (-), (<.>)]
        unary = map fromUnaryZeroC [negate, (^ 2)]
    oneof ([fromRealImagZeroC, primitiveZeroC] ++ nary ++ binary ++ unary)

instance Arbitrary (Expression Zero C) where
    arbitrary = fmap fst genZeroC

data SuiteZeroC =
    SuiteZeroC (Expression Zero C) ValMaps

instance Show SuiteZeroC where
    show (SuiteZeroC e valMaps) =
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

instance Arbitrary SuiteZeroC where
    arbitrary = do
        (exp, names) <- genZeroC
        valMaps <- genValMaps names
        return $ SuiteZeroC exp valMaps

-------------------------------------------------------------------------------
-- | MARK: Gen functions One R
--
--
-------------------------------------------------------------------------------
primitiveOneR :: Gen (Expression One R, Vars)
primitiveOneR = do
    name <- elements . map ((++ "1") . pure) $ ['A' .. 'Z']
    dbl <- arbitrary
    elements $
        replicate 6 (var1d vectorSize name, [[], [name], [], []]) ++
        replicate 4 (const1d vectorSize dbl, [[], [], [], []])

operandOneR :: Gen (Expression One R, Vars)
operandOneR = oneof $ replicate 9 primitiveOneR ++ replicate 2 genOneR -- ratio 9 / 2

fromNaryOneR ::
       ([Expression One R] -> Expression One R)
    -> Gen (Expression One R, Vars)
fromNaryOneR f = do
    numOperands <- elements [3 .. 4]
    ons <- vectorOf numOperands operandOneR
    let exp = f . map fst $ ons
        vars = mergeVars . map snd $ ons
    return (exp, vars)

fromUnaryOneR ::
       (Expression One R -> Expression One R) -> Gen (Expression One R, Vars)
fromUnaryOneR f = do
    on <- operandOneR
    let exp = f . fst $ on
        names = snd on
    return (exp, names)

fromBinaryOneR ::
       (Expression One R -> Expression One R -> Expression One R)
    -> Gen (Expression One R, Vars)
fromBinaryOneR f = do
    on1 <- operandOneR
    on2 <- operandOneR
    let exp = f (fst on1) (fst on2)
        vars = mergeVars [snd on1, snd on2]
    return (exp, vars)

fromScaleOneR :: Gen (Expression One R, Vars)
fromScaleOneR = do
    scalar <- operandZeroR
    scalee <- operandOneR
    let exp = fst scalar *. fst scalee
        vars = mergeVars [snd scalar, snd scalee]
    return (exp, vars)

genOneR :: Gen (Expression One R, Vars)
genOneR = do
    let nary = map fromNaryOneR [sum, product]
        binary = map fromBinaryOneR [(+), (-)]
        unary = map fromUnaryOneR [negate, (^ 2)]
    oneof ([primitiveOneR] ++ nary ++ binary ++ unary ++ [fromScaleOneR])

instance Arbitrary (Expression One R) where
    arbitrary = fmap fst genOneR

-- |
--
data SuiteOneR =
    SuiteOneR (Expression One R) ValMaps

instance Show SuiteOneR where
    show (SuiteOneR e valMaps) =
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
instance Arbitrary SuiteOneR where
    arbitrary = do
        (exp, vars) <- genOneR
        valMaps <- genValMaps vars
        return $ SuiteOneR exp valMaps
