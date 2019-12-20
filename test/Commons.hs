{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Commons where

import Control.Applicative (liftA2)
import Control.Monad (foldM, forM)
import Data.Array
import Data.Complex
import Data.Function.HT (nest)
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Set (Set, fromList, toList)
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Typeable (Typeable)
import Debug.Trace (traceShowId)
import GHC.IO.Unsafe (unsafePerformIO)
import HashedExpression
import HashedInterp
import HashedNormalize
import HashedOperation
import qualified HashedOperation
import HashedPrettify
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
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

-- |
--
-- | Remove duplicate but also sort
--
removeDuplicate :: (Ord a) => [a] -> [a]
removeDuplicate = toList . fromList

-- | 
--
vectorOfDifferent :: Eq a => Int -> Gen a -> Gen [a]
vectorOfDifferent sz gen = foldM f [] [1 .. sz]
  where
    f acc _ = (: acc) <$> gen `suchThat` (not . flip elem acc)

-- | Format
--
format :: [(String, String)] -> String
format = intercalate "\n" . map oneLine
  where
    oneLine (f, s) = f ++ ": " ++ s

-- |
--
inspect :: (Typeable d, Typeable rc) => Expression d rc -> Expression d rc
inspect x =
    unsafePerformIO $ do
        showExp x
        return x

-- | Vars list
--
type Vars = [[String]] -- Vars 0D, 1D, 2D, 3D, ..

mergeVars :: [Vars] -> Vars
mergeVars = foldl f [[], [], [], []]
  where
    f x y = map removeDuplicate $ zipWith (++) x y

genDouble :: Gen Double
genDouble = arbitrary `suchThat` inSmallRange
  where
    inSmallRange x = x >= 0 && x <= 10

-- |
--
genValMap :: Vars -> Gen ValMaps
genValMap vars = do
    let [names0d, names1d, names2d, names3d] = vars
    list0d <- vectorOf (length names0d) genDouble
    let vm0 = Map.fromList . zip names0d $ map VScalar list0d
    list1d <- vectorOf (length names1d) . vectorOf defaultDim1D $ genDouble
    let vm1 =
            Map.fromList .
            zip names1d . map (V1D . listArray (0, defaultDim1D - 1)) $
            list1d
    list2d <-
        vectorOf (length names2d) . vectorOf (default1stDim2D * default2ndDim2D) $
        genDouble
    let vm2 =
            Map.fromList .
            zip names2d .
            map
                (V2D .
                 listArray ((0, 0), (default1stDim2D - 1, default2ndDim2D - 1))) $
            list2d
    list3d <-
        vectorOf (length names3d) .
        vectorOf (default1stDim2D * default2ndDim2D * default3rdDim3D) $
        genDouble
    let vm3 =
            Map.fromList .
            zip names3d .
            map
                (V3D .
                 listArray
                     ( (0, 0, 0)
                     , ( default1stDim3D - 1
                       , default2ndDim3D - 1
                       , default3rdDim3D - 1))) $
            list3d
    return $ Map.unions [vm0, vm1, vm2, vm3]

shouldApprox :: (HasCallStack, Approximable a) => a -> a -> Expectation
shouldApprox x y = assertBool msg (x ~= y)
  where
    msg = "Expected: " ++ prettifyShow x ++ "\nGot: " ++ prettifyShow y

infix 1 `shouldApprox`

withRatio :: [(Int, a)] -> [a]
withRatio = concatMap (uncurry replicate)

-------------------------------------------------------------------------------
-- | MARK: Gen functions Scalar R
--
--
-------------------------------------------------------------------------------
primitiveScalarR :: Gen (Expression Scalar R, Vars)
primitiveScalarR = do
    name <- elements . map pure $ ['a' .. 'z']
    dbl <- genDouble
    elements . withRatio $
        [ (6, (var name, [[name], [], [], []]))
        , (4, (const dbl, [[], [], [], []]))
        ]

operandScalarR :: Gen (Expression Scalar R, Vars)
operandScalarR = oneof . withRatio $ [(8, primitiveScalarR), (2, genScalarR)]

genScalarR :: Gen (Expression Scalar R, Vars)
genScalarR =
    oneof . withRatio $
    [ (4, fromNaryScalarR sum)
    , (4, fromNaryScalarR product)
    , (4, fromBinaryScalarR (*.))
    , (4, fromBinaryScalarR (+))
    , (4, fromBinaryScalarR (*))
    , (4, fromBinaryScalarR (-))
    , (4, fromBinaryScalarR (<.>))
    , (2, fromUnaryScalarR negate)
    , (1, fromUnaryScalarR (^ 2))
    , (1, fromInnerProdHigherScalarR)
    , (2, fromScalarCScalarR)
    , (1, fromPiecewiseScalarR)
    ]
    --    replicate 8 primitiveScalarR ++ replicate 2 genScalarR
  where
    fromPiecewiseScalarR :: Gen (Expression Scalar R, Vars)
    fromPiecewiseScalarR = do
        numBranches <- elements [2 .. 4]
        branches <- vectorOf numBranches operandScalarR
        condition <- operandScalarR
        marks <- sort <$> vectorOfDifferent (numBranches - 1) arbitrary
        let vars = mergeVars $ map snd branches ++ [snd condition]
            exp = piecewise marks (fst condition) $ map fst branches
        return (exp, vars)
    fromNaryScalarR ::
           ([Expression Scalar R] -> Expression Scalar R)
        -> Gen (Expression Scalar R, Vars)
    fromNaryScalarR f = do
        numOperands <- elements [3 .. 4]
        ons <- vectorOf numOperands operandScalarR
        let exp = f . map fst $ ons
            vars = mergeVars . map snd $ ons
        return (exp, vars)
    fromInnerProdHigherScalarR :: Gen (Expression Scalar R, Vars)
    fromInnerProdHigherScalarR = do
        operand1 <- genOneR
        operand2 <- genOneR
        let exp = fst operand1 <.> fst operand2
            vars = mergeVars [snd operand1, snd operand2]
        return (exp, vars)
    fromUnaryScalarR ::
           (Expression Scalar R -> Expression Scalar R)
        -> Gen (Expression Scalar R, Vars)
    fromUnaryScalarR f = do
        on <- operandScalarR
        let exp = f . fst $ on
            names = snd on
        return (exp, names)
    fromBinaryScalarR ::
           (Expression Scalar R -> Expression Scalar R -> Expression Scalar R)
        -> Gen (Expression Scalar R, Vars)
    fromBinaryScalarR f = do
        on1 <- operandScalarR
        on2 <- operandScalarR
        let exp = f (fst on1) (fst on2)
            vars = mergeVars [snd on1, snd on2]
        return (exp, vars)
    fromScalarCScalarR :: Gen (Expression Scalar R, Vars)
    fromScalarCScalarR = do
        rand <- elements [True, False]
        (zeroC, vars) <- genScalarC
        let exp =
                if rand
                    then xRe zeroC
                    else xIm zeroC
        return (exp, vars)

instance Arbitrary (Expression Scalar R) where
    arbitrary = fmap fst genScalarR

-- |
--
data SuiteScalarR =
    SuiteScalarR (Expression Scalar R) ValMaps

instance Show SuiteScalarR where
    show (SuiteScalarR e valMaps) =
        format [("Expr", exp), ("ValMap", show valMaps)]
      where
        exp = prettifyDebug e
        normalizedExp = prettifyDebug . normalize $ e
        evalExp = eval valMaps e
        evalNormalized = eval valMaps $ normalize e

-- |
--
instance Arbitrary SuiteScalarR where
    arbitrary = do
        (exp, vars) <- genScalarR
        valMaps <- genValMap vars
        return $ SuiteScalarR exp valMaps

-------------------------------------------------------------------------------
-- | MARK: Gen functions Scalar C
--
--
-------------------------------------------------------------------------------
primitiveScalarC :: Gen (Expression Scalar C, Vars)
primitiveScalarC = do
    name1 <- elements . map pure $ ['a' .. 'z']
    name2 <- elements . map pure $ ['a' .. 'z']
    dbl <- genDouble
    elements . withRatio $
        [ (1, (var name1 +: var name2, [[name1, name2], [], [], []]))
        , (1, (const dbl +: const 0, [[], [], [], []]))
        ]

operandScalarC :: Gen (Expression Scalar C, Vars)
operandScalarC = oneof . withRatio $ [(9, primitiveScalarC), (1, genScalarC)]

genScalarC :: Gen (Expression Scalar C, Vars)
genScalarC =
    oneof . withRatio $
    [ (6, fromNaryScalarC sum)
    , (3, fromNaryScalarC product)
    , (6, fromBinaryScalarC (*.))
    , (6, fromBinaryScalarC (+))
    , (3, fromBinaryScalarC (*))
    , (6, fromBinaryScalarC (-))
    , (3, fromBinaryScalarC (<.>))
    , (3, fromUnaryScalarC negate)
    , (1, fromUnaryScalarC (^ 2))
    , (1, fromInnerProdHigherScalarC)
    , (2, fromRealImagScalarC)
    , (1, fromPiecewiseScalarC)
    ]
  where
    fromPiecewiseScalarC :: Gen (Expression Scalar C, Vars)
    fromPiecewiseScalarC = do
        numBranches <- elements [2 .. 4]
        branches <- vectorOf numBranches operandScalarC
        condition <- operandScalarR
        marks <- sort <$> vectorOfDifferent (numBranches - 1) arbitrary
        let vars = mergeVars $ map snd branches ++ [snd condition]
            exp = piecewise marks (fst condition) $ map fst branches
        return (exp, vars)
    fromNaryScalarC ::
           ([Expression Scalar C] -> Expression Scalar C)
        -> Gen (Expression Scalar C, Vars)
    fromNaryScalarC f = do
        numOperands <- elements [3]
        ons <- vectorOf numOperands operandScalarC
        let exp = f . map fst $ ons
            vars = mergeVars . map snd $ ons
        return (exp, vars)
    fromUnaryScalarC ::
           (Expression Scalar C -> Expression Scalar C)
        -> Gen (Expression Scalar C, Vars)
    fromUnaryScalarC f = do
        on <- operandScalarC
        let exp = f . fst $ on
            vars = snd on
        return (exp, vars)
    fromInnerProdHigherScalarC :: Gen (Expression Scalar C, Vars)
    fromInnerProdHigherScalarC = do
        operand1 <- genOneC
        operand2 <- genOneC
        let exp = fst operand1 <.> fst operand2
            vars = mergeVars [snd operand1, snd operand2]
        return (exp, vars)
    fromBinaryScalarC ::
           (Expression Scalar C -> Expression Scalar C -> Expression Scalar C)
        -> Gen (Expression Scalar C, Vars)
    fromBinaryScalarC f = do
        on1 <- operandScalarC
        on2 <- operandScalarC
        let exp = f (fst on1) (fst on2)
            vars = mergeVars [snd on1, snd on2]
        return (exp, vars)
    fromRealImagScalarC :: Gen (Expression Scalar C, Vars)
    fromRealImagScalarC = do
        on1 <- genScalarR
        on2 <- genScalarR
        let exp = fst on1 +: fst on2
            vars = mergeVars [snd on1, snd on2]
        return (exp, vars)

instance Arbitrary (Expression Scalar C) where
    arbitrary = fmap fst genScalarC

data SuiteScalarC =
    SuiteScalarC (Expression Scalar C) ValMaps

instance Show SuiteScalarC where
    show (SuiteScalarC e valMaps) =
        format [("Expr", exp), ("ValMap", show valMaps)]
      where
        exp = prettifyDebug e
        normalizedExp = prettifyDebug . normalize $ e
        evalExp = eval valMaps e
        evalNormalized = eval valMaps $ normalize e

instance Arbitrary SuiteScalarC where
    arbitrary = do
        (exp, names) <- genScalarC
        valMaps <- genValMap names
        return $ SuiteScalarC exp valMaps

-------------------------------------------------------------------------------
-- | MARK: Gen functions One R
--
--
-------------------------------------------------------------------------------
primitiveOneR :: Gen (Expression One R, Vars)
primitiveOneR = do
    name <- elements . map ((++ "1") . pure) $ ['a' .. 'z']
    dbl <- genDouble
    elements . withRatio $
        [ (6, (var1d defaultDim1D name, [[], [name], [], []]))
        , (4, (const1d defaultDim1D dbl, [[], [], [], []]))
        ]

operandOneR :: Gen (Expression One R, Vars)
operandOneR = oneof . withRatio $ [(8, primitiveOneR), (2, genOneR)]

genOneR :: Gen (Expression One R, Vars)
genOneR =
    oneof . withRatio $
    [ (4, fromNaryOneR sum)
    , (4, fromNaryOneR product)
    , (4, fromBinaryOneR (+))
    , (4, fromBinaryOneR (*))
    , (4, fromBinaryOneR (-))
    , (3, fromUnaryOneR negate)
    , (1, fromUnaryOneR (^ 2))
    , (1, fromScaleOneR)
    , (1, fromOneCOneR)
    , (3, fromRotateOneR)
    , (1, fromPiecewiseOneR)
    , (1, fromFourierTransformOneR)
    ]
  where
    fromFourierTransformOneR :: Gen (Expression One R, Vars)
    fromFourierTransformOneR = do
        on <- operandOneR
        reOrIm <- elements [xRe, xIm]
        return (reOrIm . ft . fst $ on, snd on)
    fromPiecewiseOneR :: Gen (Expression One R, Vars)
    fromPiecewiseOneR = do
        numBranches <- elements [2 .. 4]
        branches <- vectorOf numBranches operandOneR
        condition <- operandOneR
        marks <- sort <$> vectorOfDifferent (numBranches - 1) arbitrary
        let vars = mergeVars $ map snd branches ++ [snd condition]
            exp = piecewise marks (fst condition) $ map fst branches
        return (exp, vars)
    fromRotateOneR :: Gen (Expression One R, Vars)
    fromRotateOneR = do
        on <- operandOneR
        amount <- elements [-9 .. 9]
        return (rotate amount $ fst on, snd on)
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
           (Expression One R -> Expression One R)
        -> Gen (Expression One R, Vars)
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
        scalar <- operandScalarR
        scalee <- operandOneR
        let exp = fst scalar *. fst scalee
            vars = mergeVars [snd scalar, snd scalee]
        return (exp, vars)
    fromOneCOneR :: Gen (Expression One R, Vars)
    fromOneCOneR = do
        rand <- elements [True, False]
        (oneC, vars) <- genOneC
        let exp =
                if rand
                    then xRe oneC
                    else xIm oneC
        return (exp, vars)

instance Arbitrary (Expression One R) where
    arbitrary = fmap fst genOneR

-- |
--
data SuiteOneR =
    SuiteOneR (Expression One R) ValMaps

instance Show SuiteOneR where
    show (SuiteOneR e valMaps) =
        format [("Expr", exp), ("ValMap", show valMaps)]
      where
        exp = prettifyDebug e
        normalizedExp = prettifyDebug . normalize $ e
        evalExp = eval valMaps e
        evalNormalized = eval valMaps $ normalize e

-- |
--
instance Arbitrary SuiteOneR where
    arbitrary = do
        (exp, vars) <- genOneR
        valMaps <- genValMap vars
        return $ SuiteOneR exp valMaps

-------------------------------------------------------------------------------
-- | MARK: Gen functions One C
--
--
-------------------------------------------------------------------------------
primitiveOneC :: Gen (Expression One C, Vars)
primitiveOneC = do
    name1 <- elements . map ((++ "1") . pure) $ ['a' .. 'z']
    name2 <- elements . map ((++ "1") . pure) $ ['a' .. 'z']
    dbl1 <- genDouble
    dbl2 <- genDouble
    elements . withRatio $
        [ ( 6
          , ( var1d defaultDim1D name1 +: var1d defaultDim1D name2
            , [[], [name1, name2], [], []]))
        , ( 4
          , ( const1d defaultDim1D dbl1 +: const1d defaultDim1D dbl2
            , [[], [], [], []]))
        ]

operandOneC :: Gen (Expression One C, Vars)
operandOneC = oneof . withRatio $ [(9, primitiveOneC), (1, genOneC)]

genOneC :: Gen (Expression One C, Vars)
genOneC =
    oneof . withRatio $
    [ (6, fromNaryOneC sum)
    , (3, fromNaryOneC product)
    , (6, fromBinaryOneC (+))
    , (6, fromBinaryOneC (-))
    , (3, fromBinaryOneC (*))
    , (3, fromUnaryOneC negate)
    , (1, fromUnaryOneC (^ 2))
    , (2, fromScaleOneC)
    , (3, fromRotateOneC)
    , (1, fromPiecewiseOneC)
    , (1, fromFourierTransformOneC)
    ]
  where
    fromFourierTransformOneC :: Gen (Expression One C, Vars)
    fromFourierTransformOneC = do
        on <- operandOneC
        return (ft . fst $ on, snd on)
    fromPiecewiseOneC :: Gen (Expression One C, Vars)
    fromPiecewiseOneC = do
        numBranches <- elements [2 .. 4]
        branches <- vectorOf numBranches operandOneC
        condition <- operandOneR
        marks <- sort <$> vectorOfDifferent (numBranches - 1) arbitrary
        let vars = mergeVars $ map snd branches ++ [snd condition]
            exp = piecewise marks (fst condition) $ map fst branches
        return (exp, vars)
    fromRotateOneC :: Gen (Expression One C, Vars)
    fromRotateOneC = do
        on <- operandOneC
        amount <- elements [-9 .. 9]
        return (rotate amount $ fst on, snd on)
    fromNaryOneC ::
           ([Expression One C] -> Expression One C)
        -> Gen (Expression One C, Vars)
    fromNaryOneC f = do
        numOperands <- elements [3]
        ons <- vectorOf numOperands operandOneC
        let exp = f . map fst $ ons
            vars = mergeVars . map snd $ ons
        return (exp, vars)
    fromUnaryOneC ::
           (Expression One C -> Expression One C)
        -> Gen (Expression One C, Vars)
    fromUnaryOneC f = do
        on <- operandOneC
        let exp = f . fst $ on
            names = snd on
        return (exp, names)
    fromBinaryOneC ::
           (Expression One C -> Expression One C -> Expression One C)
        -> Gen (Expression One C, Vars)
    fromBinaryOneC f = do
        on1 <- operandOneC
        on2 <- operandOneC
        let exp = f (fst on1) (fst on2)
            vars = mergeVars [snd on1, snd on2]
        return (exp, vars)
    fromScaleOneC :: Gen (Expression One C, Vars)
    fromScaleOneC = do
        scalarR <- operandScalarR
        scalarC <- operandScalarC
        scalee <- operandOneC
        rand <- elements [True, False]
        let (exp, vars) =
                if rand
                    then ( fst scalarR *. fst scalee
                         , mergeVars [snd scalarR, snd scalee])
                    else ( fst scalarC *. fst scalee
                         , mergeVars [snd scalarC, snd scalee])
        return (exp, vars)

instance Arbitrary (Expression One C) where
    arbitrary = fmap fst genOneC

-- |
--
data SuiteOneC =
    SuiteOneC (Expression One C) ValMaps

instance Show SuiteOneC where
    show (SuiteOneC e valMaps) =
        format [("Expr", exp), ("ValMap", show valMaps)]
      where
        exp = prettifyDebug e
        normalizedExp = prettifyDebug . normalize $ e
        evalExp = eval valMaps e
        evalNormalized = eval valMaps $ normalize e

-- |
--
instance Arbitrary SuiteOneC where
    arbitrary = do
        (exp, vars) <- genOneC
        valMaps <- genValMap vars
        return $ SuiteOneC exp valMaps

-------------------------------------------------------------------------------
-- | MARK: Gen functions Two R
--
--
-------------------------------------------------------------------------------
primitiveTwoR :: Gen (Expression Two R, Vars)
primitiveTwoR = do
    name <- elements . map ((++ "2") . pure) $ ['a' .. 'z']
    dbl <- genDouble
    elements . withRatio $
        [ ( 6
          , ( var2d (default1stDim2D, default2ndDim2D) name
            , [[], [], [name], []]))
        , ( 4
          , (const2d (default1stDim2D, default2ndDim2D) dbl, [[], [], [], []]))
        ]

operandTwoR :: Gen (Expression Two R, Vars)
operandTwoR = oneof . withRatio $ [(8, primitiveTwoR), (2, genTwoR)]

genTwoR :: Gen (Expression Two R, Vars)
genTwoR =
    oneof . withRatio $
    [ (4, fromNaryTwoR sum)
    , (4, fromNaryTwoR product)
    , (4, fromBinaryTwoR (+))
    , (4, fromBinaryTwoR (*))
    , (4, fromBinaryTwoR (-))
    , (3, fromUnaryTwoR negate)
    , (1, fromUnaryTwoR (^ 2))
    , (1, fromScaleTwoR)
    , (1, fromTwoCTwoR)
    , (2, fromRotateTwoR)
    , (1, fromFourierTransformTwoR)
    ]
  where
    fromFourierTransformTwoR :: Gen (Expression Two R, Vars)
    fromFourierTransformTwoR = do
        on <- operandTwoR
        reOrIm <- elements [xRe, xIm]
        return (reOrIm . ft . fst $ on, snd on)
    fromRotateTwoR :: Gen (Expression Two R, Vars)
    fromRotateTwoR = do
        on <- operandTwoR
        amount1 <- elements [-9 .. 9]
        amount2 <- elements [-9 .. 9]
        return (rotate (amount1, amount2) $ fst on, snd on)
    fromNaryTwoR ::
           ([Expression Two R] -> Expression Two R)
        -> Gen (Expression Two R, Vars)
    fromNaryTwoR f = do
        numOperands <- elements [3 .. 4]
        ons <- vectorOf numOperands operandTwoR
        let exp = f . map fst $ ons
            vars = mergeVars . map snd $ ons
        return (exp, vars)
    fromUnaryTwoR ::
           (Expression Two R -> Expression Two R)
        -> Gen (Expression Two R, Vars)
    fromUnaryTwoR f = do
        on <- operandTwoR
        let exp = f . fst $ on
            names = snd on
        return (exp, names)
    fromBinaryTwoR ::
           (Expression Two R -> Expression Two R -> Expression Two R)
        -> Gen (Expression Two R, Vars)
    fromBinaryTwoR f = do
        on1 <- operandTwoR
        on2 <- operandTwoR
        let exp = f (fst on1) (fst on2)
            vars = mergeVars [snd on1, snd on2]
        return (exp, vars)
    fromScaleTwoR :: Gen (Expression Two R, Vars)
    fromScaleTwoR = do
        scalar <- operandScalarR
        scalee <- operandTwoR
        let exp = fst scalar *. fst scalee
            vars = mergeVars [snd scalar, snd scalee]
        return (exp, vars)
    fromTwoCTwoR :: Gen (Expression Two R, Vars)
    fromTwoCTwoR = do
        rand <- elements [True, False]
        (oneC, vars) <- genTwoC
        let exp =
                if rand
                    then xRe oneC
                    else xIm oneC
        return (exp, vars)

instance Arbitrary (Expression Two R) where
    arbitrary = fmap fst genTwoR

-- |
--
data SuiteTwoR =
    SuiteTwoR (Expression Two R) ValMaps

instance Show SuiteTwoR where
    show (SuiteTwoR e valMaps) =
        format [("Expr", exp), ("ValMap", show valMaps)]
      where
        exp = prettifyDebug e
        normalizedExp = prettifyDebug . normalize $ e
        evalExp = eval valMaps e
        evalNormalized = eval valMaps $ normalize e

-- |
--
instance Arbitrary SuiteTwoR where
    arbitrary = do
        (exp, vars) <- genTwoR
        valMaps <- genValMap vars
        return $ SuiteTwoR exp valMaps

-------------------------------------------------------------------------------
-- | MARK: Gen functions Two C
--
--
-------------------------------------------------------------------------------
primitiveTwoC :: Gen (Expression Two C, Vars)
primitiveTwoC = do
    name1 <- elements . map ((++ "1") . pure) $ ['a' .. 'z']
    name2 <- elements . map ((++ "1") . pure) $ ['a' .. 'z']
    dbl1 <- genDouble
    dbl2 <- genDouble
    elements . withRatio $
        [ ( 6
          , ( var2d (default1stDim2D, default2ndDim2D) name1 +:
              var2d (default1stDim2D, default2ndDim2D) name2
            , [[], [], [name1, name2], []]))
        , ( 4
          , ( const2d (default1stDim2D, default2ndDim2D) dbl1 +:
              const2d (default1stDim2D, default2ndDim2D) dbl2
            , [[], [], [], []]))
        ]

operandTwoC :: Gen (Expression Two C, Vars)
operandTwoC = oneof . withRatio $ [(9, primitiveTwoC), (1, genTwoC)]

genTwoC :: Gen (Expression Two C, Vars)
genTwoC =
    oneof . withRatio $
    [ (6, fromNaryTwoC sum)
    , (3, fromNaryTwoC product)
    , (6, fromBinaryTwoC (+))
    , (6, fromBinaryTwoC (-))
    , (3, fromBinaryTwoC (*))
    , (3, fromUnaryTwoC negate)
    , (1, fromUnaryTwoC (^ 2))
    , (2, fromScaleTwoC)
    , (2, fromRotateTwoC)
    , (1, fromFourierTransformTwoC)
    ]
  where
    fromFourierTransformTwoC :: Gen (Expression Two C, Vars)
    fromFourierTransformTwoC = do
        on <- operandTwoC
        return (ft . fst $ on, snd on)
    fromRotateTwoC :: Gen (Expression Two C, Vars)
    fromRotateTwoC = do
        on <- operandTwoC
        amount1 <- elements [-9 .. 9]
        amount2 <- elements [-9 .. 9]
        return (rotate (amount1, amount2) $ fst on, snd on)
    fromNaryTwoC ::
           ([Expression Two C] -> Expression Two C)
        -> Gen (Expression Two C, Vars)
    fromNaryTwoC f = do
        numOperands <- elements [3]
        ons <- vectorOf numOperands operandTwoC
        let exp = f . map fst $ ons
            vars = mergeVars . map snd $ ons
        return (exp, vars)
    fromUnaryTwoC ::
           (Expression Two C -> Expression Two C)
        -> Gen (Expression Two C, Vars)
    fromUnaryTwoC f = do
        on <- operandTwoC
        let exp = f . fst $ on
            names = snd on
        return (exp, names)
    fromBinaryTwoC ::
           (Expression Two C -> Expression Two C -> Expression Two C)
        -> Gen (Expression Two C, Vars)
    fromBinaryTwoC f = do
        on1 <- operandTwoC
        on2 <- operandTwoC
        let exp = f (fst on1) (fst on2)
            vars = mergeVars [snd on1, snd on2]
        return (exp, vars)
    fromScaleTwoC :: Gen (Expression Two C, Vars)
    fromScaleTwoC = do
        scalarR <- operandScalarR
        scalarC <- operandScalarC
        scalee <- operandTwoC
        rand <- elements [True, False]
        let (exp, vars) =
                if rand
                    then ( fst scalarR *. fst scalee
                         , mergeVars [snd scalarR, snd scalee])
                    else ( fst scalarC *. fst scalee
                         , mergeVars [snd scalarC, snd scalee])
        return (exp, vars)

instance Arbitrary (Expression Two C) where
    arbitrary = fmap fst genTwoC

-- |
--
data SuiteTwoC =
    SuiteTwoC (Expression Two C) ValMaps

instance Show SuiteTwoC where
    show (SuiteTwoC e valMaps) =
        format [("Expr", exp), ("ValMap", show valMaps)]
      where
        exp = prettifyDebug e
        normalizedExp = prettifyDebug . normalize $ e
        evalExp = eval valMaps e
        evalNormalized = eval valMaps $ normalize e

-- |
--
instance Arbitrary SuiteTwoC where
    arbitrary = do
        (exp, vars) <- genTwoC
        valMaps <- genValMap vars
        return $ SuiteTwoC exp valMaps

-------------------------------------------------------------------------------
-- | MARK: Arbitrary instance for all kinds of expressions
--
--
-------------------------------------------------------------------------------
data ArbitraryExpresion =
    forall d et. (DimensionType d, ElementType et) =>
                 ArbitraryExpresion (Expression d et)

instance Show ArbitraryExpresion where
    show (ArbitraryExpresion exp) = show exp

instance Arbitrary ArbitraryExpresion where
    arbitrary =
        let option1 =
                fmap ArbitraryExpresion (arbitrary :: Gen (Expression Scalar R))
            option2 =
                fmap ArbitraryExpresion (arbitrary :: Gen (Expression Scalar C))
            option3 =
                fmap ArbitraryExpresion (arbitrary :: Gen (Expression One R))
            option4 =
                fmap ArbitraryExpresion (arbitrary :: Gen (Expression One C))
            option5 =
                fmap ArbitraryExpresion (arbitrary :: Gen (Expression Two R))
            option6 =
                fmap ArbitraryExpresion (arbitrary :: Gen (Expression Two C))
         in oneof [option1, option2, option3, option4, option5, option6]

-- |
--
getWrappedExp :: ArbitraryExpresion -> (ExpressionMap, Int)
getWrappedExp (ArbitraryExpresion (Expression n mp)) = (mp, n)

-- |
--
sz :: Expression d et -> Int
sz = IM.size . exMap

infix 1 `shouldNormalizeTo`

shouldNormalizeTo ::
       (HasCallStack, DimensionType d, ElementType et, Typeable et, Typeable d)
    => Expression d et
    -> Expression d et
    -> IO ()
shouldNormalizeTo exp1 exp2 = do
    prettify (normalize exp1) `shouldBe` prettify (normalize exp2)
    normalize exp1 `shouldBe` normalize exp2
