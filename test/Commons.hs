{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Commons where

import Control.Applicative (liftA2)
import Control.Monad (forM)
import Data.Array
import Data.Complex
import Data.Function.HT (nest)
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
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
        vectorOf (length names2d) . vectorOf (vectorSize * vectorSize) $
        arbitrary
    let vm2 =
            Map.fromList .
            zip names2d .
            map (listArray ((0, 0), (vectorSize - 1, vectorSize - 1))) $
            list2d
    list3d <-
        vectorOf (length names3d) .
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

withRatio :: [(Int, a)] -> [a]
withRatio = concatMap (uncurry replicate)

-------------------------------------------------------------------------------
-- | MARK: Gen functions Zero R
--
--
-------------------------------------------------------------------------------
primitiveZeroR :: Gen (Expression Zero R, Vars)
primitiveZeroR = do
    name <- elements . map pure $ ['a' .. 'z']
    dbl <- arbitrary
    elements . withRatio $
        [ (6, (var name, [[name], [], [], []]))
        , (4, (const dbl, [[], [], [], []]))
        ]

operandZeroR :: Gen (Expression Zero R, Vars)
operandZeroR = oneof . withRatio $ [(8, primitiveZeroR), (2, genZeroR)]

genZeroR :: Gen (Expression Zero R, Vars)
genZeroR =
    oneof . withRatio $
    [ (4, fromNaryZeroR sum)
    , (4, fromNaryZeroR product)
    , (4, fromBinaryZeroR (*.))
    , (4, fromBinaryZeroR (+))
    , (4, fromBinaryZeroR (*))
    , (4, fromBinaryZeroR (-))
    , (4, fromBinaryZeroR (<.>))
    , (2, fromUnaryZeroR negate)
    , (1, fromUnaryZeroR (^ 2))
    , (1, fromInnerProdHigherZeroR)
    , (2, fromZeroCZeroR)
    ]
    --    replicate 8 primitiveZeroR ++ replicate 2 genZeroR
  where
    fromNaryZeroR ::
           ([Expression Zero R] -> Expression Zero R)
        -> Gen (Expression Zero R, Vars)
    fromNaryZeroR f = do
        numOperands <- elements [3 .. 4]
        ons <- vectorOf numOperands operandZeroR
        let exp = f . map fst $ ons
            vars = mergeVars . map snd $ ons
        return (exp, vars)
    fromInnerProdHigherZeroR :: Gen (Expression Zero R, Vars)
    fromInnerProdHigherZeroR = do
        operand1 <- genOneR
        operand2 <- genOneR
        let exp = fst operand1 <.> fst operand2
            vars = mergeVars [snd operand1, snd operand2]
        return (exp, vars)
    fromUnaryZeroR ::
           (Expression Zero R -> Expression Zero R)
        -> Gen (Expression Zero R, Vars)
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
    fromZeroCZeroR :: Gen (Expression Zero R, Vars)
    fromZeroCZeroR = do
        rand <- elements [True, False]
        (zeroC, vars) <- genZeroC
        let exp =
                if rand
                    then xRe zeroC
                    else xIm zeroC
        return (exp, vars)

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
        exp = prettifyDebug e
        simplifiedExp = prettifyDebug . simplify $ e
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
    elements . withRatio $
        [ (1, (var name1 +: var name2, [[name1, name2], [], [], []]))
        , (1, (const dbl +: const 0, [[], [], [], []]))
        ]

operandZeroC :: Gen (Expression Zero C, Vars)
operandZeroC = oneof . withRatio $ [(9, primitiveZeroC), (1, genZeroC)]

genZeroC :: Gen (Expression Zero C, Vars)
genZeroC =
    oneof . withRatio $
    [ (6, fromNaryZeroC sum)
    , (3, fromNaryZeroC product)
    , (6, fromBinaryZeroC (*.))
    , (6, fromBinaryZeroC (+))
    , (3, fromBinaryZeroC (*))
    , (6, fromBinaryZeroC (-))
    , (3, fromBinaryZeroC (<.>))
    , (3, fromUnaryZeroC negate)
    , (1, fromUnaryZeroC (^ 2))
    , (1, fromInnerProdHigherZeroC)
    , (2, fromRealImagZeroC)
    ]
  where
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
           (Expression Zero C -> Expression Zero C)
        -> Gen (Expression Zero C, Vars)
    fromUnaryZeroC f = do
        on <- operandZeroC
        let exp = f . fst $ on
            vars = snd on
        return (exp, vars)
    fromInnerProdHigherZeroC :: Gen (Expression Zero C, Vars)
    fromInnerProdHigherZeroC = do
        operand1 <- genOneC
        operand2 <- genOneC
        let exp = fst operand1 <.> fst operand2
            vars = mergeVars [snd operand1, snd operand2]
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
        exp = prettifyDebug e
        simplifiedExp = prettifyDebug . simplify $ e
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
    name <- elements . map ((++ "1") . pure) $ ['a' .. 'z']
    dbl <- arbitrary
    elements . withRatio $
        [ (6, (var1d vectorSize name, [[], [name], [], []]))
        , (4, (const1d vectorSize dbl, [[], [], [], []]))
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
    , (2, fromRotateOneR)
    ]
  where
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
        scalar <- operandZeroR
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
        format
            [ ("Expr", exp)
            , ("Simplified", simplifiedExp)
            , ("ValMap", show valMaps)
            , ("EvalExp", show evalExp)
            , ("EvalExpSimplify", show evalSimplified)
            ]
      where
        exp = prettifyDebug e
        simplifiedExp = prettifyDebug . simplify $ e
        evalExp = eval valMaps e
        evalSimplified = eval valMaps $ simplify e

-- |
--
instance Arbitrary SuiteOneR where
    arbitrary = do
        (exp, vars) <- genOneR
        valMaps <- genValMaps vars
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
    dbl1 <- arbitrary
    dbl2 <- arbitrary
    elements . withRatio $
        [ ( 6
          , ( var1d vectorSize name1 +: var1d vectorSize name2
            , [[], [name1, name2], [], []]))
        , ( 4
          , ( const1d vectorSize dbl1 +: const1d vectorSize dbl2
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
    ]
  where
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
        scalarR <- operandZeroR
        scalarC <- operandZeroC
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
        format
            [ ("Expr", exp)
            , ("Simplified", simplifiedExp)
            , ("ValMap", show valMaps)
            , ("EvalExp", show evalExp)
            , ("EvalExpSimplify", show evalSimplified)
            ]
      where
        exp = prettifyDebug e
        simplifiedExp = prettifyDebug . simplify $ e
        evalExp = eval valMaps e
        evalSimplified = eval valMaps $ simplify e

-- |
--
instance Arbitrary SuiteOneC where
    arbitrary = do
        (exp, vars) <- genOneC
        valMaps <- genValMaps vars
        return $ SuiteOneC exp valMaps

-------------------------------------------------------------------------------
-- | MARK: Gen functions Two R
--
--
-------------------------------------------------------------------------------
primitiveTwoR :: Gen (Expression Two R, Vars)
primitiveTwoR = do
    name <- elements . map ((++ "2") . pure) $ ['a' .. 'z']
    dbl <- arbitrary
    elements . withRatio $
        [ (6, (var2d (vectorSize, vectorSize) name, [[], [], [name], []]))
        , (4, (const2d (vectorSize, vectorSize) dbl, [[], [], [], []]))
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
    ]
  where
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
        scalar <- operandZeroR
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
        format
            [ ("Expr", exp)
            , ("Simplified", simplifiedExp)
            , ("ValMap", show valMaps)
            , ("EvalExp", show evalExp)
            , ("EvalExpSimplify", show evalSimplified)
            ]
      where
        exp = prettifyDebug e
        simplifiedExp = prettifyDebug . simplify $ e
        evalExp = eval valMaps e
        evalSimplified = eval valMaps $ simplify e

-- |
--
instance Arbitrary SuiteTwoR where
    arbitrary = do
        (exp, vars) <- genTwoR
        valMaps <- genValMaps vars
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
    dbl1 <- arbitrary
    dbl2 <- arbitrary
    elements . withRatio $
        [ ( 6
          , ( var2d (vectorSize, vectorSize) name1 +:
              var2d (vectorSize, vectorSize) name2
            , [[], [], [name1, name2], []]))
        , ( 4
          , ( const2d (vectorSize, vectorSize) dbl1 +:
              const2d (vectorSize, vectorSize) dbl2
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
    ]
  where
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
        scalarR <- operandZeroR
        scalarC <- operandZeroC
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
        format
            [ ("Expr", exp)
            , ("Simplified", simplifiedExp)
            , ("ValMap", show valMaps)
            , ("EvalExp", show evalExp)
            , ("EvalExpSimplify", show evalSimplified)
            ]
      where
        exp = prettifyDebug e
        simplifiedExp = prettifyDebug . simplify $ e
        evalExp = eval valMaps e
        evalSimplified = eval valMaps $ simplify e

-- |
--
instance Arbitrary SuiteTwoC where
    arbitrary = do
        (exp, vars) <- genTwoC
        valMaps <- genValMaps vars
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
                fmap ArbitraryExpresion (arbitrary :: Gen (Expression Zero R))
            option2 =
                fmap ArbitraryExpresion (arbitrary :: Gen (Expression Zero C))
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

infix 1 `shouldSimplifyTo`

shouldSimplifyTo ::
       (HasCallStack, DimensionType d, ElementType et, Typeable et, Typeable d)
    => Expression d et
    -> Expression d et
    -> IO ()
shouldSimplifyTo exp1 exp2 = do
    prettify (simplify exp1) `shouldBe` prettify (simplify exp2)
    simplify exp1 `shouldBe` simplify exp2
