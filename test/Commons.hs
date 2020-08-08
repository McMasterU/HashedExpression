{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
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
import GHC.TypeLits (KnownNat, Nat)
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Utils
import HashedExpression.Interp
import HashedExpression.Operation
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import HashedExpression.Value
import Test.HUnit
import Test.Hspec
import Test.QuickCheck
import Var
import Prelude hiding ((^))

sizeReduceFactor :: Int
sizeReduceFactor = 3

-- |
--
-- | Remove duplicate but also sort
removeDuplicate :: (Ord a) => [a] -> [a]
removeDuplicate = toList . fromList

-- |
vectorOfDifferent :: Eq a => Int -> Gen a -> Gen [a]
vectorOfDifferent sz gen = foldM f [] [1 .. sz]
  where
    f acc _ = (: acc) <$> gen `suchThat` (not . flip elem acc)

-- | Format
format :: [(String, String)] -> String
format = intercalate "\n" . map oneLine
  where
    oneLine (f, s) = f ++ ": " ++ s

-- |
inspect :: (Typeable d, Typeable rc) => Expression d rc -> Expression d rc
inspect x =
  unsafePerformIO $ do
    showExp x
    return x

-- | VarsAndParams list
type VarsAndParams = [[String]] -- VarsAndParams 0D, 1D, 2D, 3D, ..

mergeVarsAndParams :: [VarsAndParams] -> VarsAndParams
mergeVarsAndParams = foldl f [[], [], [], []]
  where
    f x y = map removeDuplicate $ zipWith (++) x y

genDouble :: Gen Double
genDouble = arbitrary `suchThat` inSmallRange
  where
    inSmallRange x = x >= 0 && x <= 10

-- |
genValMap ::
  forall size1D size2D1 size2D2.
  (KnownNat size1D, KnownNat size2D1, KnownNat size2D2) =>
  VarsAndParams ->
  Gen ValMaps
genValMap vars = do
  let sz1D = nat @size1D
      sz2D1 = nat @size2D1
      sz2D2 = nat @size2D2
  let [names0d, names1d, names2d, names3d] = vars
  list0d <- vectorOf (length names0d) genDouble
  let vm0 = Map.fromList . zip names0d $ map VScalar list0d
  list1d <- vectorOf (length names1d) . vectorOf sz1D $ genDouble
  let vm1 =
        Map.fromList . zip names1d . map (V1D . listArray (0, sz1D - 1)) $
          list1d
  list2d <- vectorOf (length names2d) . vectorOf (sz2D1 * sz2D2) $ genDouble
  let vm2 =
        Map.fromList
          . zip names2d
          . map (V2D . listArray ((0, 0), (sz2D1 - 1, sz2D2 - 1)))
          $ list2d
  let vm3 = Map.empty -- TODO: not testing with 3D yet.
  return $ Map.unions [vm0, vm1, vm2, vm3]

shouldApprox :: (HasCallStack, Approximable a) => a -> a -> Expectation
shouldApprox x y = assertBool msg (x ~= y)
  where
    msg = "Expected: " ++ prettifyShow y ++ "\nGot: " ++ prettifyShow x

infix 1 `shouldApprox`

liftE1 ::
  (Expression d1 et1 -> Expression d2 et2) ->
  (Expression d1 et1, VarsAndParams) ->
  (Expression d2 et2, VarsAndParams)
liftE1 op (e, v) = (op e, v)

liftE2 ::
  (Expression d1 et1 -> Expression d2 et2 -> Expression d3 et3) ->
  (Expression d1 et1, VarsAndParams) ->
  (Expression d2 et2, VarsAndParams) ->
  (Expression d3 et3, VarsAndParams)
liftE2 op (e1, v1) (e2, v2) = (op e1 e2, mergeVarsAndParams [v1, v2])

-------------------------------------------------------------------------------
primitiveScalarR :: Gen (Expression Scalar R, VarsAndParams)
primitiveScalarR = do
  varName <- elements . map pure $ ['a' .. 'z']
  paramName <- elements . map (: "p") $ ['a' .. 'z']
  dbl <- genDouble
  elements
    [ (variable varName, [[varName], [], [], []]),
      (param paramName, [[paramName], [], [], []]),
      (constant dbl, [[], [], [], []])
    ]

primitiveScalarC :: Gen (Expression Scalar C, VarsAndParams)
primitiveScalarC = liftE2 (+:) <$> primitiveScalarR <*> primitiveScalarR

-------------------------------------------------------------------------------
primitive1DR ::
  forall n.
  KnownNat n =>
  Gen (Expression n R, VarsAndParams)
primitive1DR = do
  varName <- elements . map (: "1") $ ['a' .. 'z']
  paramName <- elements . map (: "p1") $ ['a' .. 'z']
  dbl <- genDouble
  elements
    [ (variable1D @n varName, [[], [varName], [], []]),
      (param1D @n paramName, [[], [paramName], [], []]),
      (constant1D @n dbl, [[], [], [], []])
    ]

primitive1DC ::
  forall n.
  KnownNat n =>
  Gen (Expression n C, VarsAndParams)
primitive1DC = liftE2 (+:) <$> primitive1DR @n <*> primitive1DR @n

-------------------------------------------------------------------------------
primitive2DR ::
  forall m n.
  (KnownNat m, KnownNat n) =>
  Gen (Expression '(m, n) R, VarsAndParams)
primitive2DR = do
  varName <- elements . map (: "2") $ ['a' .. 'z']
  paramName <- elements . map (: "p2") $ ['a' .. 'z']
  dbl <- genDouble
  elements
    [ (variable2D @m @n varName, [[], [], [varName], []]),
      (param2D @m @n paramName, [[], [], [paramName], []]),
      (constant2D @m @n dbl, [[], [], [], []])
    ]

primitive2DC ::
  forall m n.
  (KnownNat m, KnownNat n) =>
  Gen (Expression '(m, n) C, VarsAndParams)
primitive2DC = liftE2 (+:) <$> primitive2DR @m @n <*> primitive2DR @m @n

-------------------------------------------------------------------------------
genScalarR ::
  forall default1D default2D1 default2D2.
  (KnownNat default1D, KnownNat default2D1, KnownNat default2D2) =>
  Int ->
  Gen (Expression Scalar R, VarsAndParams)
genScalarR size
  | size == 0 = primitiveScalarR
  | otherwise =
    let sub = genScalarR @default1D @default2D1 @default2D2 (size `div` sizeReduceFactor)
        subC = genScalarC @default1D @default2D1 @default2D2 (size `div` sizeReduceFactor)
        sub1D = gen1DR @default1D @default2D1 @default2D2 (size `div` sizeReduceFactor)
        sub2D = gen2DR @default1D @default2D1 @default2D2 (size `div` sizeReduceFactor)
        fromPiecewise = do
          numBranches <- elements [2, 3]
          branches <- vectorOf numBranches sub
          condition <- sub
          marks <- sort <$> vectorOfDifferent (numBranches - 1) arbitrary
          let vars = mergeVarsAndParams $ map snd branches ++ [snd condition]
              exp = piecewise marks (fst condition) $ map fst branches
          return (exp, vars)
        binary op = liftE2 op <$> sub <*> sub
        unary op = liftE1 op <$> sub
     in oneof
          [ fromPiecewise,
            binary (+),
            binary (*),
            binary (*.),
            binary (-),
            binary (<.>),
            unary negate,
            unary (^ 2),
            liftE1 xRe <$> subC,
            liftE1 xIm <$> subC,
            liftE2 (<.>) <$> sub1D <*> sub1D,
            liftE2 (<.>) <$> sub2D <*> sub2D
          ]

-------------------------------------------------------------------------------
genScalarC ::
  forall default1D default2D1 default2D2.
  (KnownNat default1D, KnownNat default2D1, KnownNat default2D2) =>
  Int ->
  Gen (Expression Scalar C, VarsAndParams)
genScalarC size
  | size == 0 = primitiveScalarC
  | otherwise =
    let sub = genScalarC @default1D @default2D1 @default2D2 (size `div` sizeReduceFactor)
        subR = genScalarR @default1D @default2D1 @default2D2 (size `div` sizeReduceFactor)
        sub1D = gen1DC @default1D @default2D1 @default2D2 (size `div` sizeReduceFactor)
        sub2D = gen2DC @default1D @default2D1 @default2D2 (size `div` sizeReduceFactor)
        fromPiecewise = do
          numBranches <- elements [2, 3]
          branches <- vectorOf numBranches sub
          condition <- subR
          marks <- sort <$> vectorOfDifferent (numBranches - 1) arbitrary
          let vars = mergeVarsAndParams $ map snd branches ++ [snd condition]
              exp = piecewise marks (fst condition) $ map fst branches
          return (exp, vars)
        binary op = liftE2 op <$> sub <*> sub
        unary op = liftE1 op <$> sub
     in oneof
          [ fromPiecewise,
            binary (+),
            binary (*),
            binary (*.),
            binary (-),
            binary (<.>),
            unary negate,
            unary (^ 2),
            liftE2 (+:) <$> subR <*> subR,
            liftE2 (<.>) <$> sub1D <*> sub1D,
            liftE2 (<.>) <$> sub2D <*> sub2D
          ]

-------------------------------------------------------------------------------
gen1DR ::
  forall n default2D1 default2D2.
  (KnownNat n, KnownNat default2D1, KnownNat default2D2) =>
  Int ->
  Gen (Expression n R, VarsAndParams)
gen1DR size
  | size == 0 = primitive1DR
  | otherwise =
    let sub = gen1DR @n @default2D1 @default2D2 (size `div` sizeReduceFactor)
        subC = gen1DC @n @default2D1 @default2D2 (size `div` sizeReduceFactor)
        subScalar = genScalarR @n @default2D1 @default2D2 (size `div` sizeReduceFactor)
        fromPiecewise = do
          numBranches <- elements [2, 3]
          branches <- vectorOf numBranches sub
          condition <- sub
          marks <- sort <$> vectorOfDifferent (numBranches - 1) arbitrary
          let vars = mergeVarsAndParams $ map snd branches ++ [snd condition]
              exp = piecewise marks (fst condition) $ map fst branches
          return (exp, vars)
        fromRotate = do
          amount <- elements [- (nat @n) .. nat @n]
          liftE1 (rotate amount) <$> sub
        binary op = liftE2 op <$> sub <*> sub
        unary op = liftE1 op <$> sub
     in oneof
          [ fromPiecewise,
            binary (+),
            binary (*),
            binary (-),
            unary negate,
            unary (^ 2),
            liftE1 xRe <$> subC,
            liftE1 xIm <$> subC,
            liftE2 (*.) <$> subScalar <*> sub,
            fromRotate
          ]

-------------------------------------------------------------------------------
gen1DC ::
  forall n default2D1 default2D2.
  (KnownNat n, KnownNat default2D1, KnownNat default2D2) =>
  Int ->
  Gen (Expression n C, VarsAndParams)
gen1DC size
  | size == 0 = primitive1DC
  | otherwise =
    let sub = gen1DC @n @default2D1 @default2D2 (size `div` sizeReduceFactor)
        subR = gen1DR @n @default2D1 @default2D2 (size `div` sizeReduceFactor)
        subScalar = genScalarC @n @default2D1 @default2D2 (size `div` sizeReduceFactor)
        fromPiecewise = do
          numBranches <- elements [2, 3]
          branches <- vectorOf numBranches sub
          condition <- subR
          marks <- sort <$> vectorOfDifferent (numBranches - 1) arbitrary
          let vars = mergeVarsAndParams $ map snd branches ++ [snd condition]
              exp = piecewise marks (fst condition) $ map fst branches
          return (exp, vars)
        fromRotate = do
          amount <- elements [- (nat @n) .. nat @n]
          liftE1 (rotate amount) <$> sub
        binary op = liftE2 op <$> sub <*> sub
        unary op = liftE1 op <$> sub
     in oneof
          [ fromPiecewise,
            binary (+),
            binary (*),
            binary (-),
            unary negate,
            unary (^ 2),
            liftE2 (+:) <$> subR <*> subR,
            liftE2 (*.) <$> subScalar <*> sub,
            unary ft,
            unary ift,
            fromRotate
          ]

-------------------------------------------------------------------------------
gen2DR ::
  forall default1D m n.
  (KnownNat default1D, KnownNat m, KnownNat n) =>
  Int ->
  Gen (Expression '(m, n) R, VarsAndParams)
gen2DR size
  | size == 0 = primitive2DR
  | otherwise =
    let sub = gen2DR @default1D @m @n (size `div` sizeReduceFactor)
        subC = gen2DC @default1D @m @n (size `div` sizeReduceFactor)
        subScalar = genScalarR @default1D @m @n (size `div` sizeReduceFactor)
        fromPiecewise = do
          numBranches <- elements [2, 3]
          branches <- vectorOf numBranches sub
          condition <- sub
          marks <- sort <$> vectorOfDifferent (numBranches - 1) arbitrary
          let vars = mergeVarsAndParams $ map snd branches ++ [snd condition]
              exp = piecewise marks (fst condition) $ map fst branches
          return (exp, vars)
        fromRotate = do
          amount1 <- elements [- (nat @m) .. nat @m]
          amount2 <- elements [- (nat @n) .. nat @n]
          liftE1 (rotate (amount1, amount2)) <$> sub
        binary op = liftE2 op <$> sub <*> sub
        unary op = liftE1 op <$> sub
     in oneof
          [ fromPiecewise,
            binary (+),
            binary (*),
            binary (-),
            unary negate,
            unary (^ 2),
            liftE1 xRe <$> subC,
            liftE1 xIm <$> subC,
            liftE2 (*.) <$> subScalar <*> sub,
            fromRotate
          ]

-------------------------------------------------------------------------------
gen2DC ::
  forall default1D m n.
  (KnownNat default1D, KnownNat m, KnownNat n) =>
  Int ->
  Gen (Expression '(m, n) C, VarsAndParams)
gen2DC size
  | size == 0 = primitive2DC
  | otherwise =
    let sub = gen2DC @default1D @m @n (size `div` sizeReduceFactor)
        subR = gen2DR @default1D @m @n (size `div` sizeReduceFactor)
        subScalar = genScalarC @default1D @m @n (size `div` sizeReduceFactor)
        fromPiecewise = do
          numBranches <- elements [2, 3]
          branches <- vectorOf numBranches sub
          condition <- subR
          marks <- sort <$> vectorOfDifferent (numBranches - 1) arbitrary
          let vars = mergeVarsAndParams $ map snd branches ++ [snd condition]
              exp = piecewise marks (fst condition) $ map fst branches
          return (exp, vars)
        fromRotate = do
          amount1 <- elements [- (nat @m) .. nat @m]
          amount2 <- elements [- (nat @n) .. nat @n]
          liftE1 (rotate (amount1, amount2)) <$> sub
        binary op = liftE2 op <$> sub <*> sub
        unary op = liftE1 op <$> sub
     in oneof
          [ fromPiecewise,
            binary (+),
            binary (*),
            binary (-),
            unary negate,
            unary (^ 2),
            liftE2 (+:) <$> subR <*> subR,
            liftE2 (*.) <$> subScalar <*> sub,
            unary ft,
            unary ift,
            fromRotate
          ]

-------------------------------------------------------------------------------
data Suite (size1D :: Nat) (size2D1 :: Nat) (size2D2 :: Nat) d et
  = Suite (Expression d et) ValMaps
  deriving (Show)

-------------------------------------------------------------------------------
type TestSuite = Suite Default1D Default2D1 Default2D2

type SuiteScalarR = TestSuite Scalar R

type SuiteScalarC = TestSuite Scalar C

type SuiteOneR = TestSuite Default1D R

type SuiteOneC = TestSuite Default1D C

type SuiteTwoR = TestSuite '(Default2D1, Default2D2) R

type SuiteTwoC = TestSuite '(Default2D1, Default2D2) C

-------------------------------------------------------------------------------
instance
  (KnownNat size1D, KnownNat size2D1, KnownNat size2D2) =>
  Arbitrary (Suite size1D size2D1 size2D2 Scalar R)
  where
  arbitrary = do
    (exp, vars) <- sized $ genScalarR @size1D @size2D1 @size2D2
    valMaps <- genValMap @size1D @size2D1 @size2D2 vars
    return $ Suite exp valMaps

instance
  (KnownNat size1D, KnownNat size2D1, KnownNat size2D2) =>
  Arbitrary (Suite size1D size2D1 size2D2 Scalar C)
  where
  arbitrary = do
    (exp, vars) <- sized $ genScalarC @size1D @size2D1 @size2D2
    valMaps <- genValMap @size1D @size2D1 @size2D2 vars
    return $ Suite exp valMaps

instance
  (KnownNat size1D, KnownNat size2D1, KnownNat size2D2) =>
  Arbitrary (Suite size1D size2D1 size2D2 size1D R)
  where
  arbitrary = do
    (exp, vars) <- sized $ gen1DR @size1D @size2D1 @size2D2
    valMaps <- genValMap @size1D @size2D1 @size2D2 vars
    return $ Suite exp valMaps

instance
  (KnownNat size1D, KnownNat size2D1, KnownNat size2D2) =>
  Arbitrary (Suite size1D size2D1 size2D2 size1D C)
  where
  arbitrary = do
    (exp, vars) <- sized $ gen1DC @size1D @size2D1 @size2D2
    valMaps <- genValMap @size1D @size2D1 @size2D2 vars
    return $ Suite exp valMaps

instance
  (KnownNat size1D, KnownNat size2D1, KnownNat size2D2) =>
  Arbitrary (Suite size1D size2D1 size2D2 '(size2D1, size2D2) R)
  where
  arbitrary = do
    (exp, vars) <- sized $ gen2DR @size1D @size2D1 @size2D2
    valMaps <- genValMap @size1D @size2D1 @size2D2 vars
    return $ Suite exp valMaps

instance
  (KnownNat size1D, KnownNat size2D1, KnownNat size2D2) =>
  Arbitrary (Suite size1D size2D1 size2D2 '(size2D1, size2D2) C)
  where
  arbitrary = do
    (exp, vars) <- sized $ gen2DC @size1D @size2D1 @size2D2
    valMaps <- genValMap @size1D @size2D1 @size2D2 vars
    return $ Suite exp valMaps

-------------------------------------------------------------------------------
instance Arbitrary (Expression Scalar R) where
  arbitrary = fst <$> sized (genScalarR @Default1D @Default2D1 @Default2D2)

instance Arbitrary (Expression Scalar C) where
  arbitrary = fst <$> sized (genScalarC @Default1D @Default2D1 @Default2D2)

instance KnownNat n => Arbitrary (Expression n R) where
  arbitrary = fst <$> sized (gen1DR @n @Default2D1 @Default2D2)

instance KnownNat n => Arbitrary (Expression n C) where
  arbitrary = fst <$> sized (gen1DC @n @Default2D1 @Default2D2)

instance (KnownNat m, KnownNat n) => Arbitrary (Expression '(m, n) R) where
  arbitrary = fst <$> sized (gen2DR @Default1D @m @n)

instance (KnownNat m, KnownNat n) => Arbitrary (Expression '(m, n) C) where
  arbitrary = fst <$> sized (gen2DC @Default1D @m @n)

-------------------------------------------------------------------------------
data ArbitraryExpresion = forall d et. (Dimension d, ElementType et, Typeable et, Typeable d) => ArbitraryExpresion (Expression d et)

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
            (arbitrary :: Gen (Expression Default1D R))
        option4 =
          fmap
            ArbitraryExpresion
            (arbitrary :: Gen (Expression Default1D C))
        option5 =
          fmap
            ArbitraryExpresion
            (arbitrary :: Gen (Expression '(Default2D1, Default2D2) R))
        option6 =
          fmap
            ArbitraryExpresion
            (arbitrary :: Gen (Expression '(Default2D1, Default2D2) C))
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
