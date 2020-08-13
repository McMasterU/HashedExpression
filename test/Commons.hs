{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Commons where

import Control.Applicative (liftA2)
import Control.Monad (foldM, forM)
import Data.Array hiding (range)
import Data.Complex (Complex (..))
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
genValMap :: VarsAndParams -> Gen ValMaps
genValMap vars = do
  let sz1D = nat @Default1D
      sz2D1 = nat @Default2D1
      sz2D2 = nat @Default2D2
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

-- |
unsafeProject :: [DimSelector] -> Expression d1 et1 -> Expression d2 et2
unsafeProject ds e = wrap $ apply (Unary (specProject ds)) [unwrap e]

unsafeInject :: [DimSelector] -> Expression d1 et1 -> Expression d2 et2 -> Expression d2 et2
unsafeInject ds sub base = wrap $ apply (Binary (specInject ds)) [unwrap sub, unwrap base]

genDimSelector :: Int -> Gen DimSelector
genDimSelector size = do
  let id = elements [0 .. size - 1]
  let step = elements [1 .. size]
  oneof [Range <$> id <*> id <*> step, At <$> id]

genAtSelector :: Int -> Gen DimSelector
genAtSelector size = do
  let id = elements [0 .. size - 1]
  oneof [At <$> id]

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
primitive1DR :: Gen (Expression Default1D R, VarsAndParams)
primitive1DR = do
  varName <- elements . map (: "1") $ ['a' .. 'z']
  paramName <- elements . map (: "p1") $ ['a' .. 'z']
  dbl <- genDouble
  elements
    [ (variable1D @Default1D varName, [[], [varName], [], []]),
      (param1D @Default1D paramName, [[], [paramName], [], []]),
      (constant1D @Default1D dbl, [[], [], [], []])
    ]

primitive1DC :: Gen (Expression Default1D C, VarsAndParams)
primitive1DC = liftE2 (+:) <$> primitive1DR <*> primitive1DR

-------------------------------------------------------------------------------
primitive2DR :: Gen (Expression '(Default2D1, Default2D2) R, VarsAndParams)
primitive2DR = do
  varName <- elements . map (: "2") $ ['a' .. 'z']
  paramName <- elements . map (: "p2") $ ['a' .. 'z']
  dbl <- genDouble
  elements
    [ (variable2D @Default2D1 @Default2D2 varName, [[], [], [varName], []]),
      (param2D @Default2D1 @Default2D2 paramName, [[], [], [paramName], []]),
      (constant2D @Default2D1 @Default2D2 dbl, [[], [], [], []])
    ]

primitive2DC :: Gen (Expression '(Default2D1, Default2D2) C, VarsAndParams)
primitive2DC = liftE2 (+:) <$> primitive2DR <*> primitive2DR

-------------------------------------------------------------------------------
genScalarR :: Int -> Gen (Expression Scalar R, VarsAndParams)
genScalarR size
  | size == 0 = primitiveScalarR
  | otherwise =
    let sub = genScalarR (size `div` sizeReduceFactor)
        subC = genScalarC (size `div` sizeReduceFactor)
        sub1D = gen1DR (size `div` sizeReduceFactor)
        sub2D = gen2DR (size `div` sizeReduceFactor)
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
        fromProjection = do
          (exp, vars) <- sub1D
          ds <- genAtSelector defaultDim1D
          return (unsafeProject [ds] exp, vars)
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
            liftE2 (<.>) <$> sub2D <*> sub2D,
            fromProjection
          ]

-------------------------------------------------------------------------------
genScalarC :: Int -> Gen (Expression Scalar C, VarsAndParams)
genScalarC size
  | size == 0 = primitiveScalarC
  | otherwise =
    let sub = genScalarC (size `div` sizeReduceFactor)
        subR = genScalarR (size `div` sizeReduceFactor)
        sub1D = gen1DC (size `div` sizeReduceFactor)
        sub2D = gen2DC (size `div` sizeReduceFactor)
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
        fromProjection = do
          (exp, vars) <- sub1D
          ds <- genAtSelector defaultDim1D
          return (unsafeProject [ds] exp, vars)
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
            liftE2 (<.>) <$> sub2D <*> sub2D,
            fromProjection
          ]

-------------------------------------------------------------------------------
gen1DR :: Int -> Gen (Expression Default1D R, VarsAndParams)
gen1DR size
  | size == 0 = primitive1DR
  | otherwise =
    let sub = gen1DR (size `div` sizeReduceFactor)
        subC = gen1DC (size `div` sizeReduceFactor)
        subScalar = genScalarR (size `div` sizeReduceFactor)
        fromPiecewise = do
          numBranches <- elements [2, 3]
          branches <- vectorOf numBranches sub
          condition <- sub
          marks <- sort <$> vectorOfDifferent (numBranches - 1) arbitrary
          let vars = mergeVarsAndParams $ map snd branches ++ [snd condition]
              exp = piecewise marks (fst condition) $ map fst branches
          return (exp, vars)
        fromRotate = do
          amount <- elements [- (nat @Default1D) .. nat @Default1D]
          liftE1 (rotate amount) <$> sub
        binary op = liftE2 op <$> sub <*> sub
        unary op = liftE1 op <$> sub
        fromProjectInject = do
          (exp1, vars1) <- sub
          (exp2, vars2) <- sub
          ds <- genDimSelector defaultDim1D
          let vars = mergeVarsAndParams [vars1, vars2]
          return (unsafeInject [ds] (unsafeProject [ds] exp1) exp2, vars)
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
            fromRotate,
            fromProjectInject
          ]

-------------------------------------------------------------------------------
gen1DC :: Int -> Gen (Expression Default1D C, VarsAndParams)
gen1DC size
  | size == 0 = primitive1DC
  | otherwise =
    let sub = gen1DC (size `div` sizeReduceFactor)
        subR = gen1DR (size `div` sizeReduceFactor)
        subScalar = genScalarC (size `div` sizeReduceFactor)
        fromPiecewise = do
          numBranches <- elements [2, 3]
          branches <- vectorOf numBranches sub
          condition <- subR
          marks <- sort <$> vectorOfDifferent (numBranches - 1) arbitrary
          let vars = mergeVarsAndParams $ map snd branches ++ [snd condition]
              exp = piecewise marks (fst condition) $ map fst branches
          return (exp, vars)
        fromRotate = do
          amount <- elements [- (nat @Default1D) .. nat @Default1D]
          liftE1 (rotate amount) <$> sub
        binary op = liftE2 op <$> sub <*> sub
        unary op = liftE1 op <$> sub
        fromProjectInject = do
          (exp1, vars1) <- sub
          (exp2, vars2) <- sub
          ds <- genDimSelector defaultDim1D
          let vars = mergeVarsAndParams [vars1, vars2]
          return (unsafeInject [ds] (unsafeProject [ds] exp1) exp2, vars)
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
            fromRotate,
            fromProjectInject
          ]

-------------------------------------------------------------------------------
gen2DR :: Int -> Gen (Expression '(Default2D1, Default2D2) R, VarsAndParams)
gen2DR size
  | size == 0 = primitive2DR
  | otherwise =
    let sub = gen2DR (size `div` sizeReduceFactor)
        subC = gen2DC (size `div` sizeReduceFactor)
        subScalar = genScalarR (size `div` sizeReduceFactor)
        fromPiecewise = do
          numBranches <- elements [2, 3]
          branches <- vectorOf numBranches sub
          condition <- sub
          marks <- sort <$> vectorOfDifferent (numBranches - 1) arbitrary
          let vars = mergeVarsAndParams $ map snd branches ++ [snd condition]
              exp = piecewise marks (fst condition) $ map fst branches
          return (exp, vars)
        fromRotate = do
          amount1 <- elements [- (nat @Default2D1) .. nat @Default2D1]
          amount2 <- elements [- (nat @Default2D2) .. nat @Default2D2]
          liftE1 (rotate (amount1, amount2)) <$> sub
        binary op = liftE2 op <$> sub <*> sub
        unary op = liftE1 op <$> sub
        fromProjectInject = do
          (exp1, vars1) <- sub
          (exp2, vars2) <- sub
          ds1 <- genDimSelector default1stDim2D
          ds2 <- genDimSelector default2ndDim2D
          let vars = mergeVarsAndParams [vars1, vars2]
          return (unsafeInject [ds1, ds2] (unsafeProject [ds1, ds2] exp1) exp2, vars)
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
            fromRotate,
            fromProjectInject
          ]

-------------------------------------------------------------------------------
gen2DC :: Int -> Gen (Expression '(Default2D1, Default2D2) C, VarsAndParams)
gen2DC size
  | size == 0 = primitive2DC
  | otherwise =
    let sub = gen2DC (size `div` sizeReduceFactor)
        subR = gen2DR (size `div` sizeReduceFactor)
        subScalar = genScalarC (size `div` sizeReduceFactor)
        fromPiecewise = do
          numBranches <- elements [2, 3]
          branches <- vectorOf numBranches sub
          condition <- subR
          marks <- sort <$> vectorOfDifferent (numBranches - 1) arbitrary
          let vars = mergeVarsAndParams $ map snd branches ++ [snd condition]
              exp = piecewise marks (fst condition) $ map fst branches
          return (exp, vars)
        fromRotate = do
          amount1 <- elements [- (nat @Default2D1) .. nat @Default2D1]
          amount2 <- elements [- (nat @Default2D2) .. nat @Default2D2]
          liftE1 (rotate (amount1, amount2)) <$> sub
        binary op = liftE2 op <$> sub <*> sub
        unary op = liftE1 op <$> sub
        fromProjectInject = do
          (exp1, vars1) <- sub
          (exp2, vars2) <- sub
          ds1 <- genDimSelector default1stDim2D
          ds2 <- genDimSelector default2ndDim2D
          let vars = mergeVarsAndParams [vars1, vars2]
          return (unsafeInject [ds1, ds2] (unsafeProject [ds1, ds2] exp1) exp2, vars)
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
            fromRotate,
            fromProjectInject
          ]

-------------------------------------------------------------------------------
data Suite d et
  = Suite (Expression d et) ValMaps
  deriving (Show)

-------------------------------------------------------------------------------
type SuiteScalarR = Suite Scalar R

type SuiteScalarC = Suite Scalar C

type SuiteOneR = Suite Default1D R

type SuiteOneC = Suite Default1D C

type SuiteTwoR = Suite '(Default2D1, Default2D2) R

type SuiteTwoC = Suite '(Default2D1, Default2D2) C

-------------------------------------------------------------------------------
instance Arbitrary SuiteScalarR where
  arbitrary = do
    (exp, vars) <- sized $ genScalarR
    valMaps <- genValMap vars
    return $ Suite exp valMaps

instance Arbitrary SuiteScalarC where
  arbitrary = do
    (exp, vars) <- sized $ genScalarC
    valMaps <- genValMap vars
    return $ Suite exp valMaps

instance Arbitrary SuiteOneR where
  arbitrary = do
    (exp, vars) <- sized $ gen1DR
    valMaps <- genValMap vars
    return $ Suite exp valMaps

instance Arbitrary SuiteOneC where
  arbitrary = do
    (exp, vars) <- sized $ gen1DC
    valMaps <- genValMap vars
    return $ Suite exp valMaps

instance Arbitrary SuiteTwoR where
  arbitrary = do
    (exp, vars) <- sized $ gen2DR
    valMaps <- genValMap vars
    return $ Suite exp valMaps

instance Arbitrary SuiteTwoC where
  arbitrary = do
    (exp, vars) <- sized $ gen2DC
    valMaps <- genValMap vars
    return $ Suite exp valMaps

-------------------------------------------------------------------------------
instance Arbitrary (Expression Scalar R) where
  arbitrary = fst <$> sized genScalarR

instance Arbitrary (Expression Scalar C) where
  arbitrary = fst <$> sized genScalarC

instance Arbitrary (Expression Default1D R) where
  arbitrary = fst <$> sized gen1DR

instance Arbitrary (Expression Default1D C) where
  arbitrary = fst <$> sized gen1DC

instance Arbitrary (Expression '(Default2D1, Default2D2) R) where
  arbitrary = fst <$> sized gen2DR

instance Arbitrary (Expression '(Default2D1, Default2D2) C) where
  arbitrary = fst <$> sized gen2DC

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

instance (Ix i) => ComplexRealOp (Array i Double) (Array i (Complex Double)) where
  (+:) arr1 arr2 = listArray (bounds arr1) $ zipWith (+:) (elems arr1) (elems arr2)
  xRe arr = listArray (bounds arr) $ map xRe (elems arr)
  xIm arr = listArray (bounds arr) $ map xIm (elems arr)
  conjugate arr = listArray (bounds arr) $ map conjugate (elems arr)
