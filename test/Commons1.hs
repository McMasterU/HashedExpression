{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Commons1 where

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
import HashedExpression.Internal.Expression

import GHC.TypeLits (KnownNat)
import HashedExpression.Internal.Normalize
import HashedExpression.Internal.Utils
import Var
import HashedExpression.Interp
import HashedExpression.Operation
import qualified HashedExpression.Operation
import HashedExpression.Prettify
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
type Vars = [[(String, Shape)]] -- Vars 0D, 1D, 2D, 3D, ..

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
    undefined

--    let [names0d, names1d, names2d, names3d] = vars
--    list0d <- vectorOf (length names0d) genDouble
--    let vm0 = Map.fromList . zip names0d $ map VScalar list0d
--    list1d <- vectorOf (length names1d) . vectorOf defaultDim1D $ genDouble
--    let vm1 =
--            Map.fromList .
--            zip names1d . map (V1D . listArray (0, defaultDim1D - 1)) $
--            list1d
--    list2d <-
--        vectorOf (length names2d) . vectorOf (default1stDim2D * default2ndDim2D) $
--        genDouble
--    let vm2 =
--            Map.fromList .
--            zip names2d .
--            map
--                (V2D .
--                 listArray ((0, 0), (default1stDim2D - 1, default2ndDim2D - 1))) $
--            list2d
--    list3d <-
--        vectorOf (length names3d) .
--        vectorOf (default1stDim2D * default2ndDim2D * default3rdDim3D) $
--        genDouble
--    let vm3 =
--            Map.fromList .
--            zip names3d .
--            map
--                (V3D .
--                 listArray
--                     ( (0, 0, 0)
--                     , ( default1stDim3D - 1
--                       , default2ndDim3D - 1
--                       , default3rdDim3D - 1))) $
--            list3d
--    return $ Map.unions [vm0, vm1, vm2, vm3]
shouldApprox :: (HasCallStack, Approximable a) => a -> a -> Expectation
shouldApprox x y = assertBool msg (x ~= y)
  where
    msg = "Expected: " ++ prettifyShow x ++ "\nGot: " ++ prettifyShow y

infix 1 `shouldApprox`

liftE1 ::
       (Expression d1 et1 -> Expression d2 et2)
    -> (Expression d1 et1, Vars)
    -> (Expression d2 et2, Vars)
liftE1 op (e, v) = (op e, v)

liftE2 ::
       (Expression d1 et1 -> Expression d2 et2 -> Expression d3 et3)
    -> (Expression d1 et1, Vars)
    -> (Expression d2 et2, Vars)
    -> (Expression d3 et3, Vars)
liftE2 op (e1, v1) (e2, v2) = ((op e1 e2), (mergeVars [v1, v2]))

-------------------------------------------------------------------------------
data Suite d et =
    Suite (Expression d et) ValMaps

-------------------------------------------------------------------------------
primitiveScalarR :: Gen (Expression Scalar R, Vars)
primitiveScalarR = do
    name <- elements . map pure $ ['a' .. 'z']
    dbl <- genDouble
    r <- (`mod` 10) <$> arbitrary
    if r < 6
        then return ((var name), [[(name, [])], [], [], []])
        else return ((const dbl), [[], [], [], []])

primitiveScalarC :: Gen (Expression Scalar C, Vars)
primitiveScalarC = liftE2 (+:) <$> primitiveScalarR <*> primitiveScalarR

-------------------------------------------------------------------------------
primitive1DR ::
       forall n. KnownNat n
    => Gen (Expression n R, Vars)
primitive1DR = do
    name <- elements . map ((++ "1") . pure) $ ['a' .. 'z']
    dbl <- genDouble
    r <- (`mod` 10) <$> arbitrary
    if r < 6
        then return
                 ( (variable1D @n name)
                 , [[], [(name, [valueFromNat @n])], [], []])
        else return ((constant1D @n dbl), [[], [], [], []])

primitive1DC ::
       forall n. KnownNat n
    => Gen (Expression n C, Vars)
primitive1DC = liftE2 (+:) <$> primitive1DR @n <*> primitive1DR @n

-------------------------------------------------------------------------------
primitive2DR ::
       forall m n. (KnownNat m, KnownNat n)
    => Gen (Expression '( m, n) R, Vars)
primitive2DR = do
    name <- elements . map ((++ "2") . pure) $ ['a' .. 'z']
    dbl <- genDouble
    r <- (`mod` 10) <$> arbitrary
    if r < 6
        then return
                 ( variable2D @m @n name
                 , [[], [], [(name, [valueFromNat @m, valueFromNat @n])], []])
        else return (constant2D @m @n dbl, [[], [], [], []])

primitive2DC ::
       forall m n. (KnownNat m, KnownNat n)
    => Gen (Expression '( m, n) C, Vars)
primitive2DC = liftE2 (+:) <$> primitive2DR @m @n <*> primitive2DR @m @n

--type  = 10

-------------------------------------------------------------------------------
genScalarR :: Int -> Gen (Expression Scalar R, Vars)
genScalarR size
    | size == 0 = primitiveScalarR
    | otherwise =
        let sub = genScalarR (size `div` 3)
            subC = genScalarC (size `div` 3)
            sub1D = gen1DR @Default1D (size `div` 3)
            sub2D = gen2DR @Default2D1 @Default2D2 (size `div` 3)
            fromPiecewise = do
                numBranches <- elements [2 .. 4]
                branches <- vectorOf numBranches sub
                condition <- sub
                marks <- sort <$> vectorOfDifferent (numBranches - 1) arbitrary
                let vars = mergeVars $ map snd branches ++ [snd condition]
                    exp = piecewise marks (fst condition) $ map fst branches
                return (exp, vars)
            binary op = liftE2 op <$> sub <*> sub
            unary op = liftE1 op <$> sub
         in oneof
                [ fromPiecewise
                , binary (+)
                , binary (*)
                , binary (*.)
                , binary (-)
                , binary (<.>)
                , unary negate
                , unary (^ 2)
                , liftE1 xRe <$> subC
                , liftE1 xIm <$> subC
                , liftE2 (<.>) <$> sub1D <*> sub1D
                , liftE2 (<.>) <$> sub2D <*> sub2D
                ]



genScalarC :: Int -> Gen (Expression Scalar C, Vars)
genScalarC size
    | size == 0 = primitiveScalarC
    | otherwise =
        let sub = genScalarC (size `div` 3)
            subR = genScalarR (size `div` 3)
            sub1D = gen1DC @Default1D (size `div` 3)
            sub2D = gen2DC @Default2D1 @Default2D2 (size `div` 3)
            fromPiecewise = do
                numBranches <- elements [2 .. 4]
                branches <- vectorOf numBranches sub
                condition <- subR
                marks <- sort <$> vectorOfDifferent (numBranches - 1) arbitrary
                let vars = mergeVars $ map snd branches ++ [snd condition]
                    exp = piecewise marks (fst condition) $ map fst branches
                return (exp, vars)
            binary op = liftE2 op <$> sub <*> sub
            unary op = liftE1 op <$> sub
         in oneof
                [ fromPiecewise
                , binary (+)
                , binary (*)
                , binary (*.)
                , binary (-)
                , binary (<.>)
                , unary negate
                , unary (^ 2)
                , liftE2 (+:) <$> subR <*> subR
                , liftE2 (<.>) <$> sub1D <*> sub1D
                , liftE2 (<.>) <$> sub2D <*> sub2D
                ]

gen1DR ::
       forall n. KnownNat n
    => Int
    -> Gen (Expression n R, Vars)
gen1DR size = undefined

gen1DC ::
       forall n. KnownNat n
    => Int
    -> Gen (Expression n C, Vars)
gen1DC size = undefined

gen2DR ::
       forall m n. (KnownNat m, KnownNat n)
    => Int
    -> Gen (Expression '( m, n) R, Vars)
gen2DR size = undefined

gen2DC ::
       forall m n. (KnownNat m, KnownNat n)
    => Int
    -> Gen (Expression '( m, n) C, Vars)
gen2DC size = undefined

-------------------------------------------------------------------------------
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
