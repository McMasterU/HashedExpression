{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module InterpSpec where

import Commons
import Data.Complex (Complex (..))
import Data.Map.Strict (fromList, union)
import Data.Maybe (fromJust)
import Debug.Trace (traceShowId)
import GHC.TypeLits (CmpNat, Div, KnownNat, Mod, Nat, natVal, type (+), type (-), type (<=))
import HashedExpression.Internal
import HashedExpression.Internal.Base
import HashedExpression.Internal.Expression
import HashedExpression.Internal.OperationSpec
import HashedExpression.Interp

import Test.Hspec
import Test.QuickCheck
import Var
import Prelude hiding ((**), (^))
import qualified Prelude

-------------------------------------------------------------------------------
unsafeProject :: [DimSelector] -> Expression d1 et1 -> Expression d2 et2
unsafeProject ds e = wrapExpression $ apply (Unary (specProject ds)) [asExpression e]

unsafeInject :: [DimSelector] -> Expression d1 et1 -> Expression d2 et2 -> Expression d2 et2
unsafeInject ds sub base = wrapExpression $ apply (Binary (specInject ds)) [asExpression sub, asExpression base]

unsafeRotate :: RotateAmount -> Expression d1 et1 -> Expression d2 et2
unsafeRotate amount e = wrapExpression $ apply (Unary (specRotate amount)) [asExpression e]

-- |
prop_RotateOneR1 :: SuiteOneR -> Bool
prop_RotateOneR1 (Suite exp valMap) =
  eval valMap (rotate 0 exp) == eval valMap exp

-- |
prop_RotateOneR2 :: SuiteOneR -> Int -> Bool
prop_RotateOneR2 (Suite exp valMap) amount =
  eval valMap (f exp) == eval valMap exp
  where
    f = rotate amount . rotate (- amount)

-- |
prop_RotateOneR3 :: SuiteOneR -> Int -> Int -> Bool
prop_RotateOneR3 (Suite exp valMap) amount1 amount2 =
  eval valMap (f1 exp) == eval valMap (f2 exp)
  where
    f1 = rotate amount1 . rotate amount2
    f2 = rotate (amount1 + amount2)

-- |
prop_RotateTwoR1 :: SuiteTwoR -> Bool
prop_RotateTwoR1 (Suite exp valMap) =
  eval valMap (rotate (0, 0) exp) == eval valMap exp

-- |
prop_RotateTwoR2 :: SuiteTwoR -> (Int, Int) -> Bool
prop_RotateTwoR2 (Suite exp valMap) (offset1, offset2) =
  eval valMap (f exp) == eval valMap exp
  where
    f = rotate (offset1, offset2) . rotate (- offset1, - offset2)

-- |
prop_RotateTwoR3 :: SuiteTwoR -> (Int, Int) -> (Int, Int) -> Bool
prop_RotateTwoR3 (Suite exp valMap) amount1 amount2 =
  eval valMap (f1 exp) == eval valMap (f2 exp)
  where
    f1 = rotate amount1 . rotate amount2
    f2 = rotate (fst amount1 + fst amount2, snd amount1 + snd amount2)

--- TODO: Maybe implement term-level projecting and injecting and thus able to randomize selectors ?
prop_ProjectInjectOneR :: SuiteOneR -> Expectation
prop_ProjectInjectOneR (Suite exp valMap) = do
  let s = range @1 @5
  eval valMap (inject s (project s exp) exp) `shouldBe` eval valMap exp
  let s = range @5 @6
  eval valMap (inject s (project s exp) exp) `shouldBe` eval valMap exp
  let s = at @7
  eval valMap (inject s (project s exp) exp) `shouldBe` eval valMap exp
  let s = range @0 @9
  eval valMap (inject s (project s exp) exp) `shouldBe` eval valMap exp
  let s1 = range @0 @4
      s2 = range @5 @(Default1D - 1)
  let part1 = project s1 exp
  let part2 = project s2 exp
  let combine = inject s1 part1 . inject s2 part2 $ zero1
  eval valMap combine `shouldBe` eval valMap exp
  let s1 = range @0 @2
      s2 = range @3 @(Default1D - 1)
  let part1 = project s1 exp
  let part2 = project s2 exp
  let combine = inject s1 part1 . inject s2 part2 $ zero1
  eval valMap combine `shouldBe` eval valMap exp

-- | Get 2 projection parts, injecting them to 0 correspondingly should equal the original
prop_ProjectInjectOneRUntyped :: SuiteOneR -> Expectation
prop_ProjectInjectOneRUntyped (Suite exp valMap) = do
  between <- generate $ elements [0 .. defaultDim1D - 2]
  let ds1 = [Range 0 between 1]
  let ds2 = [Range (between + 1) (defaultDim1D - 1) 1]
  let part1 = unsafeProject ds1 exp
  let part2 = unsafeProject ds2 exp
  let combined = unsafeInject ds1 part1 . unsafeInject ds2 part2 $ zero1
  eval valMap combined `shouldBe` eval valMap exp

prop_ProjectInjectOneC :: SuiteOneC -> Expectation
prop_ProjectInjectOneC (Suite exp valMap) = do
  let s = range @2 @3
  eval valMap (inject s (project s exp) exp) `shouldBe` eval valMap exp
  let s = range @1 @6
  eval valMap (inject s (project s exp) exp) `shouldBe` eval valMap exp
  let s = at @3
  eval valMap (inject s (project s exp) exp) `shouldBe` eval valMap exp
  let s = range @0 @9
  eval valMap (inject s (project s exp) exp) `shouldBe` eval valMap exp
  let s1 = ranges @0 @(Default1D - 1) @2
      s2 = ranges @1 @(Default1D - 1) @2
  let part1 = project s1 exp
  let part2 = project s2 exp
  let combine = inject s1 part1 . inject s2 part2 $ zero1 +: zero1
  eval valMap combine `shouldBe` eval valMap exp
  let s1 = ranges @0 @(Default1D - 1) @3
      s2 = ranges @1 @(Default1D - 1) @3
      s3 = ranges @2 @(Default1D - 1) @3
  let part1 = project s1 exp
  let part2 = project s2 exp
  let part3 = project s3 exp
  let combine = inject s3 part3 . inject s1 part1 . inject s2 part2 $ zero1 +: zero1
  eval valMap combine `shouldBe` eval valMap exp

prop_ProjectInjectTwoR :: SuiteTwoR -> Expectation
prop_ProjectInjectTwoR (Suite exp valMap) = do
  let s = (range @2 @3, at @4)
  eval valMap (inject s (project s exp) exp) `shouldBe` eval valMap exp
  let s = (ranges @1 @3 @2, at @2)
  eval valMap (inject s (project s exp) exp) `shouldBe` eval valMap exp
  let s = (at @3, at @3)
  eval valMap (inject s (project s exp) exp) `shouldBe` eval valMap exp
  let s = (at @3, range @0 @2)
  eval valMap (inject s (project s exp) exp) `shouldBe` eval valMap exp
  let part1 = project (range @0 @2, range @0 @(Default2D2 - 1)) exp
  let part2 = project (range @3 @4, range @0 @(Default2D2 - 1)) exp
  let combine =
        inject (range @0 @2, range @0 @(Default2D2 - 1)) part1
          . inject (range @3 @4, range @0 @(Default2D2 - 1)) part2
          $ zero2
  eval valMap combine `shouldBe` eval valMap exp

prop_ProjectInjectTwoC :: SuiteTwoC -> Expectation
prop_ProjectInjectTwoC (Suite exp valMap) = do
  let s = (range @2 @3, at @4)
  eval valMap (inject s (project s exp) exp) `shouldBe` eval valMap exp
  let s = (ranges @1 @3 @2, at @2)
  eval valMap (inject s (project s exp) exp) `shouldBe` eval valMap exp
  let s = (at @3, at @3)
  eval valMap (inject s (project s exp) exp) `shouldBe` eval valMap exp
  let s = (at @3, range @0 @2)
  eval valMap (inject s (project s exp) exp) `shouldBe` eval valMap exp
  let part1 = project (range @0 @2, range @0 @(Default2D2 - 1)) exp
  let part2 = project (range @3 @4, range @0 @(Default2D2 - 1)) exp
  let combine =
        inject (range @0 @2, range @0 @(Default2D2 - 1)) part1
          . inject (range @3 @4, range @0 @(Default2D2 - 1)) part2
          $ zero2 +: zero2
  eval valMap combine `shouldBe` eval valMap exp

-- ideas:
--    dot product (<.>) vs scaling (*.)
--    exponential and log
--    conjugate of complex number
--    power and square root
--    advance: piecewise function

-- properties of dot product
-- u . v = |u||v| cos t
-- u . v = v . u
-- u . v = 0 when u and v are orthogonal
-- 0 . 0 = 0

-- | v|^2 = v . v
--  a (u . v) = (a u) . v
--  (au + bv) . w = (au) . w + (bv) . w

-- [Scalar] 0 . 0 = 0
-- prop_dotProductScalar_1 ::  Bool
-- prop_dotProductScalar_1 =
--   let
--     n = variable "n"
--     valMap = fromList [("n", VNum 0)]
--   in
--     n <.> n == n

-- u . v = v . u
prop_dotProduct1D_1 :: SuiteOneR -> SuiteOneR -> Bool
prop_dotProduct1D_1 (Suite exp1 valMap1) (Suite exp2 valMap2) =
  eval valMap (exp1 <.> exp2) == eval valMap (exp2 <.> exp1)
  where
    valMap = valMap1 `union` valMap2

prop_dotProduct2D_1 :: SuiteTwoR -> SuiteTwoR -> Bool
prop_dotProduct2D_1 (Suite exp1 valMap1) (Suite exp2 valMap2) =
  eval valMap (exp1 <.> exp2) == eval valMap (exp2 <.> exp1)
  where
    valMap = valMap1 `union` valMap2

--[1D] |v|^2 = v . v
-- prop_dotProduct1D_2 :: SuiteOneR -> Bool
-- prop_dotProduct1D_2 (Suite exp1 valMap1) =
--     eval valMap1 ((??? exp1)^2) == eval valMap1 (exp1 <.> exp1)

--  a (u . v) = (a u) . v
prop_dotProduct1D_3 :: SuiteScalarR -> SuiteOneR -> SuiteOneR -> Expectation
prop_dotProduct1D_3 (Suite a valMap1) (Suite exp1 valMap2) (Suite exp2 valMap3) =
  eval valMap (a * (exp1 <.> exp2)) `shouldApprox` eval valMap ((a *. exp1) <.> exp2)
  where
    valMap = valMap1 `union` valMap2 `union` valMap3

prop_dotProduct2D_3 :: SuiteScalarR -> SuiteTwoR -> SuiteTwoR -> Expectation
prop_dotProduct2D_3 (Suite a valMap1) (Suite exp1 valMap2) (Suite exp2 valMap3) =
  eval valMap (a * (exp1 <.> exp2)) `shouldApprox` eval valMap ((a *. exp1) <.> exp2)
  where
    valMap = valMap1 `union` valMap2 `union` valMap3

-- (au + bv) . w = (au) . w + (bv) . w
prop_dotProduct1D_4 :: SuiteScalarR -> SuiteScalarR -> SuiteOneR -> SuiteOneR -> SuiteOneR -> Expectation
prop_dotProduct1D_4 (Suite a valMap1) (Suite b valMap2) (Suite exp1 valMap3) (Suite exp2 valMap4) (Suite exp3 valMap5) =
  eval valMap (((a *. exp1) + (b *. exp2)) <.> exp3) `shouldApprox` eval valMap (((a *. exp1) <.> exp3) + ((b *. exp2) <.> exp3))
  where
    valMap = valMap1 `union` valMap2 `union` valMap3 `union` valMap4 `union` valMap5

-- [ arithmetic properties ]
-- 1) commutative properties:  a + b = b + a
--                             a * b = b * a
-- 2) associative properties:  a + (b + c) = (a + b) + c
--                             (a * b) * c = a * (b * c)
-- 3) Distributive properties: a * (b + c) = a * b + a * c
-- 4) Identity element: a + 0 = a
--                      a * 1 = a
-- 5) Inverse Element:  a + (-a) = 0
--                      a * (1 / a) = 1

prop_Commutative_Addition :: SuiteScalarR -> SuiteScalarR -> Bool
prop_Commutative_Addition (Suite exp1 valMap1) (Suite exp2 valMap2) =
  eval valMap (exp1 + exp2) == eval valMap (exp2 + exp1)
  where
    valMap = valMap1 `union` valMap2

prop_Commutative_Multiplication :: SuiteScalarR -> SuiteScalarR -> Bool
prop_Commutative_Multiplication (Suite exp1 valMap1) (Suite exp2 valMap2) =
  eval valMap (exp1 * exp2) == eval valMap (exp2 * exp1)
  where
    valMap = valMap1 `union` valMap2

prop_Distributive :: SuiteScalarR -> SuiteScalarR -> SuiteScalarR -> Expectation
prop_Distributive (Suite exp1 valMap1) (Suite exp2 valMap2) (Suite exp3 valMap3) =
  eval valMap (exp1 * (exp2 + exp3)) `shouldApprox` eval valMap ((exp1 * exp2) + (exp1 * exp3))
  where
    valMap = valMap1 `union` valMap2 `union` valMap3

prop_Identity_Addition :: SuiteScalarR -> Bool
prop_Identity_Addition (Suite exp1 valMap1) =
  eval valMap1 (exp1 + 0) == eval valMap1 exp1

prop_Identity_Multiplication :: SuiteScalarR -> Bool
prop_Identity_Multiplication (Suite exp1 valMap1) =
  eval valMap1 (exp1 * 1) == eval valMap1 exp1

prop_Inverse_Addition :: SuiteScalarR -> Bool
prop_Inverse_Addition (Suite exp1 valMap1) =
  eval valMap1 (exp1 + (negate exp1)) == VR 0

prop_Inverse_Multiplication :: SuiteScalarR -> Property
prop_Inverse_Multiplication (Suite exp1 valMap1) =
  eval valMap1 exp1 /= VR 0
    ==> (eval valMap1 (exp1 * (1 / exp1)) `shouldApprox` VR 1)

-- [ exponential properties ]
-- 1) x^a * x^b = x ^ (a+b)
-- 2) (xy)^a = x^a * y^a
-- 3) (x^a) / (x^b) = x ^ (a-b), x != 0
-- 4) a^(-m) = 1 / a^m, a != 0
-- 5) (a/b)^m = a^m / b^m, b != 0

newtype IntB n = IntB Int deriving (Show, Eq, Ord)

instance (KnownNat n) => Arbitrary (IntB n) where
  arbitrary = do
    let nNat = nat @n
    x <- arbitrary
    return $ IntB $ abs (x `mod` nNat)

prop_ExpScalar_1 :: SuiteScalarR -> IntB 5 -> IntB 5 -> Expectation
prop_ExpScalar_1 (Suite exp1 valMap1) (IntB a) (IntB b) =
  eval valMap1 ((exp1 ^ a) * (exp1 ^ b)) `shouldApprox` eval valMap1 (exp1 ^ (a + b))

prop_ExpScalar_2 :: SuiteScalarR -> SuiteScalarR -> IntB 5 -> Expectation
prop_ExpScalar_2 (Suite exp1 valMap1) (Suite exp2 valMap2) (IntB a) =
  eval valMap ((exp1 * exp2) ^ a) `shouldApprox` eval valMap ((exp1 ^ a) * (exp2 ^ a))
  where
    valMap = valMap1 `union` valMap2

prop_ExpScalar_3 :: SuiteScalarR -> IntB 10 -> IntB 10 -> Property
prop_ExpScalar_3 (Suite exp1 valMap1) (IntB a) (IntB b) =
  (eval valMap1 exp1 /= VR 0)
    ==> (eval valMap1 ((exp1 ^ a) / (exp1 ^ b)) `shouldApprox` eval valMap1 (exp1 ^ (a - b)))

prop_ExpScalar_4 :: SuiteScalarR -> IntB 10 -> Property
prop_ExpScalar_4 (Suite exp1 valMap1) (IntB a) =
  (eval valMap1 exp1 /= VR 0)
    ==> (eval valMap1 (exp1 ^ (- a)) `shouldApprox` eval valMap1 (1 / (exp1 ^ a)))

prop_TransposeTwice :: forall m n et. (IsElementType et, KnownNat m, KnownNat n) => Suite (D2 m n) et -> Expectation
prop_TransposeTwice (Suite exp valMap) =
  eval valMap (transpose (transpose exp)) `shouldApprox` eval valMap exp

prop_TransposeMatrixMultiplication ::
  forall m n p et.
  (IsElementType et, KnownNat m, KnownNat n, KnownNat p) =>
  Suite (D2 m n) et ->
  Suite (D2 n p) et ->
  Expectation
prop_TransposeMatrixMultiplication (Suite exp1 valMap1) (Suite exp2 valMap2) =
  eval valMap (transpose $ exp1 ** exp2) `shouldApprox` eval valMap (transpose exp2 ** transpose exp1)
  where
    valMap = valMap1 `union` valMap2

prop_MatrixMultplicationAssociative ::
  forall m n p q et.
  ( IsElementType et,
    KnownNat m,
    KnownNat n,
    KnownNat p,
    KnownNat q
  ) =>
  Suite (D2 m n) et ->
  Suite (D2 n p) et ->
  Suite (D2 p q) et ->
  Expectation
prop_MatrixMultplicationAssociative (Suite a valMap1) (Suite b valMap2) (Suite c valMap3) =
  eval valMap ((a ** b) ** c) `shouldApprox` eval valMap (a ** (b ** c))
  where
    valMap = valMap1 `union` valMap2 `union` valMap3

spec :: Spec
spec =
  describe "Interp spec" $ do
    specify "prop_dotProduct1D_1" $ property prop_dotProduct1D_1
    specify "prop_dotProduct2D_1" $ property prop_dotProduct2D_1
    specify "prop_dotProduct1D_3" $ property prop_dotProduct1D_3
    specify "prop_dotProduct2D_3" $ property prop_dotProduct2D_3
    specify "prop_dotProduct1D_4" $ property prop_dotProduct1D_4

    --arithmetic properties
    specify "prop_Commutative_Addition" $ property prop_Commutative_Addition
    specify "prop_Commutative_Multiplication" $ property prop_Commutative_Multiplication
    specify "prop_Distributive" $ property prop_Distributive
    specify "prop_Identity_Addition" $ property prop_Identity_Addition
    specify "prop_Identity_Multiplication" $ property prop_Identity_Multiplication
    specify "prop_Inverse_Addition" $ property prop_Inverse_Addition
    specify "prop_Inverse_Multiplication" $ property prop_Inverse_Multiplication

    --exponential properties
    specify "prop_ExpScalar_1" $ property prop_ExpScalar_1
    specify "prop_ExpScalar_2" $ property prop_ExpScalar_2
    specify "prop_ExpScalar_3" $ property prop_ExpScalar_3
    specify "prop_ExpScalar_4" $ property prop_ExpScalar_4

    specify "prop_Rotate One R rotate 0 should stay the same" $
      property prop_RotateOneR1
    specify "prop_Rotate One R rotate a and -a should stay the same" $
      property prop_RotateOneR2
    specify "prop_Rotate One R rotate a then rotate b should equal rotate (a + b)" $
      property prop_RotateOneR3
    specify "prop_Rotate Two R rotate (0, 0) should stay the same" $
      property prop_RotateTwoR1
    specify "prop_Rotate Two R rotate a and -a should stay the same" $
      property prop_RotateTwoR2
    specify "prop_Rotate Two R rotate a then rotate b should equal rotate (a + b)" $
      property prop_RotateTwoR3
    specify "prop_Project_Inject One R" $
      property prop_ProjectInjectOneR
    specify "prop_Project_Inject One R Untyped" $
      property prop_ProjectInjectOneRUntyped
    specify "prop_Project_Inject One C" $
      property prop_ProjectInjectOneC
    specify "prop_Project_Inject Two R" $
      property prop_ProjectInjectTwoR
    specify "prop_Project_Inject Two C" $
      property prop_ProjectInjectTwoC
    specify "prop_Transpose_Twice 1" $
      property (prop_TransposeTwice @3 @4 @R)
    specify "prop_Transpose_Twice 2" $
      property (prop_TransposeTwice @5 @7 @C)
    specify "prop_Transpose_Twice 3" $
      property (prop_TransposeTwice @4 @10 @R)
    specify "transpose & matrix multiplication 1" $
      property (prop_TransposeMatrixMultiplication @3 @4 @5 @R)
    specify "transpose & matrix multiplication 2" $
      property (prop_TransposeMatrixMultiplication @5 @8 @9 @C)
    specify "transpose & matrix multiplication 3" $
      property (prop_TransposeMatrixMultiplication @4 @2 @3 @R)
    specify "matrix mul associative 1" $
      property (prop_MatrixMultplicationAssociative @4 @2 @3 @1 @R)
    specify "matrix mul associative 2" $
      property (prop_MatrixMultplicationAssociative @4 @5 @6 @3 @C)
