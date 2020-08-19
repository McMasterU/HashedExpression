{-# LANGUAGE TypeOperators #-}

module InterpSpec where

import Commons
import Data.Complex (Complex (..))
import Data.Map.Strict (union)
import Data.Maybe (fromJust)
import Debug.Trace (traceShowId)
import GHC.TypeLits (CmpNat, Div, KnownNat, Mod, natVal, type (+), type (-), type (<=))
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Utils
import HashedExpression.Interp
import HashedExpression.Operation hiding (product, sum)
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import Test.Hspec
import Test.QuickCheck
import Var
import Prelude hiding ((^))

-- |
prop_AddScalarR :: SuiteScalarR -> SuiteScalarR -> Bool
prop_AddScalarR (Suite exp1 valMaps1) (Suite exp2 valMaps2) =
  eval valMaps (exp1 + exp2) == eval valMaps exp1 + eval valMaps exp2
  where
    valMaps = valMaps1 `union` valMaps2

-- |
prop_MultiplyScalarR :: SuiteScalarR -> SuiteScalarR -> Bool
prop_MultiplyScalarR (Suite exp1 valMaps1) (Suite exp2 valMaps2) =
  eval valMaps (exp1 * exp2) == eval valMaps exp1 * eval valMaps exp2
  where
    valMaps = valMaps1 `union` valMaps2

-- |
prop_AddScalarC :: SuiteScalarC -> SuiteScalarC -> Bool
prop_AddScalarC (Suite exp1 valMaps1) (Suite exp2 valMaps2) =
  eval valMaps (exp1 + exp2) == eval valMaps exp1 + eval valMaps exp2
  where
    valMaps = valMaps1 `union` valMaps2

-- |
prop_MultiplyScalarC :: SuiteScalarC -> SuiteScalarC -> Bool
prop_MultiplyScalarC (Suite exp1 valMaps1) (Suite exp2 valMaps2) =
  eval valMaps (exp1 * exp2) == eval valMaps exp1 * eval valMaps exp2
  where
    valMaps = valMaps1 `union` valMaps2

-- |
prop_RotateOneR1 :: SuiteOneR -> Bool
prop_RotateOneR1 (Suite exp valMaps) =
  eval valMaps (rotate 0 exp) == eval valMaps exp

-- |
prop_RotateOneR2 :: SuiteOneR -> Int -> Bool
prop_RotateOneR2 (Suite exp valMaps) amount =
  eval valMaps (f exp) == eval valMaps exp
  where
    f = rotate amount . rotate (- amount)

-- |
prop_RotateOneR3 :: SuiteOneR -> Int -> Int -> Bool
prop_RotateOneR3 (Suite exp valMaps) amount1 amount2 =
  eval valMaps (f1 exp) == eval valMaps (f2 exp)
  where
    f1 = rotate amount1 . rotate amount2
    f2 = rotate (amount1 + amount2)

-- |
prop_RotateTwoR1 :: SuiteTwoR -> Bool
prop_RotateTwoR1 (Suite exp valMaps) =
  eval valMaps (rotate (0, 0) exp) == eval valMaps exp

-- |
prop_RotateTwoR2 :: SuiteTwoR -> (Int, Int) -> Bool
prop_RotateTwoR2 (Suite exp valMaps) (offset1, offset2) =
  eval valMaps (f exp) == eval valMaps exp
  where
    f = rotate (offset1, offset2) . rotate (- offset1, - offset2)

-- |
prop_RotateTwoR3 :: SuiteTwoR -> (Int, Int) -> (Int, Int) -> Bool
prop_RotateTwoR3 (Suite exp valMaps) amount1 amount2 =
  eval valMaps (f1 exp) == eval valMaps (f2 exp)
  where
    f1 = rotate amount1 . rotate amount2
    f2 = rotate (fst amount1 + fst amount2, snd amount1 + snd amount2)
    
--    a +: b = a + bi
prop_realImag :: SuiteTwoR -> SuiteTwoR -> Expectation
prop_realImag (Suite exp1 valMap1) (Suite exp2 valMap2) = do
  (eval valMap exp1 +: eval valMap) exp2 `shouldApprox` eval valMap (exp1 +: exp2)
  where 
    valMap = valMap1 `union` valMap2

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

spec :: Spec
spec =
  describe "Interp spec" $ do
    specify "prop_realImag" $ property prop_realImag
--    specify "prop_Add Scalar R" $ property prop_AddScalarR
--    specify "prop_Multiply Scalar R" $ property prop_MultiplyScalarR
--    specify "prop_Add Scalar C" $ property prop_AddScalarC
--    specify "prop_Multiply Scalar C" $ property prop_MultiplyScalarC
--    specify "prop_Rotate One R rotate 0 should stay the same" $
--      property prop_RotateOneR1
--    specify "prop_Rotate One R rotate a and -a should stay the same" $
--      property prop_RotateOneR2
--    specify "prop_Rotate One R rotate a then rotate b should equal rotate (a + b)" $
--      property prop_RotateOneR3
--    specify "prop_Rotate Two R rotate (0, 0) should stay the same" $
--      property prop_RotateTwoR1
--    specify "prop_Rotate Two R rotate a and -a should stay the same" $
--      property prop_RotateTwoR2
--    specify "prop_Rotate Two R rotate a then rotate b should equal rotate (a + b)" $
--      property prop_RotateTwoR3
--    specify "prop_Project_Inject One R" $
--      property prop_ProjectInjectOneR
--    specify "prop_Project_Inject One R Untyped" $
--      property prop_ProjectInjectOneRUntyped
--    specify "prop_Project_Inject One C" $
--      property prop_ProjectInjectOneC
--    specify "prop_Project_Inject Two R" $
--      property prop_ProjectInjectTwoR
--    specify "prop_Project_Inject Two C" $
--      property prop_ProjectInjectTwoC
