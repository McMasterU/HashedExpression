module InterpSpec where

import Commons
import Data.Complex (Complex(..))
import Data.Map.Strict (union)
import Data.Maybe (fromJust)
import Debug.Trace (traceShowId)
import HashedExpression.Internal.Expression

import HashedExpression.Internal.Normalize
import HashedExpression.Internal.Utils
import HashedExpression.Interp
import HashedExpression.Operation hiding (product, sum)
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
    , sum
    , tan
    , tanh
    )
import Test.Hspec
import Test.QuickCheck (property)
import Var

-- |
--
prop_AddScalarR :: SuiteScalarR -> SuiteScalarR -> Bool
prop_AddScalarR (Suite exp1 valMaps1) (Suite exp2 valMaps2) =
    eval valMaps (exp1 + exp2) == eval valMaps exp1 + eval valMaps exp2
  where
    valMaps = valMaps1 `union` valMaps2

-- |
--
prop_MultiplyScalarR :: SuiteScalarR -> SuiteScalarR -> Bool
prop_MultiplyScalarR (Suite exp1 valMaps1) (Suite exp2 valMaps2) =
    eval valMaps (exp1 * exp2) == eval valMaps exp1 * eval valMaps exp2
  where
    valMaps = valMaps1 `union` valMaps2

-- |
--
prop_AddScalarC :: SuiteScalarC -> SuiteScalarC -> Bool
prop_AddScalarC (Suite exp1 valMaps1) (Suite exp2 valMaps2) =
    eval valMaps (exp1 + exp2) == eval valMaps exp1 + eval valMaps exp2
  where
    valMaps = valMaps1 `union` valMaps2

-- |
--
prop_MultiplyScalarC :: SuiteScalarC -> SuiteScalarC -> Bool
prop_MultiplyScalarC (Suite exp1 valMaps1) (Suite exp2 valMaps2) =
    eval valMaps (exp1 * exp2) == eval valMaps exp1 * eval valMaps exp2
  where
    valMaps = valMaps1 `union` valMaps2

-- |
--
prop_RotateOneR1 :: SuiteOneR -> Bool
prop_RotateOneR1 (Suite exp valMaps) =
    eval valMaps (rotate 0 exp) == eval valMaps exp

-- |
--
prop_RotateOneR2 :: SuiteOneR -> Int -> Bool
prop_RotateOneR2 (Suite exp valMaps) amount =
    eval valMaps (f exp) == eval valMaps exp
  where
    f = rotate amount . rotate (-amount)

-- |
--
prop_RotateOneR3 :: SuiteOneR -> Int -> Int -> Bool
prop_RotateOneR3 (Suite exp valMaps) amount1 amount2 =
    eval valMaps (f1 exp) == eval valMaps (f2 exp)
  where
    f1 = rotate amount1 . rotate amount2
    f2 = rotate (amount1 + amount2)

-- |
--
prop_RotateTwoR1 :: SuiteTwoR -> Bool
prop_RotateTwoR1 (Suite exp valMaps) =
    eval valMaps (rotate (0, 0) exp) == eval valMaps exp

-- |
--
prop_RotateTwoR2 :: SuiteTwoR -> (Int, Int) -> Bool
prop_RotateTwoR2 (Suite exp valMaps) (offset1, offset2) =
    eval valMaps (f exp) == eval valMaps exp
  where
    f = rotate (offset1, offset2) . rotate (-offset1, -offset2)

-- |
--
prop_RotateTwoR3 :: SuiteTwoR -> (Int, Int) -> (Int, Int) -> Bool
prop_RotateTwoR3 (Suite exp valMaps) amount1 amount2 =
    eval valMaps (f1 exp) == eval valMaps (f2 exp)
  where
    f1 = rotate amount1 . rotate amount2
    f2 = rotate (fst amount1 + fst amount2, snd amount1 + snd amount2)

spec :: Spec
spec =
    describe "Interp spec" $ do
        specify "prop_Add Scalar R" $ property prop_AddScalarR
        specify "prop_Multiply Scalar R" $ property prop_MultiplyScalarR
        specify "prop_Add Scalar C" $ property prop_AddScalarC
        specify "prop_Multiply Scalar C" $ property prop_MultiplyScalarC
        specify "prop_Rotate One R rotate 0 should stay the same" $
            property prop_RotateOneR1
        specify "prop_Rotate One R rotate a and -a should stay the same" $
            property prop_RotateOneR2
        specify
            "prop_Rotate One R rotate a then rotate b should equal rotate (a + b)" $
            property prop_RotateOneR3
        specify "prop_Rotate Two R rotate (0, 0) should stay the same" $
            property prop_RotateTwoR1
        specify "prop_Rotate Two R rotate a and -a should stay the same" $
            property prop_RotateTwoR2
        specify
            "prop_Rotate Two R rotate a then rotate b should equal rotate (a + b)" $
            property prop_RotateTwoR3
