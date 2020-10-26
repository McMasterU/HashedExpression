{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

module HashedExpression.Modeling.Unit where

import Data.Kind
import GHC.TypeLits (ErrorMessage (..), KnownNat, Nat, Symbol, TypeError, type (+))
import qualified GHC.TypeNats as N
import HashedExpression.Internal.Base (ElementType (..), IsExpression (..), IsScalarReal (..), Op (..))
import qualified HashedExpression.Internal.Base as Base
import HashedExpression.Internal.Builder (ExprBuilder, buildExpr)
import HashedExpression.Internal.MonadExpression
import HashedExpression.Modeling.Typed (IsShape (..), Scalar)
import HashedExpression.Modeling.Unit.Common
import HashedExpression.Modeling.Unit.SI (Kilogram, Meter, Recip, SameUnit, Unit)
import qualified HashedExpression.Modeling.Unit.SI as U
import HashedExpression.Modeling.Unit.TypeInt (TypeInt (..))
import Prelude hiding ((*), (+), (/))
import qualified Prelude as P

--------------------------------------------------------------------------------

data SampleStep = Nat :/: Nat

type family a :/ b where
  a :/ 0 =
    TypeError
      ( Text "Division to zero: "
          :<>: ShowType a
          :<>: Text "/0"
      )
  a :/ b = a :/: b

type family EqSampleStep (x :: SampleStep) (y :: SampleStep) :: Bool where
  EqSampleStep (a :/: b) (c :/: d) = a N.* d ==? c N.* b

type family PrintSampleStep (x :: SampleStep) :: ErrorMessage where
  PrintSampleStep (a :/: b) = ShowType a :<>: Text "/" :<>: ShowType b

type family SameSampleStep (x :: SampleStep) (y :: SampleStep) :: Constraint where
  SameSampleStep x y =
    Unless
      (EqSampleStep x y)
      ( Text "Sample step mismatch"
          :$$: (Text "Expected: " :<>: PrintSampleStep x)
          :$$: (Text "  Actual: " :<>: PrintSampleStep y)
      )

--------------------------------------------------------------------------------

type family SameUnitSampleStep (x :: (Unit, SampleStep)) (y :: (Unit, SampleStep)) :: Constraint where
  SameUnitSampleStep '(unit1, step1) '(unit2, step2) =
    (SameUnit unit1 unit2, SameSampleStep step1 step2)

type family SameUnitSampleSteps (x :: [(Unit, SampleStep)]) (y :: [(Unit, SampleStep)]) :: Constraint where
  SameUnitSampleSteps '[] '[] = Satisfied
  SameUnitSampleSteps (unitStep1 ': rest1) (unitStep2 ': rest2) =
    (SameUnitSampleStep unitStep1 unitStep2, SameUnitSampleSteps rest1 rest2)

--------------------------------------------------------------------------------
-- data Frame = Frame [(Unit, SampleStep)]

-- type FRAME = 'Frame

-- type family SameFrame (f1 :: Frame) (f2 :: Frame) :: Constraint where
--   SameFrame ( 'Frame unitSteps1) ( 'Frame unitSteps2) =
--     (SameUnitSampleSteps unitSteps1 unitSteps2)

-- type family FrameDimensionLength (frame :: Frame) :: Nat where
--   FrameDimensionLength (FRAME frameUnits) = ListLength frameUnits

-- data FrameDimensionOfLength = FrameDimensionOfLength Nat

-- type FrameShapeMatched frame shape = FrameDimensionLength frame ~ ListLength shape

--------------------------------------------------------------------------------

type DomainUnit = Unit

type RangeUnit = Unit

type NumSample = Nat

data Dimension = Dimension NumSample SampleStep DomainUnit

type D = 'Dimension

--------------------------------------------------------------------------------
data
  UnitExpr
    (dimensions :: [Dimension])
    (rangeUnit :: RangeUnit)
    (et :: ElementType)
  = UnitExpr ExprBuilder

instance IsExpression (UnitExpr dimensions unit et) where
  asRawExpr (UnitExpr exprBuilder) = buildExpr exprBuilder

instance IsScalarReal (UnitExpr '[] unit R) where
  asScalarRealRawExpr = asRawExpr

-- --------------------------------------------------------------------------------

-- gvariable ::
--   forall frame unit shape.
--   (IsShape shape, FrameShapeMatched frame shape) =>
--   String ->
--   UnitExpr frame unit shape R
-- gvariable name = UnitExpr $ introduceNode (toShape @shape, R, Var name)

-- gparam ::
--   forall frame unit shape.
--   (IsShape shape, FrameShapeMatched frame shape) =>
--   String ->
--   UnitExpr frame unit shape R
-- gparam name = UnitExpr $ introduceNode (toShape @shape, R, Param name)

-- gconstant ::
--   forall frame unit shape.
--   (IsShape shape, FrameShapeMatched frame shape) =>
--   Double ->
--   UnitExpr frame unit shape R
-- gconstant val = UnitExpr $ introduceNode (toShape @shape, R, Const val)

-- constant :: forall unit. Double -> UnitExpr FRAMELESS unit Scalar R
-- constant = gconstant @FRAMELESS @unit @Scalar

-- constant1D ::
--   forall frame unit n.
--   (KnownNat n) =>
--   FrameShapeMatched frame '[n] =>
--   Double ->
--   UnitExpr frame unit '[n] R
-- constant1D = gconstant @frame @unit @'[n]

-- constant2D ::
--   forall frame unit m n.
--   (KnownNat m, KnownNat n) =>
--   FrameShapeMatched frame '[m, n] =>
--   Double ->
--   UnitExpr frame unit '[m, n] R
-- constant2D = gconstant @frame @unit @'[m, n]

-- (+) ::
--   SameFrame frame1 frame2 =>
--   SameUnit unit1 unit2 =>
--   UnitExpr frame1 unit1 shape et ->
--   UnitExpr frame2 unit2 shape et ->
--   UnitExpr frame1 unit1 shape et
-- (+) = binary (P.+)

-- (*) ::
--   SameFrame frame1 frame2 =>
--   UnitExpr frame1 unit1 shape et ->
--   UnitExpr frame2 unit2 shape et ->
--   UnitExpr frame1 (unit1 U.* unit2) shape et
-- (*) = binary (P.*)

-- (/) ::
--   SameFrame frame1 frame2 =>
--   UnitExpr frame1 unit1 shape et ->
--   UnitExpr frame2 unit2 shape et ->
--   UnitExpr frame1 (unit1 U./ unit2) shape et
-- (/) = binary (P./)

-- type family DFTSampleStep (originalSampleStep :: SampleStep) (numSamples :: Nat) :: SampleStep where
--   DFTSampleStep (a :/: b) n = b :/: (a N.* n)

-- type family IDFTSampleStep (dftSampleStep :: SampleStep) (numSamples :: Nat) :: SampleStep where
--   IDFTSampleStep (a :/: b) n = b :/: (a N.* n)

-- dft1D ::
--   forall name frameUnit sampleStep unit n.
--   UnitExpr (FRAME name NORMAL_DOMAIN '[ '(frameUnit, sampleStep)]) unit '[n] C ->
--   UnitExpr (FRAME name FREQUENCY_DOMAIN '[ '(Recip frameUnit, DFTSampleStep sampleStep n)]) unit '[n] C
-- dft1D = unary Base.ft

-- dft2D ::
--   forall name frameUnit1 sampleStep1 frameUnit2 sampleStep2 unit m n.
--   UnitExpr (FRAME name NORMAL_DOMAIN '[ '(frameUnit1, sampleStep1), '(frameUnit2, sampleStep2)]) unit '[m, n] C ->
--   UnitExpr (FRAME name FREQUENCY_DOMAIN '[ '(Recip frameUnit1, DFTSampleStep sampleStep1 m), '(Recip frameUnit1, DFTSampleStep sampleStep1 m)]) unit '[m, n] C
-- dft2D = unary Base.ft

-- dft3D ::
--   UnitExpr (FRAME name NORMAL_DOMAIN '[ '(frameUnit1, sampleStep1), '(frameUnit2, sampleStep2), '(frameUnit2, sampleStep2)]) unit '[m, n, p] C ->
--   UnitExpr (FRAME name FREQUENCY_DOMAIN '[ '(Recip frameUnit1, DFTSampleStep sampleStep1 m), '(Recip frameUnit1, DFTSampleStep sampleStep1 m), '(Recip frameUnit1, DFTSampleStep sampleStep1 m)]) unit '[m, n, p] C
-- dft3D = unary Base.ft

-- --------------------------------------------------------------------------------

-- -- | INTERNAL USE ONLY !
-- unary :: (ExprBuilder -> ExprBuilder) -> UnitExpr frame1 unit1 shape1 et1 -> UnitExpr frame2 unit2 shape2 et2
-- unary f (UnitExpr e) = UnitExpr $ f e

-- binary ::
--   (ExprBuilder -> ExprBuilder -> ExprBuilder) ->
--   UnitExpr frame1 unit1 shape1 et1 ->
--   UnitExpr frame2 unit2 shape2 et2 ->
--   UnitExpr frame3 unit3 shape3 et3
-- binary f (UnitExpr e1) (UnitExpr e2) = UnitExpr $ f e1 e2

-- --------------------------------------------------------------------------------