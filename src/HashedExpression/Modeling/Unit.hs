{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

module HashedExpression.Modeling.Unit where

import Data.Kind
import Data.Type.Equality (type (==))
import GHC.TypeLits (ErrorMessage (..), KnownNat, Nat, Symbol, TypeError, type (+))
import qualified GHC.TypeNats as N
import HashedExpression.Internal.Base (ElementType (..), IsExpression (..), IsScalarReal (..), Op (..))
import qualified HashedExpression.Internal.Base as Base
import HashedExpression.Internal.Builder (ExprBuilder, buildExpr)
import HashedExpression.Internal.MonadExpression
import HashedExpression.Modeling.Typed (Scalar, ToShape (..), nat)
import HashedExpression.Modeling.Unit.Common
import HashedExpression.Modeling.Unit.SI (Candela, EqUnit, Kilogram, Meter, PrintUnit, PrintUnits, Recip, SameUnit, Unit)
import qualified HashedExpression.Modeling.Unit.SI as U
import HashedExpression.Modeling.Unit.TypeInt (TypeInt (..))
import Prelude hiding ((*), (+), (/))
import qualified Prelude as P

--------------------------------------------------------------------------------

data SampleStep = Nat :/: Nat

type family a :/ b where
  a :/ 0 = TypeError (Text "Division to zero: " :<>: ShowType a :<>: Text "/0")
  0 :/ b = TypeError (Text "Sample step must be positive: " :<>: Text "0/" :<>: ShowType b)
  a :/ b = (a `N.Div` (GCD a b)) :/: (b `N.Div` (GCD a b))

type family EqSampleStep (x :: SampleStep) (y :: SampleStep) :: Bool where
  EqSampleStep (a :/: b) (c :/: d) = a N.* d == c N.* b

type family PrintSampleStep (x :: SampleStep) :: ErrorMessage where
  PrintSampleStep (a :/: b) = ShowType a :<>: Text "/" :<>: ShowType b

type family PrintSampleSteps (x :: [SampleStep]) :: ErrorMessage where
  PrintSampleSteps '[] = Text ""
  PrintSampleSteps '[s] = PrintSampleStep s
  PrintSampleSteps (s ': ss) = PrintSampleStep s :<>: Text ", " :<>: PrintSampleSteps ss

type family SameSampleStep (x :: SampleStep) (y :: SampleStep) :: Constraint where
  SameSampleStep x y =
    Unless
      (EqSampleStep x y)
      ( Text "Sample step mismatch"
          :$$: (Text "Expected: " :<>: PrintSampleStep x)
          :$$: (Text "  Actual: " :<>: PrintSampleStep y)
      )

--------------------------------------------------------------------------------

-- type family SameUnitSampleStep (x :: (Unit, SampleStep)) (y :: (Unit, SampleStep)) :: Constraint where
--   SameUnitSampleStep '(unit1, step1) '(unit2, step2) =
--     (SameUnit unit1 unit2, SameSampleStep step1 step2)

-- type family SameUnitSampleSteps (x :: [(Unit, SampleStep)]) (y :: [(Unit, SampleStep)]) :: Constraint where
--   SameUnitSampleSteps '[] '[] = Satisfied
--   SameUnitSampleSteps (unitStep1 ': rest1) (unitStep2 ': rest2) =
--     (SameUnitSampleStep unitStep1 unitStep2, SameUnitSampleSteps rest1 rest2)

--------------------------------------------------------------------------------
-- data [Dimension] = [Dimension] [(Unit, SampleStep)]

-- type FRAME = '[Dimension]

-- type family SameFrame (f1 :: [Dimension]) (f2 :: [Dimension]) :: Constraint where
--   SameFrame ( '[Dimension] unitSteps1) ( '[Dimension] unitSteps2) =
--     (SameUnitSampleSteps unitSteps1 unitSteps2)

-- type family FrameDimensionLength (ds :: [Dimension]) :: Nat where
--   FrameDimensionLength (FRAME frameUnits) = ListLength frameUnits

-- data FrameDimensionOfLength = FrameDimensionOfLength Nat

-- type FrameShapeMatched ds shape = FrameDimensionLength ds ~ ListLength shape

--------------------------------------------------------------------------------

type DomainUnit = Unit

type NumSamples = Nat

data Dimension = Dimension NumSamples SampleStep DomainUnit

type RangeUnit = Unit

type family NumsSamplesDimensions (ds :: [Dimension]) :: [NumSamples] where
  NumsSamplesDimensions '[] = '[]
  NumsSamplesDimensions ((D n _ _) ': ds) = n ': NumsSamplesDimensions ds

type family SampleStepsDimensions (ds :: [Dimension]) :: [SampleStep] where
  SampleStepsDimensions '[] = '[]
  SampleStepsDimensions ((D _ ss _) ': ds) = ss ': SampleStepsDimensions ds

type family DomainUnitsDimensions (ds :: [Dimension]) :: [DomainUnit] where
  DomainUnitsDimensions '[] = '[]
  DomainUnitsDimensions ((D _ _ u) ': ds) = u ': DomainUnitsDimensions ds

type family EqDimension (x :: Dimension) (y :: Dimension) :: Bool where
  EqDimension (D n1 sampleStep1 domainUnit1) (D n2 sampleStep2 domainUnit2) =
    (n1 == n2)
      `And` (EqSampleStep sampleStep1 sampleStep2)
      `And` (EqUnit domainUnit1 domainUnit2)

type family PrintDimension (x :: Dimension) where
  PrintDimension (D n sampleStep domainUnit) =
    Text ""
      :$$: (Text "Number of samples: " :<>: ShowType n)
      :$$: (Text "Sample step: " :<>: PrintSampleStep sampleStep)
      :$$: (Text "Domain unit: " :<>: PrintUnit domainUnit)

type family EqDimensions (x :: [Dimension]) (y :: [Dimension]) :: Bool where
  EqDimensions '[] '[] = True
  EqDimensions _ '[] = False
  EqDimensions '[] _ = False
  EqDimensions (x ': xs) (y ': ys) = EqDimension x y `And` EqDimensions xs ys

type family PrintDimensions (xs :: [Dimension]) where
  PrintDimensions '[] = Text "Dimensionless"
  PrintDimensions ds =
    (Text "Numbers of samples: " :<>: Text "[" :<>: (PrintNats (NumsSamplesDimensions ds))) :<>: Text "]"
      :$$: (Text "Sample steps: " :<>: Text "[" :<>: (PrintSampleSteps (SampleStepsDimensions ds))) :<>: Text "]"
      :$$: (Text "Domain units: " :<>: Text "[" :<>: (PrintUnits (DomainUnitsDimensions ds))) :<>: Text "]"

type family SameDimensions (xs :: [Dimension]) (ys :: [Dimension]) :: Constraint where
  SameDimensions xs ys =
    Unless
      (EqDimensions xs ys)
      ( Text "Dimensions mismatch"
          :$$: Text "1st operand: " :<>: PrintDimensions xs
          :$$: Text "2nd operand: " :<>: PrintDimensions ys
      )

type Dimensionless = '[]

instance (KnownNat n, ToShape ds) => ToShape ((D n sampleStep domainUnit) ': ds) where
  toShape = (nat @n) : toShape @ds

type D = 'Dimension

--------------------------------------------------------------------------------
data
  UnitExpr
    (ds :: [Dimension])
    (rangeUnit :: RangeUnit)
    (et :: ElementType)
  = UnitExpr ExprBuilder

instance IsExpression (UnitExpr dimensions unit et) where
  asRawExpr (UnitExpr exprBuilder) = buildExpr exprBuilder

instance IsScalarReal (UnitExpr '[] unit R) where
  asScalarRealRawExpr = asRawExpr

--------------------------------------------------------------------------------

gvariable ::
  forall ds unit.
  ToShape ds =>
  String ->
  UnitExpr ds unit R
gvariable name = UnitExpr $ introduceNode (toShape @ds, R, Var name)

-- gparam ::
--   forall ds unit shape.
--   (ToShape shape, FrameShapeMatched ds shape) =>
--   String ->
--   UnitExpr ds unit shape R
-- gparam name = UnitExpr $ introduceNode (toShape @shape, R, Param name)

gconstant ::
  forall ds unit.
  ToShape ds =>
  Double ->
  UnitExpr ds unit R
gconstant val = UnitExpr $ introduceNode (toShape @ds, R, Const val)

constant :: forall unit. Double -> UnitExpr Dimensionless unit R
constant = gconstant @Dimensionless @unit

constant1D ::
  forall d unit.
  ToShape '[d] =>
  Double ->
  UnitExpr '[d] unit R
constant1D = gconstant @'[d] @unit

constant2D ::
  forall d1 d2 unit.
  ToShape '[d1, d2] =>
  Double ->
  UnitExpr '[d1, d2] unit R
constant2D = gconstant @'[d1, d2] @unit

-- constant2D ::
--   forall ds unit m n.
--   (KnownNat m, KnownNat n) =>
--   FrameShapeMatched ds '[m, n] =>
--   Double ->
--   UnitExpr ds unit '[m, n] R
-- constant2D = gconstant @ds @unit @'[m, n]

-- (+) ::
--   SameFrame frame1 frame2 =>
--   SameUnit unit1 unit2 =>
--   UnitExpr frame1 unit1 shape et ->
--   UnitExpr frame2 unit2 shape et ->
--   UnitExpr frame1 unit1 shape et
-- (+) = binary (P.+)

(+:) ::
  SameDimensions ds1 ds2 =>
  SameUnit unit1 unit2 =>
  UnitExpr ds1 unit1 R ->
  UnitExpr ds2 unit2 R ->
  UnitExpr ds1 unit1 C
(+:) = binary (Base.+:)

-- instance ComplexRealOp ()
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

type family DFTSampleStep (originalSampleStep :: SampleStep) (numSamples :: Nat) :: SampleStep where
  DFTSampleStep (a :/: b) n = b :/ (a N.* n)

type family IDFTSampleStep (dftSampleStep :: SampleStep) (numSamples :: Nat) :: SampleStep where
  IDFTSampleStep (a :/: b) n = b :/ (a N.* n)

type family DFTDimensions (ds :: [Dimension]) :: [Dimension] where
  DFTDimensions '[] = '[]
  DFTDimensions ((D n sampleStep domainUnit) ': ds) =
    (D n (DFTSampleStep sampleStep n) (Recip domainUnit)) ': DFTDimensions ds

type family IDFTDimensions (ds :: [Dimension]) :: [Dimension] where
  IDFTDimensions '[] = '[]
  IDFTDimensions ((D n sampleStep domainUnit) ': ds) =
    (D n (DFTSampleStep sampleStep n) (Recip domainUnit)) ': IDFTDimensions ds

dft :: forall ds rangeUnit. UnitExpr ds rangeUnit C -> UnitExpr (DFTDimensions ds) rangeUnit C
dft = unary Base.ft

idft :: forall ds rangeUnit. UnitExpr ds rangeUnit C -> UnitExpr (IDFTDimensions ds) rangeUnit C
idft = unary Base.ft

--------------------------------------------------------------------------------

-- | INTERNAL USE ONLY !
unary :: (ExprBuilder -> ExprBuilder) -> UnitExpr frame1 unit1 et1 -> UnitExpr frame2 unit2 et2
unary f (UnitExpr e) = UnitExpr $ f e

binary ::
  (ExprBuilder -> ExprBuilder -> ExprBuilder) ->
  UnitExpr frame1 unit1 et1 ->
  UnitExpr frame2 unit2 et2 ->
  UnitExpr frame3 unit3 et3
binary f (UnitExpr e1) (UnitExpr e2) = UnitExpr $ f e1 e2

--------------------------------------------------------------------------------
