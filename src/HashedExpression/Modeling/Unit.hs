{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

module HashedExpression.Modeling.Unit where

import Data.Kind
import GHC.TypeLits (KnownNat, Nat, Symbol, type (+))
import qualified GHC.TypeNats as N
import HashedExpression.Internal.Base (ElementType (..), IsExpression (..), IsScalarReal (..), Op (..))
import qualified HashedExpression.Internal.Base as Base
import HashedExpression.Internal.Builder (ExprBuilder, buildExpr)
import HashedExpression.Internal.MonadExpression
import HashedExpression.Modeling.Typed (IsShape (..), Scalar)
import HashedExpression.Modeling.Unit.SI
import HashedExpression.Modeling.Unit.TypeInt (TypeInt(..))
import Prelude hiding ((*), (+), (/))
import qualified Prelude as P

--------------------------------------------------------------------------------
data DomainType = NormalDomain | FrequencyDomain

type NORMAL_DOMAIN = 'NormalDomain

type FREQUENCY_DOMAIN = 'FrequencyDomain

--------------------------------------------------------------------------------

data SampleStep = Nat :/: Nat

type family SameStep (x :: SampleStep) (y :: SampleStep) :: Constraint where
  SameStep (a :/: b) (c :/: d) = (a N.* d) ~ (b N.* c)

type family SameUnitStep (x :: (Unit, SampleStep)) (y :: (Unit, SampleStep)) :: Constraint where
  SameUnitStep '(unit1, step1) '(unit2, step2) = (ToReadable unit1 ~ ToReadable unit2, SameStep step1 step2)

type family SameUnitSteps (x :: [(Unit, SampleStep)]) (y :: [(Unit, SampleStep)]) :: Constraint where
  SameUnitSteps '[] '[] = (1 ~ 1)
  SameUnitSteps (unitStep1 ': rest1) (unitStep2 ': rest2) = (SameUnitStep unitStep1 unitStep2, SameUnitSteps rest1 rest2)

--------------------------------------------------------------------------------
data Frame = Frameless | Frame Symbol DomainType [(Unit, SampleStep)]

type FRAME = 'Frame

type FRAMELESS = 'Frameless

type family SameFrame (f1 :: Frame) (f2 :: Frame) :: Constraint where
  SameFrame ( 'Frame name1 domainType1 unitSteps1) ( 'Frame name2 domainType2 unitSteps2) =
    (name1 ~ name2, domainType1 ~ domainType2, SameUnitSteps unitSteps1 unitSteps2)
  SameFrame x y = (x ~ y)

--------------------------------------------------------------------------------
type family ListLength (xs :: [a]) :: Nat where
  ListLength '[] = 0
  ListLength (_ ': xs) = 1 + ListLength xs

type family FrameDimensionLength (frame :: Frame) :: Nat where
  FrameDimensionLength FRAMELESS = 0
  FrameDimensionLength (FRAME _ _ frameUnits) = ListLength frameUnits

data FrameDimensionOfLength = FrameDimensionOfLength Nat

type FrameShapeMatched frame shape = FrameDimensionLength frame ~ ListLength shape

--------------------------------------------------------------------------------
data
  UnitExpr
    (frame :: Frame)
    (unit :: Unit)
    (shape :: [Nat])
    (et :: ElementType)
  = UnitExpr ExprBuilder

instance IsExpression (UnitExpr frame unit shape et) where
  asRawExpr (UnitExpr exprBuilder) = buildExpr exprBuilder

instance IsScalarReal (UnitExpr frame unit Scalar R) where
  asScalarRealRawExpr = asRawExpr

gvariable ::
  forall frame unit shape.
  (IsShape shape, FrameShapeMatched frame shape) =>
  String ->
  UnitExpr frame unit shape R
gvariable name = UnitExpr $ introduceNode (toShape @shape, R, Var name)

gparam ::
  forall frame unit shape.
  (IsShape shape, FrameShapeMatched frame shape) =>
  String ->
  UnitExpr frame unit shape R
gparam name = UnitExpr $ introduceNode (toShape @shape, R, Param name)

gconstant ::
  forall frame unit shape.
  (IsShape shape, FrameShapeMatched frame shape) =>
  Double ->
  UnitExpr frame unit shape R
gconstant val = UnitExpr $ introduceNode (toShape @shape, R, Const val)

constant :: forall unit. Double -> UnitExpr FRAMELESS unit Scalar R
constant = gconstant @FRAMELESS @unit @Scalar

constant1D ::
  forall frame unit n.
  (KnownNat n) =>
  FrameShapeMatched frame '[n] =>
  Double ->
  UnitExpr frame unit '[n] R
constant1D = gconstant @frame @unit @'[n]

constant2D ::
  forall frame unit m n.
  (KnownNat m, KnownNat n) =>
  FrameShapeMatched frame '[m, n] =>
  Double ->
  UnitExpr frame unit '[m, n] R
constant2D = gconstant @frame @unit @'[m, n]

(+) ::
  SameFrame frame1 frame2 =>
  ToReadable unit1 ~ ToReadable unit2 =>
  UnitExpr frame1 unit1 shape et ->
  UnitExpr frame2 unit2 shape et ->
  UnitExpr frame1 unit1 shape et
(+) = binary (P.+)

(*) ::
  SameFrame frame1 frame2 =>
  UnitExpr frame1 unit1 shape et ->
  UnitExpr frame2 unit2 shape et ->
  UnitExpr frame1 (unit1 :* unit2) shape et
(*) = binary (P.*)

(/) ::
  SameFrame frame1 frame2 =>
  UnitExpr frame1 unit1 shape et ->
  UnitExpr frame2 unit2 shape et ->
  UnitExpr frame1 (unit1 :/ unit2) shape et
(/) = binary (P./)

type family DFTSampleStep (originalSampleStep :: SampleStep) (numSamples :: Nat) :: SampleStep where
  DFTSampleStep (a :/: b) n = b :/: (a N.* n)

type family IDFTSampleStep (dftSampleStep :: SampleStep) (numSamples :: Nat) :: SampleStep where
  IDFTSampleStep (a :/: b) n = b :/: (a N.* n)

dft1D ::
  forall name frameUnit sampleStep unit n.
  UnitExpr (FRAME name NORMAL_DOMAIN '[ '(frameUnit, sampleStep)]) unit '[n] R ->
  UnitExpr (FRAME name FREQUENCY_DOMAIN '[ '(Recip frameUnit, DFTSampleStep sampleStep n)]) unit '[n] C
dft1D = unary Base.ft

dft2D ::
  forall name frameUnit1 sampleStep1 frameUnit2 sampleStep2 unit m n.
  UnitExpr (FRAME name NORMAL_DOMAIN '[ '(frameUnit1, sampleStep1), '(frameUnit2, sampleStep2)]) unit '[m, n] C ->
  UnitExpr (FRAME name FREQUENCY_DOMAIN '[ '(Recip frameUnit1, DFTSampleStep sampleStep1 m), '(Recip frameUnit1, DFTSampleStep sampleStep1 m)]) unit '[m, n] C
dft2D = unary Base.ft

dft3D ::
  UnitExpr (FRAME name NORMAL_DOMAIN '[ '(frameUnit1, sampleStep1), '(frameUnit2, sampleStep2), '(frameUnit2, sampleStep2)]) unit '[m, n, p] C ->
  UnitExpr (FRAME name FREQUENCY_DOMAIN '[ '(Recip frameUnit1, DFTSampleStep sampleStep1 m), '(Recip frameUnit1, DFTSampleStep sampleStep1 m), '(Recip frameUnit1, DFTSampleStep sampleStep1 m)]) unit '[m, n, p] C
dft3D = unary Base.ft

--------------------------------------------------------------------------------

-- | INTERNAL USE ONLY !
unary :: (ExprBuilder -> ExprBuilder) -> UnitExpr frame1 unit1 shape1 et1 -> UnitExpr frame2 unit2 shape2 et2
unary f (UnitExpr e) = UnitExpr $ f e

binary ::
  (ExprBuilder -> ExprBuilder -> ExprBuilder) ->
  UnitExpr frame1 unit1 shape1 et1 ->
  UnitExpr frame2 unit2 shape2 et2 ->
  UnitExpr frame3 unit3 shape3 et3
binary f (UnitExpr e1) (UnitExpr e2) = UnitExpr $ f e1 e2

--------------------------------------------------------------------------------
