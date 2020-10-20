{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

module HashedExpression.Modeling.Unit where

import GHC.TypeLits (KnownNat, Nat, Symbol, type (+))
import HashedExpression.Internal.Base (ElementType (..), IsExpression (..), IsScalarReal (..), Op (..))
import qualified HashedExpression.Internal.Base as Base
import HashedExpression.Internal.Builder (ExprBuilder, buildExpr)
import HashedExpression.Internal.MonadExpression
import HashedExpression.Modeling.Typed (IsShape (..), Scalar)
import HashedExpression.Modeling.Unit.Types
import Prelude hiding ((*), (+), (/))
import qualified Prelude as P

--------------------------------------------------------------------------------
data DomainType = NormalDomain | FrequencyDomain

type NORMAL_DOMAIN = 'NormalDomain

type FREQUENCY_DOMAIN = 'FrequencyDomain

--------------------------------------------------------------------------------
data Frame = Frameless | Frame Symbol DomainType [Unit]

type FRAME = 'Frame

type FRAMELESS = 'Frameless

type FRAME_1D name unit = FRAME name NORMAL_DOMAIN '[unit]

type FRAME_2D name unit1 unit2 = FRAME name NORMAL_DOMAIN '[unit1, unit2]

type FRAME_3D name unit1 unit2 unit3 = FRAME name NORMAL_DOMAIN '[unit1, unit2, unit3]

--------------------------------------------------------------------------------
type family ListLength (xs :: [a]) :: Nat where
  ListLength '[] = 0
  ListLength (_ ': xs) = 1 + ListLength xs

type family FrameDimensionLength (frame :: Frame) :: Nat where
  FrameDimensionLength FRAMELESS = 0
  FrameDimensionLength (FRAME _ _ frameUnits) = ListLength frameUnits

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

constant1D :: forall frame unit n. (KnownNat n) => FrameShapeMatched frame '[n] => Double -> UnitExpr frame unit '[n] R
constant1D = gconstant @frame @unit @'[n]

constant2D ::
  forall frame unit m n.
  (KnownNat m, KnownNat n) =>
  FrameShapeMatched frame '[m, n] =>
  Double ->
  UnitExpr frame unit '[m, n] R
constant2D = gconstant @frame @unit @'[m, n]

(+) :: UnitExpr frame unit shape et -> UnitExpr frame unit shape et -> UnitExpr frame unit shape et
(+) = binary (P.+)

(*) :: UnitExpr frame unit1 shape et -> UnitExpr frame unit2 shape et -> UnitExpr frame (unit1 |*| unit2) shape et
(*) = binary (P.*)

(/) :: UnitExpr frame unit1 shape et -> UnitExpr frame unit2 shape et -> UnitExpr frame (unit1 |/| unit2) shape et
(/) = binary (P./)

dft1D ::
  UnitExpr (FRAME name NORMAL_DOMAIN '[frameUnit]) unit shape C ->
  UnitExpr (FRAME name FREQUENCY_DOMAIN '[Recip frameUnit]) unit shape C
dft1D = unary Base.ft

dft2D ::
  UnitExpr (FRAME name NORMAL_DOMAIN '[frameUnit1, frameUnit2]) unit '[m, n] C ->
  UnitExpr (FRAME name FREQUENCY_DOMAIN '[Recip frameUnit, Recip frameUnit2]) unit '[m, n] C
dft2D = unary Base.ft

dft3D ::
  UnitExpr (FRAME name NORMAL_DOMAIN '[frameUnit1, frameUnit2, frameUnit3]) unit '[m, n, p] C ->
  UnitExpr (FRAME name FREQUENCY_DOMAIN '[Recip frameUnit, Recip frameUnit2, Recip frameUnit3]) unit '[m, n, p] C
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
