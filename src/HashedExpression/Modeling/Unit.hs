{-# LANGUAGE TypeOperators #-}

module HashedExpression.Modeling.Unit where

import GHC.TypeLits (KnownNat, Nat, Symbol, type (+))
import HashedExpression.Internal.Base (ElementType (..), IsExpression (..), IsScalarReal (..))
import qualified HashedExpression.Internal.Base as Base
import HashedExpression.Internal.Builder (ExprBuilder, buildExpr)
import HashedExpression.Modeling.Typed (D1, Scalar)
import HashedExpression.Modeling.Unit.Types

--------------------------------------------------------------------------------
data DomainType = NormalDomain | FrequencyDomain

type NORMAL_DOMAIN = 'NormalDomain

type FREQUENCY_DOMAIN = 'FrequencyDomain

--------------------------------------------------------------------------------
data Frame = Frame Symbol DomainType

type FRAME = 'Frame

--------------------------------------------------------------------------------
type family ListLength (xs :: [a]) :: Nat where
  ListLength '[] = 0
  ListLength (_ ': xs) = 1 + ListLength xs

--------------------------------------------------------------------------------
data
  UnitExpr
    (frame :: Frame)
    (domainUnits :: [Unit])
    (unit :: Unit)
    (shape :: [Nat])
    (et :: ElementType)
  = ListLength domainUnits ~ ListLength shape => UnitExpr ExprBuilder

instance IsExpression (UnitExpr frame domainUnit unit shape et) where
  asRawExpr (UnitExpr exprBuilder) = buildExpr exprBuilder

instance IsScalarReal (UnitExpr frame domainUnit unit Scalar R) where
  asScalarRealRawExpr = asRawExpr

constant :: Double -> UnitExpr frame domainUnits unit Scalar R
constant = undefined

constant1D :: (KnownNat n) => Double -> UnitExpr frame domainUnits unit (D1 n) R
constant1D = undefined

dft1D ::
  UnitExpr (FRAME name NORMAL_DOMAIN) '[domainUnit] unit shape C ->
  UnitExpr (FRAME name FREQUENCY_DOMAIN) '[Recip domainUnit] unit shape C
dft1D = undefined

dft2D ::
  UnitExpr (FRAME name NORMAL_DOMAIN) '[domainUnit1, domainUnit2] unit '[m, n] C ->
  UnitExpr (FRAME name FREQUENCY_DOMAIN) '[Recip domainUnit, Recip domainUnit2] unit '[m, n] C
dft2D = undefined

dft3D ::
  UnitExpr (FRAME name NORMAL_DOMAIN) '[domainUnit1, domainUnit2, domainUnit3] unit '[m, n, p] C ->
  UnitExpr (FRAME name FREQUENCY_DOMAIN) '[Recip domainUnit, Recip domainUnit2, Recip domainUnit3] unit '[m, n, p] C
dft3D = undefined
