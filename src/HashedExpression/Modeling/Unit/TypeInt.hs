module HashedExpression.Modeling.Unit.TypeInt where

import Data.Type.Equality (type (==))
import GHC.TypeLits (ErrorMessage (..))
import GHC.TypeNats (Nat)
import qualified GHC.TypeNats as N
import HashedExpression.Modeling.Unit.Common

data TypeInt = Positive Nat | Negative Nat

type family Normalize (x :: TypeInt) where
  Normalize (Positive 0) = Positive 0
  Normalize (Negative 0) = Positive 0
  Normalize x = x

type family EqTypeInt (x :: TypeInt) (y :: TypeInt) :: Bool where
  EqTypeInt (Positive 0) (Positive 0) = True
  EqTypeInt (Positive 0) (Negative 0) = True
  EqTypeInt (Negative 0) (Negative 0) = True
  EqTypeInt (Negative 0) (Positive 0) = True
  EqTypeInt (Positive x) (Positive y) = x == y
  EqTypeInt (Negative x) (Negative y) = x == y
  EqTypeInt _ _ = False

type family PrintTypeInt (x :: TypeInt) where
  PrintTypeInt (Positive a) = ShowType a
  PrintTypeInt (Negative a) = Text "-" :<>: ShowType a

infixl 7 *

infixl 9 +, -

type family (+) (x :: TypeInt) (y :: TypeInt) :: TypeInt where
  (Positive x) + (Positive y) = Normalize (Positive (x N.+ y))
  (Negative x) + (Negative y) = Normalize (Negative (x N.+ y))
  (Positive x) + (Negative y) = Normalize (IfThenElse (x N.<=? y) (Negative (y N.- x)) (Positive (x N.- y)))
  (Negative x) + (Positive y) = Normalize (IfThenElse (x N.<=? y) (Negative (y N.- x)) (Positive (x N.- y)))

type family (-) (x :: TypeInt) (y :: TypeInt) :: TypeInt where
  (-) i (Positive y) = i + (Negative y)
  (-) i (Negative y) = i + (Positive y)

type family (*) (x :: TypeInt) (y :: TypeInt) :: TypeInt where
  (Positive x) * (Positive y) = Normalize (Positive (x N.* y))
  (Negative x) * (Negative y) = Normalize (Positive (x N.* y))
  (Positive x) * (Negative y) = Normalize (Negative (x N.* y))
  (Negative x) * (Positive y) = Normalize (Negative (x N.* y))
