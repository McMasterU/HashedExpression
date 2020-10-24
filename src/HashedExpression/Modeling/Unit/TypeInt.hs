{-# LANGUAGE TypeOperators #-}

module HashedExpression.Modeling.Unit.TypeInt where

import GHC.TypeNats (Nat)
import qualified GHC.TypeNats as N

type family IfThenElse (c :: Bool) (a :: k) (b :: k) where
  IfThenElse True a _ = a
  IfThenElse False _ b = b

data TypeInt = Positive Nat | Negative Nat

type family Normalize (x :: TypeInt) where
  Normalize (Positive 0) = Positive 0
  Normalize (Negative 0) = Positive 0
  Normalize x = x

type family (|+|) (x :: TypeInt) (y :: TypeInt) :: TypeInt where
  (Positive x) |+| (Positive y) = Normalize (Positive (x N.+ y))
  (Negative x) |+| (Negative y) = Normalize (Negative (x N.+ y))
  (Positive x) |+| (Negative y) = Normalize (IfThenElse (x N.<=? y) (Negative (y N.- x)) (Positive (x N.- y)))
  (Negative x) |+| (Positive y) = Normalize (IfThenElse (x N.<=? y) (Negative (y N.- x)) (Positive (x N.- y)))

type family (|-|) (x :: TypeInt) (y :: TypeInt) :: TypeInt where
  (|-|) i (Positive y) = i |+| (Negative y)
  (|-|) i (Negative y) = i |+| (Positive y)

type family (|*|) (x :: TypeInt) (y :: TypeInt) :: TypeInt where
  (Positive x) |*| (Positive y) = Normalize (Positive (x N.* y))
  (Negative x) |*| (Negative y) = Normalize (Positive (x N.* y))
  (Positive x) |*| (Negative y) = Normalize (Negative (x N.* y))
  (Negative x) |*| (Positive y) = Normalize (Negative (x N.* y))
