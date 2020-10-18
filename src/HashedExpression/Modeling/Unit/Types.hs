{-# LANGUAGE TypeOperators #-}

module HashedExpression.Modeling.Unit.Types where

import GHC.TypeLits (Nat)
import Numeric.NumType.DK.Integers (TypeInt (..), type (+), type (-))
import qualified Numeric.NumType.DK.Integers as I

type family NEG (n :: Nat) :: TypeInt where
  NEG 0 = Zero
  NEG 1 = Neg1
  NEG 2 = Neg2
  NEG 3 = Neg3
  NEG 4 = Neg4
  NEG 5 = Neg5
  NEG 6 = Neg6
  NEG 7 = Neg7
  NEG 8 = Neg8
  NEG 9 = Neg9
  NEG n = Neg10Minus n

type family POS (n :: Nat) :: TypeInt where
  POS 0 = Zero
  POS 1 = Pos1
  POS 2 = Pos2
  POS 3 = Pos3
  POS 4 = Pos4
  POS 5 = Pos5
  POS 6 = Pos6
  POS 7 = Pos7
  POS 8 = Pos8
  POS 9 = Pos9
  POS n = Pos10Plus n

data Unit
  = Unit
      TypeInt -- (length, m - meter)
      TypeInt -- (mass, kg - kilogram)
      TypeInt -- (time, s - second)
      TypeInt -- (electric current, A - ampere)
      TypeInt -- (thermodynamic temperature, K - Kevin)
      TypeInt -- (amount of substance, mol - mole)
      TypeInt -- (luminous intensity, cd - candela)

type Unitless = 'Unit Zero Zero Zero Zero Zero Zero Zero

--------------------------------------------------------------------------------
infixr 8 |^|

infixl 7 |*|, |/|

type family (a :: Unit) |*| (b :: Unit) :: Unit where
  Unitless |*| d = d
  d |*| Unitless = d
  ('Unit l m t i th n j) |*| ('Unit l' m' t' i' th' n' j') =
    'Unit (l + l') (m + m') (t + t') (i + i') (th + th') (n + n') (j + j')

type family (a :: Unit) |/| (b :: Unit) :: Unit where
  d |/| Unitless = d
  d |/| d = Unitless
  ('Unit l m t i th n j) |/| ('Unit l' m' t' i' th' n' j') =
    'Unit (l - l') (m - m') (t - t') (i - i') (th - th') (n - n') (j - j')

type Recip (d :: Unit) = Unitless |/| d

type family (d :: Unit) |^| (x :: TypeInt) where
  Unitless |^| _ = Unitless
  _ |^| 'Zero = Unitless
  d |^| 'Pos1 = d
  ('Unit l m t i th n j) |^| x =
    'Unit (l I.* x) (m I.* x) (t I.* x) (i I.* x) (th I.* x) (n I.* x) (j I.* x)

--------------------------------------------------------------------------------
type Meter = 'Unit Pos1 Zero Zero Zero Zero Zero Zero

--------------------------------------------------------------------------------
type Kilogram = 'Unit Zero Pos1 Zero Zero Zero Zero Zero

--------------------------------------------------------------------------------
type Second = 'Unit Zero Zero Pos1 Zero Zero Zero Zero

--------------------------------------------------------------------------------
type Ampere = 'Unit Zero Zero Zero Pos1 Zero Zero Zero

--------------------------------------------------------------------------------
type Kevin = 'Unit Zero Zero Zero Zero Pos1 Zero Zero

--------------------------------------------------------------------------------
type Mole = 'Unit Zero Zero Zero Zero Zero Pos1 Zero

--------------------------------------------------------------------------------
type Candela = 'Unit Zero Zero Zero Zero Zero Zero Pos1
