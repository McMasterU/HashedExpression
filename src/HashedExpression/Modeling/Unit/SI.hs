{-# LANGUAGE TypeOperators #-}

module HashedExpression.Modeling.Unit.SI where

import Data.Kind (Constraint)
import GHC.TypeLits (ErrorMessage (..))
import HashedExpression.Modeling.Unit.Common
import HashedExpression.Modeling.Unit.TypeInt (EqTypeInt, TypeInt (..))
import qualified HashedExpression.Modeling.Unit.TypeInt as I

newtype UMeter = M TypeInt

type family PrintMeter (m :: UMeter) where
  PrintMeter (M (Positive 0)) = Text ""
  PrintMeter (M (Negative 0)) = Text ""
  PrintMeter (M (Positive a)) = Text "m^" :<>: ShowType a
  PrintMeter (M (Negative a)) = Text "m^(-" :<>: ShowType a :<>: Text ")"

newtype UKilogram = Kg TypeInt

type family PrintKilogram (m :: UKilogram) where
  PrintKilogram (Kg (Positive 0)) = Text ""
  PrintKilogram (Kg (Negative 0)) = Text ""
  PrintKilogram (Kg (Positive a)) = Text "kg^" :<>: ShowType a
  PrintKilogram (Kg (Negative a)) = Text "kg^(-" :<>: ShowType a :<>: Text ")"

newtype USecond = S TypeInt

type family PrintSecond (m :: USecond) where
  PrintSecond (S (Positive 0)) = Text ""
  PrintSecond (S (Negative 0)) = Text ""
  PrintSecond (S (Positive a)) = Text "s^" :<>: ShowType a
  PrintSecond (S (Negative a)) = Text "s^(-" :<>: ShowType a :<>: Text ")"

newtype UAmpere = A TypeInt

type family PrintAmpere (m :: UAmpere) where
  PrintAmpere (A (Positive 0)) = Text ""
  PrintAmpere (A (Negative 0)) = Text ""
  PrintAmpere (A (Positive a)) = Text "A^" :<>: ShowType a
  PrintAmpere (A (Negative a)) = Text "A^(-" :<>: ShowType a :<>: Text ")"

newtype UKevin = K TypeInt

type family PrintKevin (m :: UKevin) where
  PrintKevin (K (Positive 0)) = Text ""
  PrintKevin (K (Negative 0)) = Text ""
  PrintKevin (K (Positive a)) = Text "K^" :<>: ShowType a
  PrintKevin (K (Negative a)) = Text "K^(-" :<>: ShowType a :<>: Text ")"

newtype UMole = Mol TypeInt

type family PrintMole (m :: UMole) where
  PrintMole (Mol (Positive 0)) = Text ""
  PrintMole (Mol (Negative 0)) = Text ""
  PrintMole (Mol (Positive a)) = Text "mol^" :<>: ShowType a
  PrintMole (Mol (Negative a)) = Text "mol^(-" :<>: ShowType a :<>: Text ")"

newtype UCandela = Cd TypeInt

type family PrintCandela (m :: UCandela) where
  PrintCandela (Cd (Positive 0)) = Text ""
  PrintCandela (Cd (Negative 0)) = Text ""
  PrintCandela (Cd (Positive a)) = Text "Cd^" :<>: ShowType a
  PrintCandela (Cd (Negative a)) = Text "Cd^(-" :<>: ShowType a :<>: Text ")"

data Unit
  = Unit
      UMeter -- (length, m - meter)
      UKilogram -- (mass, kg - kilogram)
      USecond -- (time, s - second)
      UAmpere -- (electric current, A - ampere)
      UKevin -- (thermodynamic temperature, K - kevin)
      UMole -- (amount of substance, Mol - mole)
      UCandela -- (luminous intensity, cd - candela)

type family PrintUnit (m :: Unit) where
  PrintUnit Unitless = Text "unitless"
  PrintUnit ( 'Unit m kg s a k mol cd) =
    PrintMeter m
      :<>: Text "*"
      :<>: PrintKilogram kg
      :<>: Text "*"
      :<>: PrintSecond s
      :<>: Text "*"
      :<>: PrintAmpere a
      :<>: Text "*"
      :<>: PrintKevin k
      :<>: Text "*"
      :<>: PrintMole mol
      :<>: Text "*"
      :<>: PrintCandela cd

type family EqUnit (unit1 :: Unit) (unit2 :: Unit) :: Bool where
  EqUnit (UNIT ( 'M l) ( 'Kg m) ( 'S t) ( 'A i) ( 'K th) ( 'Mol n) ( 'Cd j)) (UNIT ( 'M l') ( 'Kg m') ( 'S t') ( 'A i') ( 'K th') ( 'Mol n') ( 'Cd j')) =
    (EqTypeInt l l')
      `And` (EqTypeInt m m')
      `And` (EqTypeInt t t')
      `And` (EqTypeInt i i')
      `And` (EqTypeInt th th')
      `And` (EqTypeInt n n')
      `And` (EqTypeInt j j')

type family SameUnit (unit1 :: Unit) (unit2 :: Unit) :: Constraint where
  SameUnit u1 u2 =
    Unless
      (EqUnit u1 u2)
      (Text "Unit mismatch: " :<>: PrintUnit u1 :<>: Text " vs " :<>: PrintUnit u2)

type Unitless =
  'Unit
    ( 'M (Positive 0))
    ( 'Kg (Positive 0))
    ( 'S (Positive 0))
    ( 'A (Positive 0))
    ( 'K (Positive 0))
    ( 'Mol (Positive 0))
    ( 'Cd (Positive 0))

type UNIT = 'Unit

--------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}

infixr 8 ^

infixl 7 :*:, /

type family (a :: Unit) :*: (b :: Unit) :: Unit where
  Unitless :*: d = d
  d :*: Unitless = d
  (UNIT ( 'M l) ( 'Kg m) ( 'S t) ( 'A i) ( 'K th) ( 'Mol n) ( 'Cd j)) :*: (UNIT ( 'M l') ( 'Kg m') ( 'S t') ( 'A i') ( 'K th') ( 'Mol n') ( 'Cd j')) =
    UNIT
      ( 'M (l I.* l'))
      ( 'Kg (m I.* m'))
      ( 'S (t I.* t'))
      ( 'A (i I.* i'))
      ( 'K (th I.* th'))
      ( 'Mol (n I.* n'))
      ( 'Cd (j I.* j'))

type family (a :: Unit) / (b :: Unit) :: Unit where
  d / Unitless = d
  d / d = Unitless
  (UNIT ( 'M l) ( 'Kg m) ( 'S t) ( 'A i) ( 'K th) ( 'Mol n) ( 'Cd j)) / (UNIT ( 'M l') ( 'Kg m') ( 'S t') ( 'A i') ( 'K th') ( 'Mol n') ( 'Cd j')) =
    UNIT
      ( 'M (l I.- l'))
      ( 'Kg (m I.- m'))
      ( 'S (t I.- t'))
      ( 'A (i I.- i'))
      ( 'K (th I.- th'))
      ( 'Mol (n I.- n'))
      ( 'Cd (j I.- j'))

type Recip (d :: Unit) = Unitless / d

type family (d :: Unit) ^ (x :: TypeInt) :: Unit where
  Unitless ^ _ = Unitless
  _ ^ (Positive 0) = Unitless
  _ ^ (Negative 0) = Unitless
  d ^ (Positive 1) = d
  (UNIT ( 'M l) ( 'Kg m) ( 'S t) ( 'A i) ( 'K th) ( 'Mol n) ( 'Cd j)) ^ x =
    UNIT
      ( 'M (l I.* x))
      ( 'Kg (m I.* x))
      ( 'S (t I.* x))
      ( 'A (i I.* x))
      ( 'K (th I.* x))
      ( 'Mol (n I.* x))
      ( 'Cd (j I.* x))

{- ORMOLU_ENABLE -}

type Zero = Positive 0

--------------------------------------------------------------------------------
type Meter = 'Unit ( 'M (Positive 1)) ( 'Kg Zero) ( 'S Zero) ( 'A Zero) ( 'K Zero) ( 'Mol Zero) ( 'Cd Zero)

type Metre = Meter

type Kilogram = 'Unit ( 'M Zero) ( 'Kg (Positive 1)) ( 'S Zero) ( 'A Zero) ( 'K Zero) ( 'Mol Zero) ( 'Cd Zero)

type Second = 'Unit ( 'M Zero) ( 'Kg Zero) ( 'S (Positive 1)) ( 'A Zero) ( 'K Zero) ( 'Mol Zero) ( 'Cd Zero)

type Ampere = 'Unit ( 'M Zero) ( 'Kg Zero) ( 'S Zero) ( 'A (Positive 1)) ( 'K Zero) ( 'Mol Zero) ( 'Cd Zero)

type Kevin = 'Unit ( 'M Zero) ( 'Kg Zero) ( 'S Zero) ( 'A Zero) ( 'K (Positive 1)) ( 'Mol Zero) ( 'Cd Zero)

type Mole = 'Unit ( 'M Zero) ( 'Kg Zero) ( 'S Zero) ( 'A Zero) ( 'K Zero) ( 'Mol (Positive 1)) ( 'Cd Zero)

type Candela = 'Unit ( 'M Zero) ( 'Kg Zero) ( 'S Zero) ( 'A Zero) ( 'K Zero) ( 'Mol Zero) ( 'Cd (Positive 1))
