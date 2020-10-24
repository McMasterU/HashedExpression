{-# LANGUAGE TypeOperators #-}

module HashedExpression.Modeling.Unit.SI where

import HashedExpression.Modeling.Unit.TypeInt

newtype UMeter = M TypeInt

newtype UKilogram = Kg TypeInt

newtype USecond = S TypeInt

newtype UAmpere = A TypeInt

newtype UKevin = K TypeInt

newtype UMole = Mol TypeInt

newtype UCandela = Cd TypeInt

data Unit
  = Unit
      UMeter -- (length, m - meter)
      UKilogram -- (mass, kg - kilogram)
      USecond -- (time, s - second)
      UAmpere -- (electric current, A - ampere)
      UKevin -- (thermodynamic temperature, K - kevin)
      UMole -- (amount of substance, Mol - mole)
      UCandela -- (luminous intensity, cd - candela)

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
infixr 8 :^

infixl 7 :*, :/

type family (a :: Unit) :* (b :: Unit) :: Unit where
  Unitless :* d = d
  d :* Unitless = d
  (UNIT ( 'M l) ( 'Kg m) ( 'S t) ( 'A i) ( 'K th) ( 'Mol n) ( 'Cd j)) :* (UNIT ( 'M l') ( 'Kg m') ( 'S t') ( 'A i') ( 'K th') ( 'Mol n') ( 'Cd j')) =
    UNIT
      ( 'M (l |+| l'))
      ( 'Kg (m |+| m'))
      ( 'S (t |+| t'))
      ( 'A (i |+| i'))
      ( 'K (th |+| th'))
      ( 'Mol (n |+| n'))
      ( 'Cd (j |+| j'))

type family (a :: Unit) :/ (b :: Unit) :: Unit where
  d :/ Unitless = d
  d :/ d = Unitless
  (UNIT ( 'M l) ( 'Kg m) ( 'S t) ( 'A i) ( 'K th) ( 'Mol n) ( 'Cd j)) :/ (UNIT ( 'M l') ( 'Kg m') ( 'S t') ( 'A i') ( 'K th') ( 'Mol n') ( 'Cd j')) =
    UNIT
      ( 'M (l |-| l'))
      ( 'Kg (m |-| m'))
      ( 'S (t |-| t'))
      ( 'A (i |-| i'))
      ( 'K (th |-| th'))
      ( 'Mol (n |-| n'))
      ( 'Cd (j |-| j'))

type Recip (d :: Unit) = Unitless :/ d

type family (d :: Unit) :^ (x :: TypeInt) :: Unit where
  Unitless :^ _ = Unitless
  _ :^ (Positive 0) = Unitless
  _ :^ (Negative 0) = Unitless
  d :^ (Positive 1) = d
  (UNIT ( 'M l) ( 'Kg m) ( 'S t) ( 'A i) ( 'K th) ( 'Mol n) ( 'Cd j)) :^ x =
    UNIT
      ( 'M (l |*| x))
      ( 'Kg (m |*| x))
      ( 'S (t |*| x))
      ( 'A (i |*| x))
      ( 'K (th |*| x))
      ( 'Mol (n |*| x))
      ( 'Cd (j |*| x))

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

--------------------------------------------------------------------------------

data ReadableUnit = Meter | Decimeter | Centimeter | Milimeter | Kilogram | Miligram | Other Unit

type family ToReadable (u :: Unit) :: ReadableUnit where
  ToReadable Meter = 'Meter
  ToReadable Kilogram = 'Kilogram
  ToReadable u = Other u