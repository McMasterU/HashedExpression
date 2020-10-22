{-# LANGUAGE TypeOperators #-}

module HashedExpression.Modeling.Unit.Types where

import qualified GHC.TypeNats as N
import GHC.TypeNats (Nat)
import Numeric.NumType.DK.Integers (TypeInt (..), type (+), type (-))
import qualified Numeric.NumType.DK.Integers as I

type family IfThenElse (c :: Bool) (a :: k) (b :: k) where
  IfThenElse True a _ = a
  IfThenElse False _ b = b  

data MyInt = Pos Nat | Neg Nat

type family Normalize (x :: MyInt) where
  Normalize (Pos 0) = Pos 0
  Normalize (Neg 0) = Pos 0
  Normalize x = x

type family (|+|) (x :: MyInt) (y :: MyInt) where
  (|+|) (Pos x) (Pos y) = Normalize (Pos (x N.+ y))
  (|+|) (Neg x) (Neg y) = Normalize (Neg (x N.+ y))
  (|+|) (Pos x) (Neg y) = Normalize (IfThenElse (x N.<=? y) (Neg (y N.- x)) (Pos (x N.- y)))
  (|+|) (Neg x) (Pos y) = Normalize (IfThenElse (x N.<=? y) (Neg (y N.- x)) (Pos (x N.- y)))

type family (|*|) (x :: MyInt) (y :: MyInt) where
  (|*|) (Pos x) (Pos y) = Normalize (Pos (x N.* y))
  (|*|) (Neg x) (Neg y) = Normalize (Pos (x N.* y))
  (|*|) (Pos x) (Neg y) = Normalize (Neg (x N.* y))
  (|*|) (Neg x) (Pos y) = Normalize (Neg (x N.* y))




-- type family NEG (n :: Nat) :: TypeInt where
--   NEG 0 = Zero
--   NEG 1 = Neg1
--   NEG 2 = Neg2
--  NEG 3 = Neg3
--   NEG 4 = Neg4
--   NEG 5 = Neg5
--   NEG 6 = Neg6
--   NEG 7 = Neg7
--   NEG 8 = Neg8
--   NEG 9 = Neg9
--   NEG n = Neg10Minus n

-- type family POS (n :: Nat) :: TypeInt where
--   POS 0 = Zero
--   POS 1 = Pos1
--   POS 2 = Pos2
--   POS 3 = Pos3
--   POS 4 = Pos4
--   POS 5 = Pos5
--   POS 6 = Pos6
--   POS 7 = Pos7
--   POS 8 = Pos8
--   POS 9 = Pos9
--   POS n = Pos10Plus n

newtype UMeter = Meter TypeInt

newtype UKilogram = Kilogram TypeInt

newtype USecond = Second TypeInt

newtype UAmpere = Ampere TypeInt

newtype UKevin =Kevin TypeInt

newtype UMole = Mole TypeInt

newtype UCandela = Candela TypeInt

data Unit
  = Unit
      UMeter -- (length, m - meter)
      UKilogram -- (mass, kg - kilogram)
      USecond -- (time, s - second)
      UAmpere -- (electric current, A - ampere)
      UKevin -- (thermodynamic temperature, K - 'Kevin)
      UMole -- (amount of substance, mol - mole)
      UCandela -- (luminous intensity, cd - candela)

type Unitless =
  'Unit
    ('Meter Zero)
    ('Kilogram Zero)
    ('Second Zero)
    ('Ampere Zero)
    ('Kevin Zero)
    ('Mole Zero)
    ('Candela Zero)

--------------------------------------------------------------------------------
infixr 8 :^ 

infixl 7 :*, :/ 

type family (a :: Unit) :* (b :: Unit) :: Unit where
  Unitless :* d = d
  d :* Unitless = d
  ( 'Unit ('Meter l) ('Kilogram m) ('Second t) ('Ampere i) ('Kevin th) 
            ('Mole n) ('Candela j))
    :* ( 'Unit ('Meter l') ('Kilogram m') 
            ('Second t') ('Ampere i') ('Kevin th') ('Mole n') ('Candela j')) =
    'Unit ('Meter (l + l')) ('Kilogram (m + m')) ('Second (t + t')) 
            ('Ampere (i + i')) ('Kevin (th + th')) ('Mole (n + n')) ('Candela (j + j'))

type family (a :: Unit)  :/ (b :: Unit) :: Unit where
  d  :/ Unitless = d
  d  :/ d = Unitless
  ( 'Unit ('Meter l) ('Kilogram m) ('Second t) ('Ampere i) ('Kevin th) ('Mole n) ('Candela j))  :/ ( 'Unit ('Meter l') ('Kilogram m') ('Second t') ('Ampere i') ('Kevin th') ('Mole n') ('Candela j')) =
    'Unit ('Meter (l - l')) ('Kilogram (m - m')) ('Second (t - t')) ('Ampere (i - i')) ('Kevin (th - th')) ('Mole (n - n')) ('Candela (j - j'))

type Recip (d :: Unit) = Unitless  :/ d

type family (d :: Unit)  :^ (x :: TypeInt) where
  Unitless  :^ _ = Unitless
  _  :^ 'Zero = Unitless
  d  :^ 'Pos1 = d
  ( 'Unit ('Meter l) ('Kilogram m) ('Second t) ('Ampere i) ('Kevin th) ('Mole n) ('Candela j))  :^ x =
    'Unit ('Meter (l I.* x)) ('Kilogram (m I.* x)) ('Second (t I.* x)) ('Ampere (i I.* x)) 
        ('Kevin (th I.* x)) ('Mole (n I.* x)) ('Candela (j I.* x))

--------------------------------------------------------------------------------
type Meter = 'Unit ('Meter Pos1) ('Kilogram Zero) ('Second Zero) ('Ampere Zero) ('Kevin Zero) ('Mole Zero) ('Candela Zero)
type Kilogram = 'Unit ('Meter Zero) ('Kilogram Pos1) ('Second Zero) ('Ampere Zero) ('Kevin Zero) ('Mole Zero) ('Candela Zero)
type Second = 'Unit ('Meter Zero) ('Kilogram Zero) ('Second Pos1) ('Ampere Zero) ('Kevin Zero) ('Mole Zero) ('Candela Zero)
type Ampere = 'Unit ('Meter Zero) ('Kilogram Zero) ('Second Zero) ('Ampere Pos1) ('Kevin Zero) ('Mole Zero) ('Candela Zero)
type Kevin = 'Unit ('Meter Zero) ('Kilogram Zero) ('Second Zero) ('Ampere Zero) ('Kevin Pos1) ('Mole Zero) ('Candela Zero)
type Mole = 'Unit ('Meter Zero) ('Kilogram Zero) ('Second Zero) ('Ampere Zero) ('Kevin Zero) ('Mole Pos1) ('Candela Zero)
type Candela = 'Unit ('Meter Zero) ('Kilogram Zero) ('Second Zero) ('Ampere Zero) ('Kevin Zero) ('Mole Zero) ('Candela Pos1)

--------------------------------------------------------------------------------