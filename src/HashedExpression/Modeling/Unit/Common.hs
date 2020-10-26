module HashedExpression.Modeling.Unit.Common where

import Data.Kind (Constraint)
import GHC.TypeLits (ErrorMessage, TypeError)
import GHC.TypeNats (Nat)
import qualified GHC.TypeNats as N

--------------------------------------------------------------------------------
--  General utilities

type Satisfied = (1 ~ 1)

type family IsEQ (x :: Ordering) :: Bool where
  IsEQ EQ = True
  IsEQ _ = False

type family IfThenElse (c :: Bool) (a :: k) (b :: k) where
  IfThenElse True a _ = a
  IfThenElse False _ b = b

type family ListLength (xs :: [a]) :: Nat where
  ListLength '[] = 0
  ListLength (_ ': xs) = 1 N.+ ListLength xs

--------------------------------------------------------------------------------
--  Utilities for natural number

infix 4 ==?

type (a :: Nat) ==? (b :: Nat) = IsEQ (N.CmpNat a b)

--------------------------------------------------------------------------------
--  Utilities for boolean

type family And (a :: Bool) (b :: Bool) :: Bool where
  And True True = True
  And _ _ = False

infixl 3 `And`

--------------------------------------------------------------------------------
--  Utilities for constraint programming

type family Unless (c :: Bool) (e :: ErrorMessage) :: Constraint where
  Unless True _ = Satisfied
  Unless False error = TypeError error
