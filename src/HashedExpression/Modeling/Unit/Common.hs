module HashedExpression.Modeling.Unit.Common where

import Data.Kind (Constraint)
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:<>:)), Symbol, TypeError)
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

type family GCD (a :: Nat) (b :: Nat) :: Nat where
  GCD 0 a = a
  GCD a 0 = a
  GCD a b = GCD b (a `N.Mod` b)

type family PrintNats (x :: [Nat]) :: ErrorMessage where
  PrintNats '[] = Text ""
  PrintNats '[s] = ShowType s
  PrintNats (s ': ss) = ShowType s :<>: Text ", " :<>: PrintNats ss

--------------------------------------------------------------------------------
--  Utilities for boolean

type family And (a :: Bool) (b :: Bool) :: Bool where
  And True True = True
  And _ _ = False

infixl 3 `And`

--------------------------------------------------------------------------------
--  Utilities for ErrorMessage
type family RemoveEmpty (a :: [ErrorMessage]) :: [ErrorMessage] where
  RemoveEmpty '[] = '[]
  RemoveEmpty (Text "" ': xs) = RemoveEmpty xs
  RemoveEmpty (x ': xs) = x ': RemoveEmpty xs

type family Intercalate (x :: Symbol) (ys :: [ErrorMessage]) :: ErrorMessage where
  Intercalate _ '[] = Text ""
  Intercalate _ '[y] = y
  Intercalate x (y ': ys) = y :<>: Text x :<>: Intercalate x ys

--------------------------------------------------------------------------------
--  Utilities for constraint programming

type family Unless (c :: Bool) (e :: ErrorMessage) :: Constraint where
  Unless True _ = Satisfied
  Unless False error = TypeError error
