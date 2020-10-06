{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  HashedExpression.Internal.Expression
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- The @Expression@ data type is the core data structure of the HashedExpresion library. This module contains all necessary definitions for
-- constructing the Expression type.
module HashedExpression.Internal.Expression where

import Data.Array
import qualified Data.Complex as DC
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Proxy (Proxy (..))
import Data.Typeable
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownNat, Nat, natVal)
import HashedExpression.Internal.Base
import Prelude hiding ((**), (^))

-- |
data Expression (d :: [Nat]) (et :: ElementType) = Expression
  { -- | index to the topological root of ExpressionMap
    exRootID :: NodeID,
    -- | Map of all 'Node' indexable by 'NodeID'
    exMap :: ExpressionMap
  }
  deriving (Show, Eq, Ord)

type role Expression nominal nominal

-- --------------------------------------------------------------------------------------------------------------------

-- * Expression Element Types

-- --------------------------------------------------------------------------------------------------------------------

type R = 'R

type C = 'C

class IsElementType (d :: ElementType) where
  toElementType :: ElementType

instance IsElementType R where
  toElementType = R

instance IsElementType C where
  toElementType = C

-- --------------------------------------------------------------------------------------------------------------------

-- * Expression Dimensions

-- --------------------------------------------------------------------------------------------------------------------

-- | Type-level encoding of shapes
type Scalar = '[]

type D1 (n :: Nat) = '[n]

type D2 (m :: Nat) (n :: Nat) = '[m, n]

type D3 (m :: Nat) (n :: Nat) (p :: Nat) = '[m, n, p]

-- | Use to constrain 'Expression' dimensions at the type level. The size of each dimension in a vector can be specified
--   using a 'KnownNat', for vectors of n-dimensions use an n-sized tuple
class Dimension (d :: [Nat]) where
  toShape :: Shape

instance Dimension '[] where
  toShape = []

instance (KnownNat x, Dimension xs) => Dimension (x ': xs) where
  toShape = (nat @x) : toShape @xs

-- | Helper function, wrapper over 'natVal' from 'GHC.TypeLits' that automaticaly converts resulting value
--   from Integer to Int
nat :: forall n. (KnownNat n) => Int
nat = fromIntegral $ natVal (Proxy :: Proxy n)

-- --------------------------------------------------------------------------------------------------------------------

instance IsExpression (Expression d et) where
  asExpression (Expression nID mp) = (mp, nID)
  wrapExpression (mp, nID) = Expression nID mp

instance IsScalarReal (Expression Scalar R) where
  asScalarReal (Expression nID mp) = (mp, nID)
