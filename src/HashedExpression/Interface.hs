-- |
-- Module      :  HashedExpression.Problem
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- This module provides a interface for representing continuous optimization problems using HashedExpression. Represent an optimization problem
-- through the 'constructProblem' function, which will return a 'ProblemResult' structure that will wrap a 'Problem' structure if a valid
-- problem was able to be constructed. Use the 'Problem' structure in conjunction with the 'HashedExpression.Codegen' module to generate c code
-- for solving with your c code solver of choice
module HashedExpression.Interface where

import Control.Monad.Except (throwError)
import Control.Monad.State.Strict
import qualified Data.IntMap as IM
import Data.List (intercalate, partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import GHC.TypeLits (KnownNat, Nat)
import HashedExpression.Differentiation.Reverse
import HashedExpression.Internal
import HashedExpression.Internal.Base
import HashedExpression.Internal.MonadExpression
import HashedExpression.Internal.Node
import HashedExpression.Internal.Simplify
import HashedExpression.Modeling.Typed
import HashedExpression.Prettify
import HashedExpression.Value

--------------------------------------------------------------------------------
newtype Bound (n :: [Nat]) = Bound String

bound1D :: forall n. (KnownNat n) => String -> Bound '[n]
bound1D = Bound

bound2D :: forall m n. (KnownNat m, KnownNat n) => String -> Bound '[m, n]
bound2D = Bound

bound3D :: forall m n p. (KnownNat m, KnownNat n, KnownNat p) => String -> Bound '[m, n, p]
bound3D = Bound

--------------------------------------------------------------------------------

data Constraint
  = BoxLower
      String -- identifier of variable
      String -- identifier of bound
  | BoxUpper
      String -- identifier of variable
      String -- identifier of bound
  | ScalarLower
      RawExpr -- scalar real expression
      Double -- lower bound
  | ScalarUpper
      RawExpr -- scalar real expression
      Double -- upper bound
  | ScalarEqual
      RawExpr -- scalar real expression
      Double -- value

class BoundedBy a b where
  (.<=) :: a -> b -> Constraint
  (.>=) :: a -> b -> Constraint
  (.==) :: a -> b -> Constraint

instance IsScalarReal e => BoundedBy e Double where
  expr .<= b = ScalarUpper (asScalarRealRawExpr expr) b
  expr .>= b = ScalarLower (asScalarRealRawExpr expr) b
  expr .== b = ScalarEqual (asScalarRealRawExpr expr) b

instance BoundedBy (TypedExpr d R) (Bound d) where
  expr .<= (Bound b) =
    let (mp, nID) = asRawExpr expr
     in case retrieveOp nID mp of
          Var name -> BoxUpper name b
          _ -> error "Left hand side of box constraint must be a variable"
  expr .>= (Bound b) =
    let (mp, nID) = asRawExpr expr
     in case retrieveOp nID mp of
          Var name -> BoxLower name b
          _ -> error "Left hand side of box constraint must be a variable"
  expr .== _ = error "Found equality box constraint, consider using parameter instead"

--------------------------------------------------------------------------------
class IsIdentifier x where
  getIdentifier :: x -> String

instance IsExpression e => IsIdentifier e where
  getIdentifier expr
    | (_, _, Var name) <- retrieveNode nID mp = name
    | (_, _, Param name) <- retrieveNode nID mp = name
    | otherwise = error $ "Must be a variable, parameter or bound identifier, found: " <> prettify expr
    where
      (mp, nID) = asRawExpr expr

instance IsIdentifier (Bound d) where
  getIdentifier (Bound name) = name

data ValueAssignment = forall e. IsIdentifier e => e :-> Val

--------------------------------------------------------------------------------
data OptimizationProblem = forall e.
  IsScalarReal e =>
  OptimizationProblem
  { objective :: e,
    constraints :: [Constraint],
    values :: [ValueAssignment]
  }
