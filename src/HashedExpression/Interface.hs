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

type VarName = String

type BoundIdentifier = String

data ConstraintDecl
  = BoxLowerDecl VarName BoundIdentifier
  | BoxUpperDecl VarName BoundIdentifier
  | GeneralLowerDecl RawExpr Double
  | GeneralUpperDecl RawExpr Double
  | GeneralEqualDecl RawExpr Double

infix 4 .<=
infix 4 .>=
infix 4 .==

class BoundedBy a b where
  (.<=) :: a -> b -> ConstraintDecl
  (.>=) :: a -> b -> ConstraintDecl
  (.==) :: a -> b -> ConstraintDecl

instance IsScalarReal e => BoundedBy e Double where
  expr .<= b = GeneralUpperDecl (asScalarRealRawExpr expr) b
  expr .>= b = GeneralLowerDecl (asScalarRealRawExpr expr) b
  expr .== b = GeneralEqualDecl (asScalarRealRawExpr expr) b

instance IsScalarReal e => BoundedBy e Integer where
  expr .<= b = GeneralUpperDecl (asScalarRealRawExpr expr) $ fromIntegral b
  expr .>= b = GeneralLowerDecl (asScalarRealRawExpr expr) $ fromIntegral b
  expr .== b = GeneralEqualDecl (asScalarRealRawExpr expr) $ fromIntegral b

instance BoundedBy (TypedExpr d R) (Bound d) where
  expr .<= (Bound b) =
    case getOp expr of
      Var name -> BoxUpperDecl name b
      _ -> error $ "Left hand side of box constraint must be a variable"
  expr .>= (Bound b) =
    case getOp expr of
      Var name -> BoxLowerDecl name b
      _ -> error $ "Left hand side of box constraint must be a variable"
  expr .== _ = error "Found equality box constraint, consider using parameter instead"

--------------------------------------------------------------------------------
class IsIdentifier x where
  getIdentifier :: x -> String

instance IsIdentifier String where
  getIdentifier = id

instance IsIdentifier RawExpr where
  getIdentifier expr
    | Var name <- getOp expr = name
    | Param name <- getOp expr = name
    | otherwise = error $ "Must be a variable, parameter or bound identifier, found"

instance IsIdentifier (TypedExpr d R) where
  getIdentifier expr
    | Var name <- getOp expr = name
    | Param name <- getOp expr = name
    | otherwise = error $ "Must be a variable, parameter or bound identifier, found"

instance IsIdentifier (Bound d) where
  getIdentifier (Bound name) = name

data ValueAssignment = forall e. IsIdentifier e => e :-> Val

--------------------------------------------------------------------------------
data OptimizationProblem = forall e.
  IsScalarReal e =>
  OptimizationProblem
  { objective :: e,
    constraints :: [ConstraintDecl],
    values :: [ValueAssignment]
  }

mkValMap :: [ValueAssignment] -> ValMap
mkValMap ss = Map.fromList $ map f ss
  where
    f (e :-> val) = (getIdentifier e, val)
