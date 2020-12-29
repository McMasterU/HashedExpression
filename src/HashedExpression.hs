-- |
-- Module      :  HashedExpression
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- This module exports everything needed to build and evaluate HashedExpressions;
-- including all HashedExpression operators, derivative functions, eval methods
-- and normalization. For example,
--
-- @
--  x = variable "x"
--  y = variable "y"
--  expr = x + y
-- @
--
-- the above code creates a simple HashedExpression using the
-- 'variable' constructor method and taking advantage of the 'Num' class instance
module HashedExpression
  ( module HashedExpression.Internal.Simplify,
    module HashedExpression.Prettify,
    module HashedExpression.Interp,
    module HashedExpression.Internal.Base,
    module HashedExpression.Problem,
    module HashedExpression.Value,
    module HashedExpression.Codegen,
    module HashedExpression.Codegen.CSimple,
    ValueAssignment (..),
    OptimizationProblem (..),
    proceed,
    bound1D,
    bound2D,
    bound3D,
  )
where

import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import GHC.TypeLits (KnownNat, Nat)
import HashedExpression.Codegen
import HashedExpression.Codegen.CSimple
import HashedExpression.Internal.Base
import HashedExpression.Internal.Node
import HashedExpression.Internal.Simplify
import HashedExpression.Interp
import HashedExpression.Prettify
import HashedExpression.Problem
import HashedExpression.Value
import Prelude hiding ((**), (^))

newtype Bound (n :: [Nat]) = Bound String

bound1D :: forall n. (KnownNat n) => String -> Bound '[n]
bound1D = Bound

bound2D :: forall m n. (KnownNat m, KnownNat n) => String -> Bound '[m, n]
bound2D = Bound

bound3D :: forall m n p. (KnownNat m, KnownNat n, KnownNat p) => String -> Bound '[m, n, p]
bound3D = Bound

data ValueAssignment = forall e. IsIdentifier e => e :-> Val

data OptimizationProblem = forall e.
  IsScalarReal e =>
  OptimizationProblem
  { objective :: e,
    constraints :: [ConstraintStatement],
    values :: [ValueAssignment]
  }

class IsIdentifier x where
  getIdentifier :: x -> Maybe String

instance IsExpression e => IsIdentifier e where
  getIdentifier expr
    | (_, _, Var name) <- retrieveNode nID mp = Just name
    | (_, _, Param name) <- retrieveNode nID mp = Just name
    | otherwise = Nothing
    where
      (mp, nID) = asRawExpr expr

instance IsIdentifier (Bound d) where
  getIdentifier (Bound name) = Just name

mkValMap :: [ValueAssignment] -> ValMap
mkValMap ss = Map.fromList $ mapMaybe f ss
  where
    f (e :-> val) = (, val) <$> getIdentifier e

proceed ::
  Codegen codegen =>
  -- | the optimization problem to solve
  OptimizationProblem ->
  -- | code generator
  codegen ->
  -- | working directory
  FilePath ->
  IO ()
proceed OptimizationProblem {..} codegen workingDir = case constructProblemAndGenCode of
  Right ok -> ok workingDir
  Left reason -> putStrLn reason
  where
    constructProblemAndGenCode :: Either String (FilePath -> IO ())
    constructProblemAndGenCode = do
      problem <- constructProblem objective (Constraint constraints)
      generateProblemCode codegen problem (mkValMap values)


