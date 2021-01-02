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

