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
module HashedExpression.Problem2 where

import Control.Monad.Except (throwError)
import Control.Monad.State.Strict
import qualified Data.IntMap as IM
import Data.List (intercalate, partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import HashedExpression.Differentiation.Reverse
import HashedExpression.Internal
import HashedExpression.Internal.Base
import HashedExpression.Internal.MonadExpression
import HashedExpression.Internal.Node
import HashedExpression.Internal.Simplify
import HashedExpression.Interface
import HashedExpression.Prettify
import HashedExpression.Value
import GHC.TypeLits (Nat, KnownNat)

-------------------------------------------------------------------------------

-- | Representation of a variable in an optimization problem
data Variable = Variable
  { -- | The variable's name
    varName :: String,
    -- | The variable's node ID
    nodeId :: NodeID,
    -- | The ID of the partial derivative of the variable
    partialDerivativeId :: NodeID
  }
  deriving (Show)

-- | A box constraint in an optimization problem
data BoxConstraint
  = -- | An upper bound
    BoxUpper
      String -- Name of bounded variable
      String -- Identifier of the upper bound
  | -- | A lower bound
    BoxLower
      String -- Name of bounded variable
      String -- The value of the lower bound

-- | A scalar constraint in an optimization problem
-- is a constraint in a form: LB <= f(variables) <= UB where LB, f(variables), UB are scalar real values
data ScalarConstraint = ScalarConstraint
  { -- | The node ID of the constraint
    constraintValueId :: NodeID,
    -- | The partial derivatives of the constraint
    constraintPartialDerivatives :: [NodeID],
    -- | The lower bound of the constraint
    constraintLowerBound :: Double,
    -- | The upper bound of the constraint
    constraintUpperBound :: Double
  }
  deriving (Show, Eq, Ord)

-- | Problem represents a valid optimization problem
data Problem = Problem
  { -- | The variables present in the problem
    variables :: [Variable],
    -- | The node ID of the objective expression
    objectiveId :: NodeID,
    -- | The expression map of the problem, including the objective function and all constraints
    expressionMap :: ExpressionMap,
    -- | A list of box constraints in the problem
    boxConstraints :: [BoxConstraint],
    -- | A list of scalar constraints in the problem
    scalarConstraints :: [ScalarConstraint]
  }

-- | Negative infinity
ninf :: Double
ninf = -1 / 0

-- | Positive infinity
inf :: Double
inf = 1 / 0




-- type ProblemConstructingM a = StateT ExpressionMap (Either String) a
-- --------------------------------------------------------------------------------

-- constructProblemHelper :: OptimizationProblem -> ProblemConstructingM Problem
-- constructProblemHelper optimizationProblemDecl = do
--   let vs = concatMap varsWithShape $ asScalarRealRawExpr obj : map getExpressionCS constraints
--   let ps = concatMap paramsWithShape $ asScalarRealRawExpr obj : map getExpressionCS constraints
--   when (Set.intersection (Set.fromList $ map fst vs) (Set.fromList $ map fst ps) /= Set.empty) $
--     throwError "Variable and parameter must be of different name"
--   -------------------------------------------------------------------------------
--   forM_ constraints checkConstraint
--   let (boxCS, scalarCS) = partition isBoxConstraint constraints
--   let boxConstraints = map toBoxConstraint boxCS
--   let expScalarConstraints = Set.toList . Set.fromList . map getExpressionCS $ scalarCS
--   -------------------------------------------------------------------------------
--   let processF exp = do
--         let (mp, name2ID) = partialDerivativesMap exp
--         let (names, beforeMergeIDs) = unzip $ Map.toList name2ID
--         afterMergedIDs <- mapM (mergeToMain . simplify . (mp,)) beforeMergeIDs
--         return $ Map.fromList $ zip names afterMergedIDs
--   let lookupDerivative :: (String, Shape) -> Map String NodeID -> ProblemConstructingM NodeID
--       lookupDerivative (name, shape) dMap = case Map.lookup name dMap of
--         Just dID -> return dID
--         _ -> introduceNode (shape, R, Const 0)
--   -------------------------------------------------------------------------------
--   fID <- mergeToMain $ asScalarRealRawExpr obj
--   fPartialDerivativeMap <- processF obj
--   -------------------------------------------------------------------------------
--   let processScalarConstraint :: RawExpr -> ProblemConstructingM (NodeID, Map String NodeID, (Double, Double))
--       processScalarConstraint exp = do
--         let (lb, ub) = getBound exp scalarCS
--         gID <- mergeToMain $ exp
--         mapHaha <- processF exp
--         return (gID, mapHaha, (lb, ub))
--   scalarConstraintsInfo <- mapM processScalarConstraint expScalarConstraints
--   -------------------------------------------------------------------------------
--   variableNodes <- varNodes <$> get
--   -------------------------------------------------------------------------------
--   let toVariable (name, shape, nID) = do
--         dID <- lookupDerivative (name, shape) fPartialDerivativeMap
--         return $ Variable name nID dID
--   variables <- mapM toVariable variableNodes
--   -------------------------------------------------------------------------------
--   let toScalarConstraint (gID, gPartialDerivativeMap, (lb, ub)) = do
--         partialDerivativeIDs <- mapM (\(name, shape, _) -> lookupDerivative (name, shape) gPartialDerivativeMap) variableNodes
--         return $
--           ScalarConstraint
--             { constraintValueId = gID,
--               constraintPartialDerivatives = partialDerivativeIDs,
--               constraintLowerBound = lb,
--               constraintUpperBound = ub
--             }
--   scalarConstraints <- mapM toScalarConstraint scalarConstraintsInfo
--   -------------------------------------------------------------------------------
--   mergedMap <- get
--   let rootNs =
--         fID :
--         ( map partialDerivativeId variables
--             ++ map constraintValueId scalarConstraints
--             ++ concatMap constraintPartialDerivatives scalarConstraints
--         )
--       -- expression map
--       finalMp :: ExpressionMap
--       finalMp = removeUnreachableManyRoots (mergedMap, rootNs)
--   return $
--     Problem
--       { variables = variables,
--         objectiveId = fID,
--         expressionMap = finalMp,
--         boxConstraints = boxConstraints,
--         scalarConstraints = scalarConstraints
--       }
--   where
--     -------------------------------------------------------------------------------
--     checkConstraint :: ConstraintStatement -> ProblemConstructingM ()
--     checkConstraint cs = do
--       let (mp, n) = getExpressionCS cs
--       case retrieveNode n mp of
--         (shape, _, Var var) -- if it is a var, then should be box constraint
--           | not . all (compatible shape) $ getValCS cs ->
--             throwError $ "Bound for " ++ var ++ " is not in the right shape"
--           | otherwise -> return ()
--         ([], _, _)
--           | not . all (compatible []) $ getValCS cs ->
--             throwError "Scalar expression must be bounded by scalar value"
--           | otherwise -> return ()
--         _ -> throwError "Only scalar inequality and box constraint are supported"
