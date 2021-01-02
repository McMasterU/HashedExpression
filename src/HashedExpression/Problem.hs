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
module HashedExpression.Problem where

import Control.Monad.Except (throwError)
import Control.Monad.State.Strict
import Data.Function
import qualified Data.IntMap as IM
import Data.List (intercalate, partition)
import Data.List.NonEmpty (NonEmpty ((:|)), groupWith)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import GHC.TypeLits (KnownNat, Nat)
import HashedExpression.Differentiation.Reverse
import HashedExpression.Interface
import HashedExpression.Internal
import HashedExpression.Internal.Base
import HashedExpression.Internal.MonadExpression
import HashedExpression.Internal.Node
import HashedExpression.Internal.Simplify
import HashedExpression.Modeling.Typed (TypedExpr)
import HashedExpression.Prettify
import HashedExpression.Value


-------------------------------------------------------------------------------

-- | Representation of a variable in an optimization problem
data Variable = Variable
  { -- | The variable's name
    varName :: VarName,
    -- | The variable's node ID
    nodeId :: NodeID,
    -- | The ID of the partial derivative of the variable
    partialDerivativeId :: NodeID
  }
  deriving (Show)

-- | A box constraint in an optimization problem
data BoxConstraint
  = -- | An upper bound
    BoxUpper VarName BoundIdentifier
  | -- | A lower bound
    BoxLower VarName BoundIdentifier

-- | A general constraint in an optimization problem
-- is a constraint in a form: LB <= f(variables) <= UB where LB, f(variables), UB are scalar real values
data GeneralConstraint = GeneralConstraint
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
    generalConstraints :: [GeneralConstraint]
  }

-- | Negative infinity
ninf :: Double
ninf = -1 / 0

-- | Positive infinity
inf :: Double
inf = 1 / 0

type ProblemConstructingM a = StateT ExpressionMap (Either String) a

mergeToMain :: RawExpr -> ProblemConstructingM NodeID
mergeToMain (mp, nID) = do
  curMp <- get
  let (mergedMp, mergedNID) = safeMerge curMp (mp, nID)
  put mergedMp
  return mergedNID

--------------------------------------------------------------------------------

-- | Construct a Problem from given objective function and constraints
constructProblem :: IsScalarReal e => e -> [ConstraintDecl] -> Either String Problem
constructProblem objectiveFunction cs =
  fst <$> runStateT (constructProblemHelper simplifiedObjective simplifiedConstraint) IM.empty
  where
    simplifiedObjective = simplify $ asRawExpr objectiveFunction
    simplifiedConstraint =
      cs
        & map
          ( \case
              GeneralUpperDecl e ub -> GeneralUpperDecl (simplify e) ub
              GeneralLowerDecl e lb -> GeneralLowerDecl (simplify e) lb
              GeneralEqualDecl e eb -> GeneralEqualDecl (simplify e) eb
              c -> c
          )

--------------------------------------------------------------------------------
constructProblemHelper :: IsScalarReal e => e -> [ConstraintDecl] -> ProblemConstructingM Problem
constructProblemHelper objective constraints = do
  let generalConstraintDecls :: [(RawExpr, (Double, Double))]
      generalConstraintDecls =
        constraints
          >>= ( \case
                  GeneralUpperDecl e ub -> [(e, (ninf, ub))]
                  GeneralLowerDecl e lb -> [(e, (lb, inf))]
                  GeneralEqualDecl e eb -> [(e, (eb, eb))]
                  _ -> []
              )
          & groupWith fst
          & map
            ( \gr@((e, _) :| _) ->
                ( e,
                  ( maximum (NonEmpty.map (fst . snd) gr), -- maximum of all lower bounds
                    minimum (NonEmpty.map (snd . snd) gr) -- minimum of all upper bounds
                  )
                )
            )
  let rawObjective = asScalarRealRawExpr objective
  --------------------------------------------------------------------------------
  let vs = concatMap (varsWithShape . fst) $ rawObjective : map fst generalConstraintDecls
  let ps = concatMap (paramsWithShape . fst) $ rawObjective : map fst generalConstraintDecls
  when (Set.intersection (Set.fromList $ map fst vs) (Set.fromList $ map fst ps) /= Set.empty) $
    throwError "Variable and parameter must be of different name"
  -------------------------------------------------------------------------------
  let processF f = do
        fID <- mergeToMain f
        curMp <- get
        let (mp, name2ID) = partialDerivativesMap (curMp, fID)
        let (names, beforeMergeIDs) = unzip $ Map.toList name2ID
        afterMergedIDs <- mapM (mergeToMain . simplify . (mp,)) beforeMergeIDs
        return $ (fID, Map.fromList $ zip names afterMergedIDs)
  --------------------------------------------------------------------------------
  let lookupDerivative :: (String, Shape) -> Map String NodeID -> ProblemConstructingM NodeID
      lookupDerivative (name, shape) dMap = case Map.lookup name dMap of
        Just dID -> return dID
        _ -> introduceNode (shape, R, Const 0)
  -------------------------------------------------------------------------------
  (fID, fPartialDerivativeMap) <- processF rawObjective
  gs <-
    generalConstraintDecls
      & mapM
        ( \(g, (lb, ub)) -> do
            (gID, gPartialDerivativeMap) <- processF g
            return (gID, gPartialDerivativeMap, ub, lb)
        )
  -------------------------------------------------------------------------------
  variableNodes <- varNodes <$> get
  let toVariable (name, shape, nID) = do
        dID <- lookupDerivative (name, shape) fPartialDerivativeMap
        return $ Variable name nID dID
  variables <- mapM toVariable variableNodes
  -------------------------------------------------------------------------------
  let toGeneralConstraint (gID, gPartialDerivativeMap, lb, ub) = do
        partialDerivativeIDs <- mapM (\(name, shape, _) -> lookupDerivative (name, shape) gPartialDerivativeMap) variableNodes
        return $
          GeneralConstraint
            { constraintValueId = gID,
              constraintPartialDerivatives = partialDerivativeIDs,
              constraintLowerBound = lb,
              constraintUpperBound = ub
            }
  generalConstraints <- mapM toGeneralConstraint gs
  -------------------------------------------------------------------------------
  let boxConstraints =
        constraints
          >>= ( \case
                  BoxLowerDecl varName boundName -> [BoxLower varName boundName]
                  BoxUpperDecl varName boundName -> [BoxUpper varName boundName]
                  _ -> []
              )
  --------------------------------------------------------------------------------
  mergedMap <- get
  let rootNs =
        fID :
        ( map partialDerivativeId variables
            ++ map constraintValueId generalConstraints
            ++ concatMap constraintPartialDerivatives generalConstraints
        )
  let finalMp :: ExpressionMap
      finalMp = removeUnreachableManyRoots (mergedMap, rootNs)
  --------------------------------------------------------------------------------
  return $
    Problem
      { variables = variables,
        objectiveId = fID,
        expressionMap = finalMp,
        boxConstraints = boxConstraints,
        generalConstraints = generalConstraints
      }
