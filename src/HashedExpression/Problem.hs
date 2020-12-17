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
import HashedExpression.Prettify
import HashedExpression.Value

-------------------------------------------------------------------------------
data ValueAssignment = forall e. IsExpression e => e :-> Val

data OptimizationProblem = forall e.
  IsScalarReal e =>
  OptimizationProblem
  { objective :: e,
    constraints :: [ConstraintStatement],
    values :: [ValueAssignment]
  }

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
      Val -- The value of the upper bound
  | -- | A lower bound
    BoxLower
      String -- Name of bounded variable
      Val -- The value of the lower bound
  | -- | A range of values bound
    BoxBetween
      String -- Name of bounded variable
      (Val, Val) -- (lower, upper)

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

-------------------------------------------------------------------------------
instance Show Problem where
  show Problem {..} =
    intercalate
      "\n"
      [ "\n-------------------------------------------------------------------------------",
        showObjective,
        showPartials,
        intercalate "\n" (map showScalarConstraint scalarConstraints)
      ]
    where
      showObjective = "Objective: " ++ debugPrint (expressionMap, objectiveId)
      showPartials =
        intercalate "\n" $
          ["∂/∂" ++ varName var ++ ": " ++ debugPrint (expressionMap, partialDerivativeId var) | var <- variables]
      showScalarConstraint (ScalarConstraint vId pIDs lb ub) =
        let withVarName = zip (map varName variables) pIDs
         in intercalate "\n" $
              ["Constraint: " ++ show lb ++ " <= " ++ debugPrint (expressionMap, vId) ++ " <= " ++ show ub]
                ++ ["∂/∂" ++ name ++ ": " ++ debugPrint (expressionMap, pID) | (name, pID) <- withVarName]

-------------------------------------------------------------------------------

-- | Negative infinity
ninf :: Double
ninf = -1 / 0

-- | Positive infinity
inf :: Double
inf = 1 / 0

-------------------------------------------------------------------------------

-- | The statement of a constraint, including an 'ExpressionMap' subexpressions, the root 'NodeID' and its value
data ConstraintStatement
  = -- | A lower bound constraint
    Lower RawExpr Val
  | -- | An upper bound constraint
    Upper RawExpr Val
  | -- | A constraint with a lower and upper bound
    Between RawExpr (Val, Val)
  deriving (Show, Eq, Ord)

-- * Functions for creating 'ConstraintStatement's.

infix 1 `between`, .>=, .<=, .==

-- | The expression is greater than the given value
(.>=) ::
  IsExpression e =>
  -- | The constraint expression
  e ->
  -- | The value of the lower bound
  Val ->
  -- | The corresponding constraint statement
  ConstraintStatement
(.>=) exp = Lower (asRawExpr exp)

-- | The expression is less than the given value
(.<=) ::
  IsExpression e =>
  -- | The constraint expression
  e ->
  -- | The value of the upper bound
  Val ->
  -- | The corresponding constraint statement
  ConstraintStatement
(.<=) exp = Upper (asRawExpr exp)

-- | The expression is between two values
between ::
  IsExpression e =>
  -- | The constraint expression
  e ->
  -- | The value of the lower and upper bounds
  (Val, Val) ->
  -- | The corresponding constraint statement
  ConstraintStatement
between exp = Between (asRawExpr exp)

-- | An equality constraint
--
--   Note: this is the same as setting the upper and lower bound to the same value
(.==) ::
  IsExpression e =>
  -- | The constraint expression
  e ->
  -- | The value to set equal to the expression
  Val ->
  -- | The corresponding constraint statement
  ConstraintStatement
(.==) exp val = Between (asRawExpr exp) (val, val)

-- | Extract the expression from the 'ConstraintStatement'
getExpressionCS :: ConstraintStatement -> RawExpr
getExpressionCS cs =
  case cs of
    Lower exp _ -> exp
    Upper exp _ -> exp
    Between exp _ -> exp

mapExpressionCS :: (RawExpr -> RawExpr) -> ConstraintStatement -> ConstraintStatement
mapExpressionCS f cs =
  case cs of
    Lower exp v -> Lower (f exp) v
    Upper exp v -> Upper (f exp) v
    Between exp v -> Between (f exp) v

-- | Extract the value from the 'ConstraintStatement'
getValCS :: ConstraintStatement -> [Val]
getValCS cs =
  case cs of
    Lower _ val -> [val]
    Upper _ val -> [val]
    Between _ (val1, val2) -> [val1, val2]

-- | Returns True if the value is a box constraint, false otherwise
isBoxConstraint :: ConstraintStatement -> Bool
isBoxConstraint cs =
  case retrieveOp n mp of
    Var _ -> True
    _ -> False
  where
    (mp, n) = getExpressionCS cs

-------------------------------------------------------------------------------

-- | A constraint is a sequence of constraint statements
newtype Constraint = Constraint [ConstraintStatement]
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------

type ProblemConstructingM a = StateT ExpressionMap (Either String) a

-------------------------------------------------------------------------------
getBound :: RawExpr -> [ConstraintStatement] -> (Double, Double)
getBound (mp, n) = foldl update (ninf, inf)
  where
    getNum val = case val of
      VScalar num -> num
      VNum num -> num
    update (lb, ub) cs
      | (mp, n) == getExpressionCS cs =
        case cs of
          Lower _ val -> (max lb (getNum val), ub)
          Upper _ val -> (lb, min ub (getNum val))
          Between _ (val1, val2) -> (max lb (getNum val1), min ub (getNum val2))
      | otherwise = (lb, ub)

toBoxConstraint :: ConstraintStatement -> BoxConstraint
toBoxConstraint cs = case cs of
  Lower (mp, n) val ->
    let Var name = retrieveOp n mp
     in BoxLower name val
  Upper (mp, n) val ->
    let Var name = retrieveOp n mp
     in BoxUpper name val
  Between (mp, n) vals ->
    let Var name = retrieveOp n mp
     in BoxBetween name vals

mergeToMain :: RawExpr -> ProblemConstructingM NodeID
mergeToMain (mp, nID) = do
  curMp <- get
  let (mergedMp, mergedNID) = safeMerge curMp (mp, nID)
  put mergedMp
  return mergedNID

varsWithShape :: RawExpr -> [(String, Shape)]
varsWithShape = mapMaybe collect . IM.toList . fst
  where
    collect (_, (shape, _, Var name)) = Just (name, shape)
    collect _ = Nothing

paramsWithShape :: RawExpr -> [(String, Shape)]
paramsWithShape = mapMaybe collect . IM.toList . fst
  where
    collect (_, (shape, _, Param name)) = Just (name, shape)
    collect _ = Nothing

constructProblemHelper :: IsScalarReal e => e -> Constraint -> ProblemConstructingM Problem
constructProblemHelper obj (Constraint constraints) = do
  let vs = concatMap varsWithShape $ asScalarRealRawExpr obj : map getExpressionCS constraints
  let ps = concatMap paramsWithShape $ asScalarRealRawExpr obj : map getExpressionCS constraints
  when (Set.intersection (Set.fromList $ map fst vs) (Set.fromList $ map fst ps) /= Set.empty) $
    throwError "Variable and parameter must be of different name"
  -------------------------------------------------------------------------------
  forM_ constraints checkConstraint
  let (boxCS, scalarCS) = partition isBoxConstraint constraints
  let boxConstraints = map toBoxConstraint boxCS
  let expScalarConstraints = Set.toList . Set.fromList . map getExpressionCS $ scalarCS
  -------------------------------------------------------------------------------
  let processF exp = do
        let (mp, name2ID) = partialDerivativesMap exp
        let (names, beforeMergeIDs) = unzip $ Map.toList name2ID
        afterMergedIDs <- mapM (mergeToMain . simplify . (mp,)) beforeMergeIDs
        return $ Map.fromList $ zip names afterMergedIDs
  let lookupDerivative :: (String, Shape) -> Map String NodeID -> ProblemConstructingM NodeID
      lookupDerivative (name, shape) dMap = case Map.lookup name dMap of
        Just dID -> return dID
        _ -> introduceNode (shape, R, Const 0)
  -------------------------------------------------------------------------------
  fID <- mergeToMain $ asScalarRealRawExpr obj
  fPartialDerivativeMap <- processF obj
  -------------------------------------------------------------------------------
  let processScalarConstraint :: RawExpr -> ProblemConstructingM (NodeID, Map String NodeID, (Double, Double))
      processScalarConstraint exp = do
        let (lb, ub) = getBound exp scalarCS
        gID <- mergeToMain $ exp
        mapHaha <- processF exp
        return (gID, mapHaha, (lb, ub))
  scalarConstraintsInfo <- mapM processScalarConstraint expScalarConstraints
  -------------------------------------------------------------------------------
  variableNodes <- varNodes <$> get
  -------------------------------------------------------------------------------
  let toVariable (name, shape, nID) = do
        dID <- lookupDerivative (name, shape) fPartialDerivativeMap
        return $ Variable name nID dID
  variables <- mapM toVariable variableNodes
  -------------------------------------------------------------------------------
  let toScalarConstraint (gID, gPartialDerivativeMap, (lb, ub)) = do
        partialDerivativeIDs <- mapM (\(name, shape, _) -> lookupDerivative (name, shape) gPartialDerivativeMap) variableNodes
        return $
          ScalarConstraint
            { constraintValueId = gID,
              constraintPartialDerivatives = partialDerivativeIDs,
              constraintLowerBound = lb,
              constraintUpperBound = ub
            }
  scalarConstraints <- mapM toScalarConstraint scalarConstraintsInfo
  -------------------------------------------------------------------------------
  mergedMap <- get
  let rootNs =
        fID :
        ( map partialDerivativeId variables
            ++ map constraintValueId scalarConstraints
            ++ concatMap constraintPartialDerivatives scalarConstraints
        )
      -- expression map
      finalMp :: ExpressionMap
      finalMp = removeUnreachableManyRoots (mergedMap, rootNs)
  return $
    Problem
      { variables = variables,
        objectiveId = fID,
        expressionMap = finalMp,
        boxConstraints = boxConstraints,
        scalarConstraints = scalarConstraints
      }
  where
    -------------------------------------------------------------------------------
    checkConstraint :: ConstraintStatement -> ProblemConstructingM ()
    checkConstraint cs = do
      let (mp, n) = getExpressionCS cs
      case retrieveNode n mp of
        (shape, _, Var var) -- if it is a var, then should be box constraint
          | not . all (compatible shape) $ getValCS cs ->
            throwError $ "Bound for " ++ var ++ " is not in the right shape"
          | otherwise -> return ()
        ([], _, _)
          | not . all (compatible []) $ getValCS cs ->
            throwError "Scalar expression must be bounded by scalar value"
          | otherwise -> return ()
        _ -> throwError "Only scalar inequality and box constraint are supported"

-- | Construct a Problem from given objective function and constraints
constructProblem :: IsScalarReal e => e -> Constraint -> Either String Problem
constructProblem objectiveFunction (Constraint cs) =
  fst <$> runStateT (constructProblemHelper simplifiedObjective simplifiedConstraint) IM.empty
  where
    simplifiedObjective = simplify $ asRawExpr objectiveFunction
    simplifiedConstraint = Constraint $ map (mapExpressionCS simplify) cs
