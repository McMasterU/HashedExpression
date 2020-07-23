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

import Control.Monad (forM_)
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict
import qualified Data.IntMap as IM
import Data.List (find, groupBy, intercalate, nub, partition, sortBy, sortOn)
import Data.List.Extra (firstJust, groupOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Debug.Trace (traceShowId)
import HashedExpression.Derivative
import HashedExpression.Internal
import HashedExpression.Internal.CollectDifferential
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Node
import HashedExpression.Internal.Normalize
import HashedExpression.Internal.OperationSpec
import HashedExpression.Internal.Structure
import HashedExpression.Internal.Utils
import HashedExpression.Prettify (debugPrint)
import HashedExpression.Value

-- TODO: better sections in the Haddock.

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
      String -- The name of the bound
      Val -- The value of the upper bound
  | -- | A lower bound
    BoxLower
      String -- The name of the bound
      Val -- The value of the lower bound
  | -- | A range of values bound
    BoxBetween
      String -- The name of the bound
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

-------------------------------------------------------------------------------

-- | Negative infinity
ninf :: Double
ninf = -1 / 0

-- | Positive infinity
inf :: Double
inf = 1 / 0

-------------------------------------------------------------------------------

-- | Return a map from variable name to the corresponding partial derivative node id
--   Partial derivatives in Expression Scalar Covector should be collected before passing to this function
partialDerivativeMaps :: (ExpressionMap, NodeID) -> Map String NodeID
partialDerivativeMaps (dfMp, dfId) =
  case retrieveOp dfId dfMp of
    Sum ns | retrieveElementType dfId dfMp == Covector -> Map.fromList $ mapMaybe getPartial ns
    _ -> Map.fromList $ mapMaybe getPartial [dfId]
  where
    getPartial :: NodeID -> Maybe (String, NodeID)
    getPartial nId
      | MulD partialId dId <- retrieveOp nId dfMp,
        DVar name <- retrieveOp dId dfMp =
        Just (name, partialId)
      | InnerProdD partialId dId <- retrieveOp nId dfMp,
        DVar name <- retrieveOp dId dfMp =
        Just (name, partialId)
      | otherwise = Nothing

-------------------------------------------------------------------------------

-- | The statement of a constraint, including an 'ExpressionMap' subexpressions, the root 'NodeID' and its value
data ConstraintStatement
  = -- | A lower bound constraint
    Lower (ExpressionMap, NodeID) Val
  | -- | An upper bound constraint
    Upper (ExpressionMap, NodeID) Val
  | -- | A constraint with a lower and upper bound
    Between (ExpressionMap, NodeID) (Val, Val)
  deriving (Show, Eq, Ord)

-- * Functions for creating 'ConstraintStatement's.

infix 1 `between`, .>=, .<=, .==

-- | The expression is greater than the given value
(.>=) ::
  (DimensionType d) =>
  -- | The constraint expression
  Expression d R ->
  -- | The value of the lower bound
  Val ->
  -- | The corresponding constraint statement
  ConstraintStatement
(.>=) exp = Lower (unwrap exp)

-- | The expression is less than the given value
(.<=) ::
  (DimensionType d) =>
  -- | The constraint expression
  Expression d R ->
  -- | The value of the upper bound
  Val ->
  -- | The corresponding constraint statement
  ConstraintStatement
(.<=) exp = Upper (unwrap exp)

-- | The expression is between two values
between ::
  (DimensionType d) =>
  -- | The constraint expression
  Expression d R ->
  -- | The value of the lower and upper bounds
  (Val, Val) ->
  -- | The corresponding constraint statement
  ConstraintStatement
between exp = Between (unwrap exp)

-- | An equality constraint
--
--   Note: this is the same as setting the upper and lower bound to the same value
(.==) ::
  (DimensionType d) =>
  -- | The expression
  Expression d R ->
  -- | The value to set equal to the expression
  Val ->
  -- | The corresponding constraint statement
  ConstraintStatement
(.==) exp val = Between (unwrap exp) (val, val)

-- | Extract the expression from the 'ConstraintStatement'
getExpressionCS :: ConstraintStatement -> (ExpressionMap, NodeID)
getExpressionCS cs =
  case cs of
    Lower exp _ -> exp
    Upper exp _ -> exp
    Between exp _ -> exp

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
    Var var -> True
    _ -> False
  where
    (mp, n) = getExpressionCS cs

-------------------------------------------------------------------------------

-- | A constraint is a sequence of constraint statements
newtype Constraint = Constraint [ConstraintStatement]
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------

-- | Information about whether the optimization problem is well-founded
data ProblemResult
  = -- | The problem is valid, here is the problem
    ProblemValid Problem
  | -- | The problem is invalid, here is the reason
    ProblemInvalid String
  deriving (Show)

-------------------------------------------------------------------------------

-- | A function might be treated as function of variables that does not appears (like constraint of
-- optimization problem) so we want to pad zero partial derivative
--
-- @
--  (( ... ) dx + ( .... ) dy) [z, t] ---> (( ... ) dx + ( ... ) dy + 0dz + 0dt)
-- @
introduceZeroPartialDerivatives ::
  [(String, Shape)] ->
  Expression Scalar Covector ->
  Expression Scalar Covector
introduceZeroPartialDerivatives varsAndShape (Expression n mp) =
  let isD name nId
        | DVar varName <- retrieveOp nId mp,
          varName == name =
          True
        | otherwise = False
      alreadyExist name = any (isD name) . IM.keys $ mp
      makePart (name, shape)
        | isScalarShape shape = apply (Binary specMulD) [aConst shape 0, dVarWithShape shape name]
        | otherwise = apply (Binary specInnerProdD) [aConst shape 0, dVarWithShape shape name]
      listToInsert = map makePart . filter ((not . alreadyExist) . fst) $ varsAndShape
   in wrap $
        case retrieveOp n mp of
          Sum ns | retrieveElementType n mp == Covector -> sumMany $ map (mp,) ns ++ listToInsert
          _ -> sumMany $ (mp, n) : listToInsert

type ProblemConstructingM a = StateT ExpressionMap (Either String) a

getBound :: (ExpressionMap, NodeID) -> [ConstraintStatement] -> (Double, Double)
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

constructProblemHelper :: Expression Scalar R -> [String] -> Constraint -> ProblemConstructingM Problem
constructProblemHelper obj names (Constraint constraints) = do
  let varsSet = Set.fromList names
  let vs = varNodesWithShape (exMap obj) ++ concatMap (varNodesWithShape . fst . getExpressionCS) constraints
  checkConflictVars vs
  forM_ constraints checkConstraint
  -------------------------------------------------------------------------------
  let varAndShape = filter (\(name, _) -> Set.member name varsSet) vs
  -- When taking derivatives, make sure all the variables are presented by introduceZeroPartialDerivatives
  let takeDerivative =
        introduceZeroPartialDerivatives varAndShape
          . collectDifferentials
          . exteriorDerivative varsSet
  -------------------------------------------------------------------------------
  let (boxCS, scalarCS) = partition isBoxConstraint constraints
  let boxConstraints = map toBoxConstraint boxCS
  let expScalarConstraints = Set.toList . Set.fromList . map getExpressionCS $ scalarCS
  -------------------------------------------------------------------------------
  let f = unwrap $ normalize obj
  let df = unwrap $ takeDerivative obj
  fID <- mergeToMain f
  dfID <- mergeToMain df
  -------------------------------------------------------------------------------
  let processScalarConstraint :: (ExpressionMap, NodeID) -> ProblemConstructingM (NodeID, NodeID, (Double, Double))
      processScalarConstraint (mp, nID) = do
        let (lb, ub) = getBound (mp, nID) scalarCS
            exp = normalize $ Expression @Scalar @R nID mp
            g = unwrap exp
            dg = unwrap $ takeDerivative exp
        gID <- mergeToMain g
        dgID <- mergeToMain dg
        return (gID, dgID, (lb, ub))
  scalarConstraintsInfo <- mapM processScalarConstraint expScalarConstraints
  -------------------------------------------------------------------------------
  curMp <- get
  let finalRelevantVars = filter (\(name, _) -> Set.member name varsSet) $ varNodesWithId curMp
  let name2PartialDerivativeID :: Map String NodeID
      name2PartialDerivativeID = partialDerivativeMaps (curMp, dfID)
      variables =
        map
          ( \(name, varNodeID) ->
              Variable name varNodeID (fromJust $ Map.lookup name name2PartialDerivativeID)
          )
          finalRelevantVars
  -------------------------------------------------------------------------------
  let scalarConstraints =
        map
          ( \(gID, dgID, (lb, ub)) ->
              let name2PartialDerivativeID = partialDerivativeMaps (curMp, dgID)
               in ScalarConstraint
                    { constraintValueId = gID,
                      constraintPartialDerivatives = map (\(name, _) -> fromJust $ Map.lookup name name2PartialDerivativeID) finalRelevantVars,
                      constraintLowerBound = lb,
                      constraintUpperBound = ub
                    }
          )
          scalarConstraintsInfo
  mergedMap <- get
  let rootNs =
        fID :
        ( map partialDerivativeId variables
            ++ map constraintValueId scalarConstraints
            ++ concatMap constraintPartialDerivatives scalarConstraints
        )
      -- remove DVar nodes
      relevantNodes = Set.fromList $ topologicalSortManyRoots (mergedMap, rootNs)
      -- expression map
      finalMp :: ExpressionMap
      finalMp = IM.filterWithKey (\nId _ -> Set.member nId relevantNodes) mergedMap
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
    checkConflictVars :: [(String, Shape)] -> ProblemConstructingM ()
    checkConflictVars vs = case find (\ls -> length ls > 1) $ map nub $ groupOn fst . sortOn fst $ vs of
      Just ((x, _) : _) -> throwError $ "Shape of variable " ++ show x ++ " is not consistent between objective and constraints"
      _ -> return ()
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
        _ -> throwError "Only scalar inequality and box constraint for variable are supported"
    -------------------------------------------------------------------------------
    mergeToMain :: (ExpressionMap, NodeID) -> ProblemConstructingM NodeID
    mergeToMain (mp, nID) = do
      curMp <- get
      let (mergedMp, mergedNID) = safeMerge curMp (mp, nID)
      put mergedMp
      return mergedNID

-------------------------------------------------------------------------------

-- | Construct a Problem from given objective function and constraints
constructProblem :: Expression Scalar R -> [String] -> Constraint -> ProblemResult
constructProblem objectiveFunction varList constraint =
  case runStateT (constructProblemHelper objectiveFunction varList constraint) IM.empty of
    Left reason -> ProblemInvalid reason
    Right (problem, _) -> ProblemValid problem
