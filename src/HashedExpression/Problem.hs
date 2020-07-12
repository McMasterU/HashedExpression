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

import qualified Data.IntMap as IM
import HashedExpression.Internal.OperationSpec
import Data.List (find, intercalate, sortBy, sortOn)
import Data.List.Extra (firstJust)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import HashedExpression.Derivative
import HashedExpression.Internal
import HashedExpression.Internal.CollectDifferential
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Node
import HashedExpression.Internal.Normalize
import HashedExpression.Internal.Utils
import HashedExpression.Prettify (debugPrint)
import HashedExpression.Value

-- TODO: better sections in the Haddock.

-------------------------------------------------------------------------------

-- | Representation of a variable in an optimization problem
data Variable
  = Variable
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
data ScalarConstraint
  = ScalarConstraint
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
data Problem
  = Problem
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
    intercalate "\n" $
      [ "-------------- Objective Function --------------",
        debugPrint (expressionMap, objectiveId)
      ]
        ++ map showPartial variables
    where
      showPartial var =
        intercalate
          "\n"
          [ "----------------∂f/∂" ++ varName var ++ "-------------",
            debugPrint (expressionMap, partialDerivativeId var)
          ]

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
partialDerivativeMaps :: Expression Scalar Covector -> Map String NodeID
partialDerivativeMaps df@(Expression dfId dfMp) =
  case retrieveOp dfId dfMp of
    Sum ns | retrieveElementType dfId dfMp == Covector -> Map.fromList $ mapMaybe getPartial ns
    _ -> Map.fromList $ mapMaybe getPartial [dfId]
  where
    getPartial :: NodeID -> Maybe (String, NodeID)
    getPartial nId
      | Mul [partialId, dId] <- retrieveOp nId dfMp,
        retrieveElementType nId dfMp == Covector,
        DVar name <- retrieveOp dId dfMp =
        Just (name, partialId)
      | InnerProd partialId dId <- retrieveOp nId dfMp,
        retrieveElementType nId dfMp == Covector,
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
data Constraint = Constraint [ConstraintStatement]
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------

-- | Information about whether the optimization problem is well-founded
data ProblemResult
  = -- | The problem is valid, here is the problem
    ProblemValid Problem
  | -- | The problem is invalid, here is the reason
    ProblemInvalid String
  | -- | The problem has no variables
    NoVariables -- TODO haddock: - what about feasibility problems given constraints?
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
        | isScalarShape shape = mulMany [aConst shape 0, dVarWithShape shape name]
        | otherwise = apply (Binary specMulCovector) [aConst shape 0, dVarWithShape shape name]
      listToInsert = map makePart . filter ((not . alreadyExist) . fst) $ varsAndShape
   in wrap $
        case retrieveOp n mp of
          Sum ns | retrieveElementType n mp == Covector -> sumMany $ map (mp,) ns ++ listToInsert
          _ -> sumMany $ (mp, n) : listToInsert

-- | Construct a Problem from given objective function and constraints
constructProblem :: Expression Scalar R -> [String] -> Constraint -> ProblemResult
constructProblem objectiveFunction varList constraint
  | null varsList = NoVariables
  | Just reason <- checkError = ProblemInvalid reason
  | otherwise = -- all the constraints are good
    let -- variable name with its node ID and partial derivative ID
        variableDatas :: [(String, (NodeID, NodeID))]
        variableDatas = Map.toList $ Map.intersectionWith (,) name2Id name2PartialDerivativeId
        -- variables
        problemVariables :: [Variable]
        problemVariables =
          map
            ( \(varName, (nId, pId)) ->
                Variable {varName = varName, nodeId = nId, partialDerivativeId = pId}
            )
            variableDatas
        -- vars with shape
        varsWithShape :: [(String, Shape)]
        varsWithShape = zip varsList (map variableShape varsList)
        -------------------------------------------------------------------------------
        extractScalarConstraint :: [(String, Shape)] -> Constraint -> [(ScalarConstraint, ExpressionMap)]
        extractScalarConstraint varsWithShape (Constraint css) =
          let scalarConstraints = filter (not . isBoxConstraint) css
              listScalarExpressions = Set.toList . Set.fromList . map getExpressionCS $ scalarConstraints
              getBound (mp, n) = foldl update (ninf, inf) scalarConstraints
                where
                  getNum val = case val of
                    VScalar num -> num
                    VNum num -> num
                    _ -> error "Why scalar constraint is bounded by non-scalar ?"
                  update (lb, ub) cs
                    | (mp, n) == getExpressionCS cs =
                      case cs of
                        Lower _ val -> (max lb (getNum val), ub)
                        Upper _ val -> (lb, min ub (getNum val))
                        Between _ (val1, val2) -> (max lb (getNum val1), min ub (getNum val2))
                    | otherwise = (lb, ub)
              vars = map fst varsWithShape
              toScalarConstraint (mp, n) =
                let exp = Expression @Scalar @R n mp
                    g = normalize exp
                    dg =
                      introduceZeroPartialDerivatives varsWithShape
                        . collectDifferentials
                        . exteriorDerivative (Set.fromList vars)
                        $ g
                    (lb, ub) = getBound (mp, n)
                    name2PartialDerivativeId :: Map String NodeID
                    name2PartialDerivativeId = partialDerivativeMaps dg
                    constraintPartialDerivatives
                      | Map.keys name2PartialDerivativeId == vars = Map.elems name2PartialDerivativeId
                      | otherwise = error "variables of objective and constraints should be the same, but is different here"
                 in ( ScalarConstraint
                        { constraintValueId = exRootID g,
                          constraintPartialDerivatives = constraintPartialDerivatives,
                          constraintLowerBound = lb,
                          constraintUpperBound = ub
                        },
                      exMap g `IM.union` exMap dg
                    )
           in map toScalarConstraint listScalarExpressions
        -------------------------------------------------------------------------------
        extractBoxConstraint :: Constraint -> [BoxConstraint]
        extractBoxConstraint (Constraint css) = map toBoxConstraint . filter isBoxConstraint $ css
          where
            toBoxConstraint cs =
              case cs of
                Lower (mp, n) val ->
                  let Var name = retrieveOp n mp
                   in BoxLower name val
                Upper (mp, n) val ->
                  let Var name = retrieveOp n mp
                   in BoxUpper name val
                Between (mp, n) vals ->
                  let Var name = retrieveOp n mp
                   in BoxBetween name vals
        -------------------------------------------------------------------------------
        boxConstraints = extractBoxConstraint constraint
        scalarConstraintsWithMp = extractScalarConstraint varsWithShape constraint
        scalarConstraints = map fst scalarConstraintsWithMp
        mergedMap = IM.unions $ [dfMp, fMp] ++ map snd scalarConstraintsWithMp
        rootNs =
          exRootID f
            : ( map partialDerivativeId problemVariables
                  ++ map constraintValueId scalarConstraints
                  ++ concatMap constraintPartialDerivatives scalarConstraints
              )
        -- remove DVar nodes
        relevantNodes = Set.fromList $ topologicalSortManyRoots (mergedMap, rootNs)
        -- expression map
        problemExpressionMap :: ExpressionMap
        problemExpressionMap = IM.filterWithKey (\nId _ -> Set.member nId relevantNodes) mergedMap
        -- objective id
        problemObjectiveId = fId
     in ProblemValid $
          Problem
            { variables = problemVariables,
              objectiveId = problemObjectiveId,
              expressionMap = problemExpressionMap,
              boxConstraints = boxConstraints,
              scalarConstraints = scalarConstraints
            }
  where
    -- 1. set of name users consider as variable
    -- list of var names provided by users
    userSpecifiedVars = Set.fromList varList
    -- 2. list of all vars name in the expression, they can be variables or fixed values
    expressionVars = Set.fromList . map fst . expressionVarNodes $ f
    -- 3. list of possible names that could be variables
    -- just because user says it is variable and it is var node doesn't mean it
    -- appears in the d: for example f = 0 * x, then this will just be 0 and dx doesn't appear
    -- when we take the derivative
    possibleVars = Set.intersection expressionVars userSpecifiedVars
    -- normalize and take the derivative and collect differentials
    f@(Expression fId fMp) = normalize objectiveFunction
    df@(Expression dfId dfMp) = collectDifferentials . exteriorDerivative possibleVars $ f
    -- Map from a variable name to partial derivative id in the problem's ExpressionMap
    name2PartialDerivativeId :: Map String NodeID
    name2PartialDerivativeId = partialDerivativeMaps df
    -- 4. After this step we can know which are actually variables - those appear in name2PartialDerivativeId
    varsList = Map.keys name2PartialDerivativeId
    varsSet = Set.fromList varsList
    -------------------------------------------------------------------------------
    name2Id :: Map String NodeID
    name2Id = Map.fromList $ filter ((`Set.member` varsSet) . fst) $ expressionVarNodes f
    -- get variable shape
    variableShape :: String -> Shape
    variableShape name =
      let nId = fromMaybe (error "query non-variable name?") $ Map.lookup name name2Id
       in retrieveShape nId fMp
    checkError =
      case constraint of
        Constraint cs -> firstJust checkConstraint cs
    checkConstraint :: ConstraintStatement -> Maybe String
    checkConstraint cs =
      case retrieveNode n mp of
        (_, _, Var var) -- if it is a var, then should be box constraint
          | not (Set.member var varsSet) -> Just $ var ++ " is not a variable"
          | any (not . compatible (variableShape var)) (getValCS cs) ->
            Just $ "Bound for " ++ var ++ " is not in the right shape"
          | otherwise -> Nothing
        ([], _, _)
          | any (not . compatible []) (getValCS cs) ->
            Just "Scalar expression must be bounded by scalar value"
          | otherwise -> Nothing
        _ -> Just "Only scalar inequality and box constraint for variable are supported"
      where
        (mp, n) = getExpressionCS cs
