module HashedExpression.Problem where

import qualified Data.IntMap as IM
import Data.List (find, intercalate, sortBy, sortOn)
import Data.List.Extra (firstJust)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import HashedExpression.Derivative
import HashedExpression.Internal.CollectDifferential
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Inner
import HashedExpression.Internal.Node
import HashedExpression.Internal.Normalize
import HashedExpression.Internal.Utils
import HashedExpression.Prettify (debugPrint)
import HashedExpression.Value

-------------------------------------------------------------------------------

-- |
data Variable
  = Variable
      { varName :: String,
        nodeId :: NodeID,
        partialDerivativeId :: NodeID
      }
  deriving (Show)

-- |
data BoxConstraint
  = BoxUpper String Val
  | BoxLower String Val
  | BoxBetween String (Val, Val)

-- |
data ScalarConstraint
  = ScalarConstraint
      { constraintValueId :: NodeID,
        constraintPartialDerivatives :: [NodeID],
        constraintLowerBound :: Double,
        constraintUpperBound :: Double
      }
  deriving (Show, Eq, Ord)

-- | Problem represents a valid optimization problem
data Problem
  = Problem
      { variables :: [Variable],
        objectiveId :: NodeID,
        expressionMap :: ExpressionMap,
        boxConstraints :: [BoxConstraint],
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
ninf :: Double
ninf = -1 / 0

inf :: Double
inf = 1 / 0

-------------------------------------------------------------------------------

-- | Return a map from variable name to the corresponding partial derivative node id
--   Partial derivatives in Expression Scalar Covector should be collected before passing to this function
partialDerivativeMaps :: Expression Scalar Covector -> Map String NodeID
partialDerivativeMaps df@(Expression dfId dfMp) =
  case retrieveNode dfId dfMp of
    Sum Covector ns -> Map.fromList $ mapMaybe getPartial ns
    _ -> Map.fromList $ mapMaybe getPartial [dfId]
  where
    getPartial :: NodeID -> Maybe (String, NodeID)
    getPartial nId
      | Mul Covector [partialId, dId] <- retrieveNode nId dfMp,
        DVar name <- retrieveNode dId dfMp =
        Just (name, partialId)
      | InnerProd Covector partialId dId <- retrieveNode nId dfMp,
        DVar name <- retrieveNode dId dfMp =
        Just (name, partialId)
      | otherwise = Nothing

-------------------------------------------------------------------------------

-- |
data ConstraintStatement
  = Lower (ExpressionMap, NodeID) Val
  | Upper (ExpressionMap, NodeID) Val
  | Between (ExpressionMap, NodeID) (Val, Val)
  deriving (Show, Eq, Ord)

-- |
infix 1 `between`, .>=, .<=, .==

(.>=) :: (DimensionType d) => Expression d R -> Val -> ConstraintStatement
(.>=) exp = Lower (unwrap exp)

(.<=) :: (DimensionType d) => Expression d R -> Val -> ConstraintStatement
(.<=) exp = Upper (unwrap exp)

between :: (DimensionType d) => Expression d R -> (Val, Val) -> ConstraintStatement
between exp = Between (unwrap exp)

(.==) :: (DimensionType d) => Expression d R -> Val -> ConstraintStatement
(.==) exp val = Between (unwrap exp) (val, val)

getExpressionCS :: ConstraintStatement -> (ExpressionMap, NodeID)
getExpressionCS cs =
  case cs of
    Lower exp _ -> exp
    Upper exp _ -> exp
    Between exp _ -> exp

getValCS :: ConstraintStatement -> [Val]
getValCS cs =
  case cs of
    Lower _ val -> [val]
    Upper _ val -> [val]
    Between _ (val1, val2) -> [val1, val2]

isBoxConstraint :: ConstraintStatement -> Bool
isBoxConstraint cs =
  case retrieveNode n mp of
    Var var -> True
    _ -> False
  where
    (mp, n) = getExpressionCS cs

-------------------------------------------------------------------------------
data Constraint
  = NoConstraint
  | Constraint [ConstraintStatement]
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------

-- |
data ProblemResult
  = ProblemValid Problem
  | ProblemInvalid String -- reason
  | NoVariables -- TODO - what about feasibility problems given constraints?
  deriving (Show)

-------------------------------------------------------------------------------

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
        extractScalarConstraint _ NoConstraint = []
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
        extractBoxConstraint NoConstraint = []
        extractBoxConstraint (Constraint css) = map toBoxConstraint . filter isBoxConstraint $ css
          where
            toBoxConstraint cs =
              case cs of
                Lower (mp, n) val ->
                  let Var name = retrieveNode n mp
                   in BoxLower name val
                Upper (mp, n) val ->
                  let Var name = retrieveNode n mp
                   in BoxUpper name val
                Between (mp, n) vals ->
                  let Var name = retrieveNode n mp
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
        NoConstraint -> Nothing
        Constraint cs -> firstJust checkConstraint cs
    checkConstraint :: ConstraintStatement -> Maybe String
    checkConstraint cs =
      case retrieveInternal n mp of
        (_, Var var) -- if it is a var, then should be box constraint
          | not (Set.member var varsSet) -> Just $ var ++ " is not a variable"
          | any (not . compatible (variableShape var)) (getValCS cs) ->
            Just $ "Bound for " ++ var ++ " is not in the right shape"
          | otherwise -> Nothing
        ([], _)
          | any (not . compatible []) (getValCS cs) ->
            Just "Scalar expression must be bounded by scalar value"
          | otherwise -> Nothing
        _ -> Just "Only scalar inequality and box constraint for variable are supported"
      where
        (mp, n) = getExpressionCS cs
