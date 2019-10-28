{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module HashedSolver where

import Control.Monad (unless, when)
import Data.Array (bounds)
import qualified Data.IntMap as IM
import Data.List (find, intercalate, sortBy, sortOn)
import Data.List.Extra (firstJust)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Tuple.HT (fst3, thd3)
import Debug.Trace (traceShowId)
import HashedCollect
import HashedDerivative
import HashedExpression
    ( Covector
    , DimensionType
    , ET(..)
    , Expression(..)
    , ExpressionMap
    , Node(..)
    , R
    , Scalar
    , Shape
    , exIndex
    , exMap
    )
import HashedInner
import HashedNode
import HashedNormalize (normalize)
import HashedPrettify
import HashedToC
import HashedUtils
import System.Process (readProcess, readProcessWithExitCode)

ninf :: Double
ninf = -1 / 0

inf :: Double
inf = 1 / 0

d2s :: Double -> String
d2s val
    | val == ninf = "-INFINITY"
    | val == inf = "INFINITY"
    | otherwise = show val

-- |
--
data Variable =
    Variable
        { varName :: String
        , nodeId :: Int
        , partialDerivativeId :: Int
        }
    deriving (Show)

-- |
--
data Problem =
    Problem
        { variables :: [Variable]
        , objectiveId :: Int
        , expressionMap :: ExpressionMap
        , memMap :: MemMap
        , boxConstraints :: [BoxConstraint]
        , scalarConstraints :: [ScalarConstraint]
        }

-- |
--
data BoxConstraint
    = BoxUpper String Val
    | BoxLower String Val
    | BoxBetween String (Val, Val)

-- |
--
data ScalarConstraint =
    ScalarConstraint
        { constraintValueId :: Int
        , constraintPartialDerivatives :: [Int]
        , constraintLowerBound :: Double
        , constraintUpperBound :: Double
        }
    deriving (Show, Eq, Ord)

-- |
--
data Constraint
    = NoConstraint
    | Constraint [ConstraintStatement]
    deriving (Show, Eq, Ord)

-- |
--
data ConstraintStatement
    = Lower (ExpressionMap, Int) Val
    | Upper (ExpressionMap, Int) Val
    | Between (ExpressionMap, Int) (Val, Val)
    deriving (Show, Eq, Ord)

getExpressionCS :: ConstraintStatement -> (ExpressionMap, Int)
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

-- |
--
(.>=) :: (DimensionType d) => Expression d R -> Val -> ConstraintStatement
(.>=) exp = Lower (unwrap exp)

(.<=) :: (DimensionType d) => Expression d R -> Val -> ConstraintStatement
(.<=) exp = Upper (unwrap exp)

between ::
       (DimensionType d) => Expression d R -> (Val, Val) -> ConstraintStatement
between exp = Between (unwrap exp)

(.==) :: (DimensionType d) => Expression d R -> Val -> ConstraintStatement
(.==) exp val = Between (unwrap exp) (val, val)

infix 1 `between`, .>=, .<=, .==

-- | Return a map from variable name to the corresponding partial derivative node id
--   Partial derivatives in Expression Scalar Covector should be collected before passing to this function
--
partialDerivativeMaps :: Expression Scalar Covector -> Map String Int
partialDerivativeMaps df@(Expression dfId dfMp) =
    case retrieveNode dfId dfMp of
        Sum Covector ns -> Map.fromList $ mapMaybe getPartial ns
        _ -> Map.fromList $ mapMaybe getPartial [dfId]
  where
    getPartial :: Int -> Maybe (String, Int)
    getPartial nId
        | Mul Covector [partialId, dId] <- retrieveNode nId dfMp
        , DVar name <- retrieveNode dId dfMp = Just (name, partialId)
        | InnerProd Covector partialId dId <- retrieveNode nId dfMp
        , DVar name <- retrieveNode dId dfMp = Just (name, partialId)
        | otherwise = Nothing

-- |
--
data ProblemResult
    = ProblemValid Problem
    | ProblemInvalid String -- reason
    | NoVariables -- TODO - what about feasibility problems given constraints?
    deriving (Show)

-- | Construct a Problem from given objective function and constraints
--
constructProblem ::
       Expression Scalar R -> [String] -> Constraint -> ProblemResult
constructProblem objectiveFunction varList constraint
    | null varsList = NoVariables
    | Just reason <- checkError = ProblemInvalid reason
    | otherwise -- all the constraints are good
     =
        let boxConstraints = extractBoxConstraint constraint
            scalarConstraintsWithMp =
                extractScalarConstraint varsWithShape constraint
            scalarConstraints = map fst scalarConstraintsWithMp
            mergedMap =
                IM.unions $ [dfMp, fMp] ++ map snd scalarConstraintsWithMp
            rootNs =
                exIndex f :
                (map partialDerivativeId problemVariables ++
                 map constraintValueId scalarConstraints ++
                 concatMap constraintPartialDerivatives scalarConstraints)
            -- remove DVar nodes
            relevantNodes =
                Set.fromList $ topologicalSortManyRoots (mergedMap, rootNs)
            -- expression map
            problemExpressionMap :: ExpressionMap
            problemExpressionMap =
                IM.filterWithKey
                    (\nId _ -> Set.member nId relevantNodes)
                    mergedMap
            -- mem map
            problemMemMap =
                makeProblemMemMap
                    problemExpressionMap
                    (map nodeId problemVariables)
            -- objective id
            problemObjectiveId = fId
         in ProblemValid $
            Problem
                { variables = problemVariables
                , objectiveId = problemObjectiveId
                , memMap = problemMemMap
                , expressionMap = problemExpressionMap
                , boxConstraints = boxConstraints
                , scalarConstraints = scalarConstraints
                }
    -- set of name users consider as variable
    -- list of var names provided by users
  where
    userSpecifiedVars = Set.fromList varList
    -- list of all vars name in the expression, they can be variables or fixed values
    expressionVars = Set.fromList . map fst . expressionVarNodes $ f
    -- list of possible names that could be variables
    -- just because user says it is variable and it is var node doesn't mean it
    -- appears in the d: for example f = 0 * x, then this will just be 0 and dx doesn't appear
    -- when we take the derivative
    possibleVars = Set.intersection expressionVars userSpecifiedVars
    -- normalize and take the derivative and collect differentials
    f@(Expression fId fMp) = normalize objectiveFunction
    df@(Expression dfId dfMp) =
        collectDifferentials . exteriorDerivative possibleVars $ f
    -- Map from a variable name to partial derivative id in the problem's ExpressionMap
    name2PartialDerivativeId :: Map String Int
    name2PartialDerivativeId = partialDerivativeMaps df
    -- After this step we can know which are actually variables - those appear in name2PartialDerivativeId
    varsList = Map.keys name2PartialDerivativeId
    vars = Set.fromList varsList
    -- Map from a variable name to id in the problem's ExpressionMap
    name2Id :: Map String Int
    name2Id =
        Map.fromList $ filter ((`Set.member` vars) . fst) $ expressionVarNodes f
    variableDatas :: [(String, (Int, Int))]
    variableDatas =
        Map.toList $ Map.intersectionWith (,) name2Id name2PartialDerivativeId
    -- From a name to a Variable data
    toVariable :: (String, (Int, Int)) -> Variable
    toVariable (varName, (nId, pId)) =
        Variable {varName = varName, nodeId = nId, partialDerivativeId = pId}
    -- variables
    problemVariables :: [Variable]
    problemVariables = map toVariable variableDatas
    -- get variable shape
    variableShape :: String -> Shape
    variableShape name =
        let nId =
                fromMaybe (error "query non-variable name?") $
                Map.lookup name name2Id
         in retrieveShape nId fMp
    -- vars with shape 
    varsWithShape = zip varsList (map variableShape varsList)
    checkError =
        case constraint of
            NoConstraint -> Nothing
            Constraint cs -> firstJust checkConstraint cs
    checkConstraint :: ConstraintStatement -> Maybe String
    checkConstraint cs =
        case retrieveInternal n mp of
            (_, Var var) -- if it is a var, then should be box constraint
                | not (Set.member var vars) ->
                    Just $ var ++ " is not a variable"
                | any (not . compatible (variableShape var)) (getValCS cs) ->
                    Just $ "Bound for " ++ var ++ " is not in the right shape"
                | otherwise -> Nothing
            ([], _)
                | any (not . compatible []) (getValCS cs) ->
                    Just "Scalar expression must be bounded by scalar value"
                | otherwise -> Nothing
            _ ->
                Just
                    "Only scalar inequality and box constraint for variable are supported"
      where
        (mp, n) = getExpressionCS cs
    extractScalarConstraint ::
           [(String, Shape)]
        -> Constraint
        -> [(ScalarConstraint, ExpressionMap)]
    extractScalarConstraint _ NoConstraint = []
    extractScalarConstraint varsWithShape (Constraint css) =
        let scalarConstraints = filter (not . isBoxConstraint) css
            listScalarExpressions =
                Set.toList . Set.fromList . map getExpressionCS $
                scalarConstraints
            getBound (mp, n) = foldl update (ninf, inf) scalarConstraints
              where
                update (lb, ub) cs
                    | (mp, n) == getExpressionCS cs =
                        case cs of
                            Lower _ (VScalar val) -> (max lb val, ub)
                            Upper _ (VScalar val) -> (lb, min ub val)
                            Between _ (VScalar val1, VScalar val2) ->
                                (max lb val1, min ub val2)
                    | otherwise = (lb, ub)
            vars = map fst varsWithShape
            toScalarConstraint (mp, n) =
                let exp = Expression @Scalar @R n mp
                    g = normalize exp
                    dg =
                        introduceZeroPartialDerivatives varsWithShape .
                        collectDifferentials .
                        exteriorDerivative (Set.fromList vars) $
                        g
                    (lb, ub) = getBound (mp, n)
                    name2PartialDerivativeId :: Map String Int
                    name2PartialDerivativeId = partialDerivativeMaps dg
                    constraintPartialDerivatives
                        | Map.keys name2PartialDerivativeId == vars =
                            Map.elems name2PartialDerivativeId
                        | otherwise =
                            error
                                "variables of objective and constraints should be the same, but is different here"
                 in ( ScalarConstraint
                          { constraintValueId = exIndex g
                          , constraintPartialDerivatives =
                                constraintPartialDerivatives
                          , constraintLowerBound = lb
                          , constraintUpperBound = ub
                          }
                    , exMap g `IM.union` exMap dg)
         in map toScalarConstraint listScalarExpressions
    extractBoxConstraint :: Constraint -> [BoxConstraint]
    extractBoxConstraint NoConstraint = []
    extractBoxConstraint (Constraint css) =
        map toBoxConstraint . filter isBoxConstraint $ css
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

-- |
--
generateReadValuesCode :: String -> Val -> String -> Int -> Code
generateReadValuesCode name val address numDoubles =
    case val of
        VScalar value -> scoped ["*(" ++ address ++ ")" <<- show value]
        V1D _ -> readFileText (name ++ ".txt")
        V2D _ -> readFileText (name ++ ".txt")
        V3D _ -> readFileText (name ++ ".txt")
        V1DFile (TXT filePath) -> readFileText filePath
        V2DFile (TXT filePath) -> readFileText filePath
        V3DFile (TXT filePath) -> readFileText filePath
        V1DFile (HDF5 filePath dataset) -> readFileHD5 filePath dataset
        V2DFile (HDF5 filePath dataset) -> readFileHD5 filePath dataset
        V3DFile (HDF5 filePath dataset) -> readFileHD5 filePath dataset
  where
    readFileText filePath =
        scoped
            [ "printf(\"Reading " ++
              name ++ " from text file " ++ filePath ++ "....\\n\");"
            , "FILE *fp = fopen(\"" ++ filePath ++ "\", \"r\");"
            , "int i;"
            , "for (i = 0; i < " ++ show numDoubles ++ "; i++) { "
            , "  fscanf(fp, \"%lf\", " ++ address ++ " + i);"
            , "}"
            , "fclose(fp);"
            ]
    readFileHD5 filePath dataset =
        scoped
            [ "printf(\"Reading " ++
              dataset ++ " from HDF5 file " ++ filePath ++ "....\\n\");"
            , "hid_t file, dset;"
            , "file = H5Fopen (\"" ++
              filePath ++ "\", H5F_ACC_RDONLY, H5P_DEFAULT);"
            , "dset = H5Dopen (file, \"" ++ dataset ++ "\", H5P_DEFAULT);"
            , "H5Dread (dset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, " ++
              address ++ ");"
            , "H5Fclose (file);"
            , "H5Dclose (dset);"
            ]

-- |
--
data GenResult
    = Invalid String
    | Success (String -> IO ()) -- Give it the name of the folder of your solver, it will write all the files necessary

-- |
--
generateProblemCode :: ValMaps -> Problem -> GenResult
generateProblemCode valMaps Problem {..}
    | Just errorMsg <- checkError = Invalid errorMsg
    | otherwise =
        Success $ \folder -> do
            let codes =
                    defineStuffs ++
                    constraintCodes ++
                    readVals ++
                    evaluatingCodes ++
                    evaluateObjectiveCodes ++
                    evaluatePartialDerivatives ++
                    evaluateScalarConstraints ++
                    evaluateScalarConstraintsJacobian
            let writeVal (var, val) =
                    when (valueFromHaskell val) $ do
                        let str = unwords . map show . valElems $ val
                        writeFile (folder ++ "/" ++ var ++ ".txt") str
            let writeUpperBound var val =
                    when (valueFromHaskell val) $ do
                        let str = unwords . map show . valElems $ val
                            fileName = var ++ "_ub.txt"
                        writeFile (folder ++ "/" ++ fileName) str
            let writeLowerBound var val =
                    when (valueFromHaskell val) $ do
                        let str = unwords . map show . valElems $ val
                            fileName = var ++ "_lb.txt"
                        writeFile (folder ++ "/" ++ fileName) str
            mapM_ writeVal $ Map.toList valMaps
            let writeEach cnt =
                    case cnt of
                        BoxLower var val -> writeLowerBound var val
                        BoxUpper var val -> writeUpperBound var val
                        BoxBetween var (val1, val2) ->
                            writeLowerBound var val1 >> writeUpperBound var val2
            mapM_ writeEach boxConstraints
            writeFile (folder ++ "/problem.c") $ intercalate "\n" codes
    -- var nodes, can be optimizing variables or fixed values
  where
    vs :: [(String, Int)]
    vs = sortOn fst $ varNodesWithId expressionMap
    -- check if this name is optimizing variable or fixed value
    isVariable :: String -> Bool
    isVariable v = v `elem` map varName variables
    -- variables we're trying to optimize over
    vars :: [String]
    vars = map varName variables
    -- values node
    vals :: [String]
    vals = filter (not . isVariable) . map fst $ vs
    -- get shape of an optimizing variable
    variableShape :: String -> Shape
    variableShape name =
        let nId =
                case find ((== name) . varName) variables of
                    Just var -> nodeId var
                    _ -> error "not a variable but you're getting it's shape"
         in retrieveShape nId expressionMap
    -- variable we have along with shape
    varsWithShape = zip vars (map variableShape vars)
    -- size of each variable (product of it's shape)
    variableSizes = map (product . variableShape) vars
    totalVarSize = sum variableSizes
    -- MemMap
    entries = entryMap memMap
    -- Check if is variable or a fixed value
    checkError
        | Just name <- find (not . (`Map.member` valMaps)) vals =
            Just $ "No value provided for " ++ name
        | otherwise
        , let isOk (var, nId)
                  | Just val <- Map.lookup var valMaps =
                      compatible (retrieveShape nId expressionMap) val
                  | otherwise = True
        , Just (var, shape) <- find (not . isOk) vs =
            Just $
            "variable " ++
            var ++
            "is of shape " ++ show shape ++ " but the value provided is not"
        | otherwise = Nothing
    variableShapes = map (variableShape . varName) variables
    variableOffsets = map (getMemOffsetReal memMap . nodeId) variables
    partialDerivativeOffsets =
        map (getMemOffsetReal memMap . partialDerivativeId) variables
    objectiveOffset = getMemOffsetReal memMap objectiveId
    -- For both variables and values
    readValCodeEach (name, nId)
        | Just val <- Map.lookup name valMaps =
            generateReadValuesCode
                name
                val
                ("ptr + " ++ show offset)
                (product shape)
        | otherwise =
            scoped
                [ "printf(\"Init value for " ++
                  name ++
                  " is not provided, generating random for " ++
                  name ++ "....\\n\");"
                , "int i;"
                , "for (i = 0; i < " ++ show offset ++ "; i++) {"
                , "  ptr[" ++
                  show offset ++
                  " + " ++ i ++ "]" <<- "((double) rand() / (RAND_MAX))"
                , "}"
                ]
      where
        offset = memOffset memMap nId LookupR
        shape = retrieveShape nId expressionMap
    objectiveAndGradient = objectiveId : map partialDerivativeId variables
    -- MARK: codes part --
    defineStuffs =
        [ "#include <math.h>"
        , "#include <stdio.h>"
        , "#include <stdlib.h>"
        , "#include \"utils.c\""
        , "#include \"hdf5.h\""
        , ""
        , ""
        , "#define NUM_VARIABLES " ++ show (length variables)
        , "#define NUM_ACTUAL_VARIABLES " ++ show totalVarSize
        , "#define MEM_SIZE " ++ show (totalDoubles memMap)
        , ""
        , "// all the actual double variables are allocated"
        , "// one after another, starts from here"
        , "#define VARS_START_OFFSET " ++
          show (getMemOffsetReal memMap (nodeId . head $ variables))
        , ""
        , ""
        , "const char* var_name[NUM_VARIABLES] = {" ++
          (intercalate ", " . map (show . varName) $ variables) ++ "};"
        , "const int var_num_dim[NUM_VARIABLES] = {" ++
          (intercalate ", " . map (show . length) $ variableShapes) ++ "};"
        , "const int var_shape[NUM_VARIABLES][3] = {" ++
          (intercalate ", " . map toShapeString $ variableShapes) ++ "};"
        , "const int var_size[NUM_VARIABLES] = {" ++
          (intercalate ", " . map show $ variableSizes) ++ "};"
        , "const int var_offset[NUM_VARIABLES] = {" ++
          (intercalate ", " . map show $ variableOffsets) ++ "};"
        , "const int partial_derivative_offset[NUM_VARIABLES] = {" ++
          (intercalate ", " . map show $ partialDerivativeOffsets) ++ "};"
        , "const int objective_offset = " ++ show objectiveOffset ++ ";"
        , "double ptr[MEM_SIZE];"
        , ""
        , ""
        ]
    constraintCodes =
        let varPosition =
                take (length variableSizes) $ scanl (+) 0 variableSizes
            varWithPos = zip vars varPosition
            getPos name =
                snd .
                fromMaybe (error "get starting position variable") .
                find ((== name) . fst) $
                varWithPos
            readUpperBoundCode name val =
                generateReadValuesCode
                    (name ++ "_ub")
                    val
                    ("upper_bound + " ++ show (getPos name))
                    (product $ variableShape name)
            readLowerBoundCode name val =
                generateReadValuesCode
                    (name ++ "_lb")
                    val
                    ("lower_bound + " ++ show (getPos name))
                    (product $ variableShape name)
            readBounds =
                let readBoundCodeEach cnt =
                        case cnt of
                            BoxUpper name val -> readUpperBoundCode name val
                            BoxLower name val -> readLowerBoundCode name val
                            BoxBetween name (val1, val2) ->
                                readLowerBoundCode name val1 ++
                                readUpperBoundCode name val2
                 in concatMap readBoundCodeEach boxConstraints
            scalarConstraintDefineStuffs =
                [ "#define NUM_SCALAR_CONSTRAINT " ++
                  show (length scalarConstraints)
                , ""
                , "double sc_lower_bound[NUM_SCALAR_CONSTRAINT];"
                , "double sc_upper_bound[NUM_SCALAR_CONSTRAINT];"
                , "const int sc_offset[NUM_SCALAR_CONSTRAINT] = {" ++
                  (intercalate "," .
                   map (show . getMemOffsetReal memMap . constraintValueId) $
                   scalarConstraints) ++
                  "};"
                , ""
                , "const int sc_partial_derivative_offset[NUM_SCALAR_CONSTRAINT][NUM_VARIABLES] = {" ++
                  intercalate
                      ", "
                      [ "{" ++
                      intercalate
                          ","
                          (map (show . getMemOffsetReal memMap) .
                           constraintPartialDerivatives $
                           sc) ++
                      "}"
                      | sc <- scalarConstraints
                      ] ++
                  "};"
                , ""
                , ""
                ]
            readBoundScalarConstraints =
                [ "sc_lower_bound[" ++ show i ++ "] = " ++ d2s val ++ ";"
                | (i, val) <-
                      zip [0 ..] $ map constraintLowerBound scalarConstraints
                ] ++
                [ "sc_upper_bound[" ++ show i ++ "] = " ++ d2s val ++ ";"
                | (i, val) <-
                      zip [0 ..] $ map constraintUpperBound scalarConstraints
                ]
         in [ "const int bound_pos[NUM_VARIABLES] = {" ++
              (intercalate ", " . map show $ varPosition) ++ "};"
            , "double lower_bound[NUM_ACTUAL_VARIABLES];"
            , "double upper_bound[NUM_ACTUAL_VARIABLES];"
            , ""
            , ""
            ] ++
            scalarConstraintDefineStuffs ++
            [ "void read_bounds() {" --
            , "  for (int i = 0; i < NUM_ACTUAL_VARIABLES; i++) {"
            , "    lower_bound[i] = -INFINITY;"
            , "    upper_bound[i] = INFINITY;"
            , "  }"
            ] ++
            space 2 readBounds ++ --
            space 2 readBoundScalarConstraints ++ --
            ["}"]
    readVals =
        ["void read_values() {"] ++ --
        ["  srand(time(NULL));"] ++
        space 2 (concatMap readValCodeEach vs) ++ --
        ["}"]
    evaluatingCodes =
        ["void evaluate_partial_derivatives_and_objective() {"] ++
        space
            2
            (generateEvaluatingCodes
                 memMap
                 (expressionMap, objectiveAndGradient)) ++
        ["}"]
    evaluateObjectiveCodes =
        ["void evaluate_objective() {"] ++
        space 2 (generateEvaluatingCodes memMap (expressionMap, [objectiveId])) ++
        ["}"]
    evaluatePartialDerivatives =
        ["void evaluate_partial_derivatives() {"] ++
        space
            2
            (generateEvaluatingCodes
                 memMap
                 (expressionMap, map partialDerivativeId variables)) ++
        ["}"]
    evaluateScalarConstraints =
        ["void evaluate_scalar_constraints() {"] ++
        space
            2
            (generateEvaluatingCodes
                 memMap
                 (expressionMap, map constraintValueId scalarConstraints)) ++
        ["}"]
    evaluateScalarConstraintsJacobian =
        ["void evaluate_scalar_constraints_jacobian() {"] ++
        space
            2
            (generateEvaluatingCodes
                 memMap
                 ( expressionMap
                 , concatMap constraintPartialDerivatives scalarConstraints)) ++
        ["}"]

-- |
--
compatible :: Shape -> Val -> Bool
compatible shape v =
    case (shape, v) of
        ([], VScalar val) -> True
        ([x], V1D arr1d)
            | bounds arr1d == (0, x - 1) -> True
        ([x, y], V2D arr2d)
            | bounds arr2d == ((0, 0), (x - 1, y - 1)) -> True
        ([x, y, z], V3D arr3d)
            | bounds arr3d == ((0, 0, 0), (x - 1, y - 1, z - 1)) -> True
        ([x], V1DFile {}) -> True
        ([x, y], V2DFile {}) -> True
        ([x, y, z], V3DFile {}) -> True
        _ -> False

-- | 
--
toShapeString :: Shape -> String
toShapeString shape
    | length shape < 3 =
        "{" ++
        (intercalate ", " . map show $ shape ++ replicate (3 - length shape) 1) ++
        "}"
    | otherwise = "{" ++ (intercalate ", " . map show $ shape) ++ "}"

instance Show Problem where
    show Problem {..} =
        intercalate "\n" $
        [ "-------------- Objective Function --------------"
        , debugPrint (expressionMap, objectiveId)
        ] ++
        map showPartial variables
      where
        showPartial var =
            intercalate
                "\n"
                [ "----------------∂f/∂" ++ varName var ++ "-------------"
                , debugPrint (expressionMap, partialDerivativeId var)
                ]
