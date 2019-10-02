{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module HashedSolver where

import Control.Monad (unless)
import Data.Array (bounds)
import qualified Data.IntMap as IM
import Data.List (find, intercalate)
import Data.List.Extra (firstJust)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Tuple.HT (fst3, thd3)
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

ninf = (-1 / 0) :: Double

inf = (1 / 0) :: Double

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
        , boxConstraints :: Maybe [BoxConstraint]
        , scalarConstraints :: Maybe [ScalarConstraint]
        }

-- |
--
data BoxConstraint
    = BoxUpper String Val
    | BoxLower String Val
    | BoxBetween String (Val, Val)

data ScalarConstraint =
    ScalarConstraint
        { constraintValueId :: Int
        , constraintPartialDerivatives :: [Int]
        , constraintLowerBound :: Double
        , constraintUpperBound :: Double
        }

-- |
--
data Constraint
    = NoConstraint
    | BoxConstraint [ConstraintStatement]
    | IPOPTConstraint [ConstraintStatement]
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
    deriving (Show)

extractBoxConstraint :: Constraint -> Maybe [BoxConstraint]
extractBoxConstraint constraint =
    case constraint of
        NoConstraint -> Nothing
        BoxConstraint css -> Just . map toBoxConstraint $ css
        IPOPTConstraint css ->
            Just . map toBoxConstraint . filter isBoxConstraint $ css
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

extractScalarConstraint ::
       [String] -> Constraint -> Maybe [(ScalarConstraint, ExpressionMap)]
extractScalarConstraint vars constraint =
    case constraint of
        NoConstraint -> Nothing
        BoxConstraint _ -> Nothing
        IPOPTConstraint css ->
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
                toScalarConstraint (mp, n) =
                    let exp = Expression @Scalar @R n mp
                        g = normalize exp
                        dg =
                            collectDifferentials .
                            exteriorDerivative (Set.fromList vars) $
                            g
                        (lb, ub) = getBound (mp, n)
                        name2PartialDerivativeId :: Map String Int
                        name2PartialDerivativeId = partialDerivativeMaps dg
                        constraintPartialDerivatives =
                            map
                                (fromJust .
                                 flip Map.lookup name2PartialDerivativeId)
                                vars
                     in ( ScalarConstraint
                              { constraintValueId = exIndex g
                              , constraintPartialDerivatives =
                                    constraintPartialDerivatives
                              , constraintLowerBound = lb
                              , constraintUpperBound = ub
                              }
                        , exMap g `IM.union` exMap dg)
             in Just $ map toScalarConstraint listScalarExpressions

-- | Construct a Problem from given objective function
--
constructProblem ::
       Expression Scalar R -> [String] -> Constraint -> ProblemResult
constructProblem objectiveFunction varList constraint
    | Just reason <- checkError = ProblemInvalid reason
    | otherwise -- all the constraints are good 
     =
        let boxConstraints = extractBoxConstraint constraint
            scalarConstraints = extractScalarConstraint vars constraint
            mergedMap =
                case scalarConstraints of
                    Nothing -> IM.union dfMp fMp
                    Just scalarConstraintAndExMap ->
                        IM.unions $
                        [dfMp, fMp] ++ map snd scalarConstraintAndExMap
            rootNs =
                case scalarConstraints of
                    Nothing ->
                        exIndex f : map partialDerivativeId problemVariables
                    Just scalarConstraintAndExMap ->
                        exIndex f :
                        (map partialDerivativeId problemVariables ++
                         map (constraintValueId . fst) scalarConstraintAndExMap ++
                         concatMap
                             (constraintPartialDerivatives . fst)
                             scalarConstraintAndExMap)
            topologicalOrder = topologicalSortManyRoots (mergedMap, rootNs)
            -- expression map
            problemExpressionMap :: ExpressionMap
            problemExpressionMap =
                IM.fromList $
                map
                    (\nId -> (nId, fromJust $ IM.lookup nId mergedMap))
                    topologicalOrder
            -- mem map
            problemMemMap = makeMemMap problemExpressionMap
            -- objective id
            problemObjectiveId = fId
         in ProblemValid $
            Problem
                { variables = problemVariables
                , objectiveId = problemObjectiveId
                , memMap = problemMemMap
                , expressionMap = problemExpressionMap
                , boxConstraints = boxConstraints
                , scalarConstraints = fmap (map fst) scalarConstraints
                }
  where
    f@(Expression fId fMp) = normalize objectiveFunction
    varNodesName = Set.fromList . map fst $ expressionVarNodes f
    -- set of vars
    varSet = Set.fromList varList `Set.intersection` varNodesName
    df@(Expression dfId dfMp) =
        collectDifferentials . exteriorDerivative varSet $ f
    -- Map from a variable name to id in the problem's ExpressionMap
    name2Id :: Map String Int
    name2Id =
        Map.fromList $
        filter (flip Set.member varSet . fst) $ expressionVarNodes f
    -- Map from a variable name to partial derivative id in the problem's ExpressionMap
    name2PartialDerivativeId :: Map String Int
    name2PartialDerivativeId = partialDerivativeMaps df
    -- Final valid list of vars
    vars = Set.elems varSet
    -- From a name to a Variable data
    toVariable :: String -> Variable
    toVariable varName =
        Variable
            { varName = varName
            , nodeId = fromJust $ Map.lookup varName name2Id
            , partialDerivativeId =
                  fromJust $ Map.lookup varName name2PartialDerivativeId
            }
    -- variables
    problemVariables :: [Variable]
    problemVariables = map toVariable vars
    -- get variable shape
    variableShape :: String -> Shape
    variableShape name =
        let nId = fromJust $ Map.lookup name name2Id
         in retrieveShape nId fMp
    checkError =
        case constraint of
            NoConstraint -> Nothing
            BoxConstraint cs -> firstJust checkBoxConstraint cs
            IPOPTConstraint cs -> firstJust checkCombinedConstraint cs
    checkBoxConstraint :: ConstraintStatement -> Maybe String
    checkBoxConstraint cs =
        case retrieveNode n mp of
            Var var
                | not (Set.member var varSet) ->
                    Just $ var ++ " is not a variable"
                | any (not . compatible (variableShape var)) (getValCS cs) ->
                    Just $ "Bound for " ++ var ++ " is not in the right shape"
                | otherwise -> Nothing
            _ -> Just "Box constraint only apply for stand-alone variable"
      where
        (mp, n) = getExpressionCS cs
    checkCombinedConstraint :: ConstraintStatement -> Maybe String
    checkCombinedConstraint cs =
        case retrieveInternal n mp of
            (_, Var var) -- if it is a var, then should be box constraint
                | not (Set.member var varSet) ->
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

-- |
--
generateReadValuesCode :: String -> Val -> String -> Int -> Code
generateReadValuesCode dataset val address numDoubles =
    case val of
        VScalar value -> scoped ["*(" ++ address ++ ")" <<- show value]
        V1D _ -> readFileText (dataset ++ ".txt")
        V2D _ -> readFileText (dataset ++ ".txt")
        V3D _ -> readFileText (dataset ++ ".txt")
        V1DFile TXT fileName -> readFileText fileName
        V2DFile TXT fileName -> readFileText fileName
        V3DFile TXT fileName -> readFileText fileName
        V1DFile HDF5 fileName -> readFileHD5 fileName
        V2DFile HDF5 fileName -> readFileHD5 fileName
        V3DFile HDF5 fileName -> readFileHD5 fileName
  where
    readFileText fileName =
        scoped
            [ "printf(\"Reading " ++
              dataset ++ " from text file " ++ fileName ++ "....\\n\");"
            , "FILE *fp = fopen(\"" ++ fileName ++ "\", \"r\");"
            , "int i;"
            , "for (i = 0; i < " ++ show numDoubles ++ "; i++) { "
            , "  fscanf(fp, \"%lf\", " ++ address ++ " + i);"
            , "}"
            , "fclose(fp);"
            ]
    readFileHD5 fileName =
        scoped
            [ "printf(\"Reading " ++
              dataset ++ " from HDF5 file " ++ fileName ++ "....\\n\");"
            , "hid_t file, dset;"
            , "file = H5Fopen (\"" ++
              fileName ++ "\", H5F_ACC_RDONLY, H5P_DEFAULT);"
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
                    unless (null $ valElems val) $ do
                        let str = unwords . map show . valElems $ val
                        writeFile (folder ++ "/" ++ var ++ ".txt") str
            let writeUpperBound var val =
                    unless (null . valElems $ val) $ do
                        let str = unwords . map show . valElems $ val
                            fileName = var ++ "_ub.txt"
                        writeFile (folder ++ "/" ++ fileName) str
            let writeLowerBound var val =
                    unless (null . valElems $ val) $ do
                        let str = unwords . map show . valElems $ val
                            fileName = var ++ "_lb.txt"
                        writeFile (folder ++ "/" ++ fileName) str
            mapM_ writeVal $ Map.toList valMaps
            case boxConstraints of
                Just cnts ->
                    let writeEach cnt =
                            case cnt of
                                BoxLower var val -> writeLowerBound var val
                                BoxUpper var val -> writeUpperBound var val
                                BoxBetween var (val1, val2) ->
                                    writeLowerBound var val1 >>
                                    writeUpperBound var val2
                     in mapM_ writeEach cnts
                _ -> return ()
            writeFile (folder ++ "/problem.c") $ intercalate "\n" codes
    -- var nodes, can be optimizing variables or fixed values
  where
    vs :: [(String, Int)]
    vs = varNodesWithId expressionMap
    -- check if this name is optimizing variable or fixed value
    isVariable :: String -> Bool
    isVariable v = v `elem` map varName variables
    -- variables we're trying to optimize over
    vars :: [String]
    vars = map varName variables
    -- get shape of an optimizing variable
    variableShape :: String -> Shape
    variableShape name =
        let nId = nodeId . fromJust . find ((== name) . varName) $ variables
         in retrieveShape nId expressionMap
    -- variable we have along with shape
    varsWithShape = zip vars (map variableShape vars)
    -- size of each variable (product of it's shape)
    variableSizes = map (product . variableShape) vars
    totalVarSize = sum variableSizes
    -- MemMap
    entries = entryMap memMap
    getMemOffset = fst3 . fromJust . flip IM.lookup entries
    -- Check if is variable or a fixed value
    checkError
        | Just var <-
             find (not . (`Map.member` valMaps)) . map varName $ variables =
            Just $ "No initial value for variable " ++ var
        | Just var <- find (not . (`Map.member` valMaps)) . map fst $ vs =
            Just $ "No value provided for fixed parameter " ++ var
        | otherwise
        , let isOk (var, nId) =
                  compatible
                      (retrieveShape nId expressionMap)
                      (fromJust (var `Map.lookup` valMaps))
        , Just (var, shape) <- find (not . isOk) vs =
            Just $
            "variable " ++
            var ++
            "is of shape " ++ show shape ++ " but the value provided is not"
        | otherwise = Nothing
    variableShapes = map (variableShape . varName) variables
    variableOffsets = map (getMemOffset . nodeId) variables
    partialDerivativeOffsets =
        map (getMemOffset . partialDerivativeId) variables
    objectiveOffset = getMemOffset objectiveId
    -- For both variables and fixed value
    readValCodeEach (var, nId) =
        generateReadValuesCode
            var
            (fromJust $ Map.lookup var valMaps)
            ("ptr + " ++ show offset)
            (product shape)
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
            getPos name = snd . fromJust . find ((== name) . fst) $ varWithPos
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
                case boxConstraints of
                    Just cnts ->
                        let readBoundCodeEach cnt =
                                case cnt of
                                    BoxUpper name val ->
                                        readUpperBoundCode name val
                                    BoxLower name val ->
                                        readLowerBoundCode name val
                                    BoxBetween name (val1, val2) ->
                                        readLowerBoundCode name val1 ++
                                        readUpperBoundCode name val2
                         in concatMap readBoundCodeEach cnts
                    _ -> []
            scalarConstraintDefineStuffs =
                case scalarConstraints of
                    Just scs ->
                        [ "#define NUM_SCALAR_CONSTRAINT " ++ show (length scs)
                        , ""
                        , "double sc_lower_bound[NUM_SCALAR_CONSTRAINT];"
                        , "double sc_upper_bound[NUM_SCALAR_CONSTRAINT];"
                        , "const int sc_offset[NUM_SCALAR_CONSTRAINT] = {" ++
                          (intercalate "," .
                           map (show . getMemOffset . constraintValueId) $
                           scs) ++
                          "};"
                        , ""
                        , "const int sc_partial_derivative_offset[NUM_SCALAR_CONSTRAINT][NUM_VARIABLES] = {" ++
                          intercalate
                              ", "
                              [ "{" ++
                              intercalate
                                  ","
                                  (map (show . getMemOffset) .
                                   constraintPartialDerivatives $
                                   sc) ++
                              "}"
                              | sc <- scs
                              ] ++
                          "};"
                        , ""
                        , ""
                        ]
                    _ -> []
            readBoundScalarConstraints =
                case scalarConstraints of
                    Just scs ->
                        [ "sc_lower_bound[" ++
                        show i ++ "] = " ++ d2s val ++ ";"
                        | (i, val) <- zip [0 ..] $ map constraintLowerBound scs
                        ] ++
                        [ "sc_upper_bound[" ++
                        show i ++ "] = " ++ d2s val ++ ";"
                        | (i, val) <- zip [0 ..] $ map constraintUpperBound scs
                        ]
                    _ -> []
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
        case scalarConstraints of
            Just scs ->
                ["void evaluate_scalar_constraints() {"] ++
                space
                    2
                    (generateEvaluatingCodes
                         memMap
                         (expressionMap, map constraintValueId scs)) ++
                ["}"]
            _ -> []
    evaluateScalarConstraintsJacobian =
        case scalarConstraints of
            Just scs ->
                ["void evaluate_scalar_constraints_jacobian() {"] ++
                space
                    2
                    (generateEvaluatingCodes
                         memMap
                         ( expressionMap
                         , concatMap constraintPartialDerivatives scs)) ++
                ["}"]
            _ -> []

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
