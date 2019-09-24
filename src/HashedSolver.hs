{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module HashedSolver where

import Data.Array (bounds)
import qualified Data.IntMap as IM
import Data.List (find, intercalate)
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
    , ET(..)
    , Expression(..)
    , ExpressionMap
    , Node(..)
    , R
    , Scalar
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
        }

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
                [ "----------------∂f/∂(" ++ varName var ++ ")-------------"
                , debugPrint (expressionMap, partialDerivativeId var)
                ]

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

-- | Construct a Problem from given objective function
--
constructProblem :: Expression Scalar R -> Set String -> Problem
constructProblem notSimplifedF vars =
    let f@(Expression fId fMp) = normalize notSimplifedF
        df@(Expression dfId dfMp) =
            collectDifferentials . exteriorDerivative vars $ f
        -- Map from a variable name to id in the problem's ExpressionMap
        name2Id :: Map String Int
        name2Id =
            Map.fromList $
            filter (flip Set.member vars . fst) $ expressionVarNodes f
        -- Map from a variable name to partial derivative id in the problem's ExpressionMap
        name2PartialDerivativeId :: Map String Int
        name2PartialDerivativeId = partialDerivativeMaps df
        -- Root ids, including the objective function and all partial derivatives
        rootNs = exIndex f : (map snd . Map.toList $ name2PartialDerivativeId)
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
        problemVariables = map toVariable . Set.toList $ vars
        mergedMap = IM.union dfMp fMp
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
     in Problem
            { variables = problemVariables
            , objectiveId = problemObjectiveId
            , memMap = problemMemMap
            , expressionMap = problemExpressionMap
            }

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
            let codes = defineStuffs ++ readVals ++ evaluatingCodes
            let writeVal (var, val)
                    | var `elem` map varName variables = do
                        let str = unwords . map show . valElems $ val
                        writeFile
                            (folder ++ "/" ++ "init_" ++ var ++ ".txt")
                            str
                    | otherwise = do
                        let str = unwords . map show . valElems $ val
                        writeFile
                            (folder ++ "/" ++ "fixed_" ++ var ++ ".txt")
                            str
            mapM_ writeVal $ Map.toList valMaps
            writeFile (folder ++ "/problem.c") $ intercalate "\n" codes
  where
    vs = varsAndShape expressionMap
    compatible shape v =
        case (shape, v) of
            ([], VScalar val) -> True
            ([x], V1D arr1d)
                | bounds arr1d == (0, x - 1) -> True
            ([x, y], V2D arr2d)
                | bounds arr2d == ((0, 0), (x - 1, y - 1)) -> True
            ([x, y, z], V3D arr3d)
                | bounds arr3d == ((0, 0, 0), (x - 1, y - 1, z - 1)) -> True
            _ -> False
    isVariable v = v `elem` map varName variables
    checkError
        | Just var <-
             find (not . (`Map.member` valMaps)) . map varName $ variables =
            Just $ "No initial value for variable " ++ var
        | Just var <- find (not . (`Map.member` valMaps)) . map fst $ vs =
            Just $ "No value provided for fixed parameter " ++ var
        | otherwise =
            let isOk (var, nId) =
                    compatible
                        (retrieveShape nId expressionMap)
                        (fromJust (var `Map.lookup` valMaps))
             in case find (not . isOk) vs of
                    Just (var, shape) ->
                        Just $
                        "variable " ++
                        var ++
                        "is of shape " ++
                        show shape ++ " but the value provided is not"
                    Nothing -> Nothing
    readValCodeEach (var, nId)
        | var `elem` map varName variables =
            generateReadValuesCode
                ("init_" ++ var ++ ".txt")
                offset
                (product shape)
        | otherwise =
            generateReadValuesCode
                ("fixed_" ++ var ++ ".txt")
                offset
                (product shape)
      where
        offset = memOffset memMap nId LookupR
        shape = retrieveShape nId expressionMap
    entries = entryMap memMap
    toShapeString shape
        | length shape < 3 =
            "{" ++
            (intercalate ", " . map show $
             shape ++ replicate (3 - length shape) 1) ++
            "}"
        | otherwise = "{" ++ (intercalate ", " . map show $ shape) ++ "}"
    variableShapes = map (flip retrieveShape expressionMap . nodeId) variables
    variableSizes =
        map
            (product . thd3 . fromJust . flip IM.lookup entries . nodeId)
            variables
    variableOffsets =
        map (fst3 . fromJust . flip IM.lookup entries . nodeId) variables
    partialDerivativeOffsets =
        map
            (fst3 . fromJust . flip IM.lookup entries . partialDerivativeId)
            variables
    objectiveOffset = fst3 . fromJust . flip IM.lookup entries $ objectiveId
    evaluatingIds = objectiveId : map partialDerivativeId variables
    defineStuffs =
        [ "#include <math.h>"
        , "#include <stdio.h>"
        , "#include <stdlib.h>"
        , "#include \"utils.c\""
        , ""
        , ""
        , "#define NUM_VARIABLES " ++ show (length variables)
        , "#define MEM_SIZE " ++ show (totalDoubles memMap)
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
    readVals =
        ["void read_values() {"] ++ --
        space 2 (concatMap readValCodeEach vs) ++ --
        ["}"]
    evaluatingCodes =
        ["void evaluate_partial_derivatives_and_objective() {"] ++
        space 2 (generateEvaluatingCodes memMap (expressionMap, evaluatingIds)) ++
        ["}"]

getMinimumGradientDescent :: Code -> IO ()
getMinimumGradientDescent codes = do
    let filePath = "algorithms/gradient_descent/problem.c"
    TIO.writeFile filePath $ T.intercalate "\n" $ map T.pack codes
    (exitCode, stdout, stderr) <-
        readProcessWithExitCode "make" ["-C", "algorithms/gradient_descent"] ""
    putStrLn stdout
    putStrLn stderr
    (exitCode, stdout, stderr) <-
        readProcessWithExitCode
            "algorithms/gradient_descent/gradient_descent"
            []
            ""
    putStrLn stdout
    putStrLn stderr
