{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module HashedSolver where

import qualified Data.IntMap as IM
import Data.List (intercalate)
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
            Map.fromList $ filter (flip Set.member vars . fst) $ varNodes f
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
generateProblemCode :: ValMaps -> Problem -> Code
generateProblemCode valMaps Problem {..} =
    defineStuffs ++ assignVals ++ evaluatingCodes
  where
    entries = entryMap memMap
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
        , "const int var_size[NUM_VARIABLES] = {" ++
          (intercalate "," . map show $ variableSizes) ++ "};"
        , "const int var_offset[NUM_VARIABLES] = {" ++
          (intercalate "," . map show $ variableOffsets) ++ "};"
        , "const int partial_derivative_offset[NUM_VARIABLES] = {" ++
          (intercalate "," . map show $ partialDerivativeOffsets) ++ "};"
        , "const int objective_offset = " ++ show objectiveOffset ++ ";"
        , "double ptr[MEM_SIZE];"
        , ""
        , ""
        ]
    assignVals =
        ["void assign_values() {"] ++
        space 2 (generateAssignValueCodes valMaps memMap expressionMap) ++ --
        ["}", "", ""]
    evaluatingCodes =
        ["void evaluate_partial_derivatives_and_objective() {"] ++
        space 2 (generateEvaluatingCodes memMap (expressionMap, evaluatingIds)) ++
        ["}"]

-- | 
--
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
