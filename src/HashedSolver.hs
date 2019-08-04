module HashedSolver where

import qualified Data.IntMap as IM
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import HashedCollect
import HashedDerivative
import HashedExpression
    ( Covector
    , ET(..)
    , Expression(..)
    , ExpressionMap
    , Node(..)
    , R
    , Zero
    , exIndex
    , exMap
    )
import HashedInner
import HashedNode
import HashedToC
import HashedUtils

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
    deriving (Show)

-- | Return a map from variable name to the corresponding partial derivative node id
--   Partial derivatives in Expression Zero Covector should be collected before passing to this function
--
partialDerivativeMaps :: Expression Zero Covector -> Map String Int
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
constructProblem :: Expression Zero R -> Problem
constructProblem f@(Expression fId fMp) =
    let df@(Expression dfId dfMp) = collectDifferentials . derivativeAllVars $ f
        allVars = varSet f
        -- Set of all variable names
        vars :: Set String
        vars = Set.fromList . map fst $ allVars
        -- Map from a variable name to id in the problem's ExpressionMap
        name2Id :: Map String Int
        name2Id = Map.fromList allVars
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
        relevantNs = topologicalSortManyRoots mergedMap rootNs
        -- expression map
        problemExpressionMap :: ExpressionMap
        problemExpressionMap =
            IM.fromList $
            map (\nId -> (nId, fromJust $ IM.lookup nId mergedMap)) relevantNs
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
