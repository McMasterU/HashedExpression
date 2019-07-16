-------------------------------------------------------------------------------
-- | Inner Hashed functions, there are functions as
-- how to combine expression with options
--
--
-------------------------------------------------------------------------------
{-# LANGUAGE TupleSections #-}
module HashedInner where

import Data.Set (Set, empty, insert, member)
import qualified Data.Set as Set
import Prelude hiding ((-))
import qualified Data.IntMap.Strict as IM
import Data.List (sort, sortBy, sortOn)
import Debug.Trace (traceShowId)
import HashedExpression
import HashedHash
import HashedNode
import HashedUtils
import Data.Maybe (mapMaybe)
import Data.Graph (topSort, buildG)

-- | Enable this when we want to check for conflict when merging two expressions
-- Different node of two maps could have the same hash (which we hope never happens)
--
checkMergeConflict :: Bool
checkMergeConflict = False

-- | TODO: Check if 2 different nodes from 2 maps have the same hash
--
safeUnion :: ExpressionMap -> ExpressionMap -> ExpressionMap
safeUnion = IM.union

-- |
--
data ElementOutcome
    = ElementSpecific ET
    | ElementDefault -- Highest element (R < C < Covector)

data NodeOutcome
    = OpOne (Arg -> Node)
    | OpOneElement (ET -> Arg -> Node) ElementOutcome
    | OpTwo (Arg -> Arg -> Node)
    | OpTwoElement (ET -> Arg -> Arg -> Node) ElementOutcome
    | OpMany (Args -> Node)
    | OpManyElement (ET -> Args -> Node) ElementOutcome

data ShapeOutcome
    = ShapeSpecific Shape
    | ShapeDefault -- Highest shape (shape with longest length)
    | ShapeBranches -- Same as branches' shape

data OperationOption
    = Normal NodeOutcome ShapeOutcome
    | Condition (ConditionArg -> [BranchArg] -> Node)

-- |
--
unwrap :: Expression d et -> (ExpressionMap, Int)
unwrap (Expression n mp) = (mp, n)

wrap :: (ExpressionMap, Int) -> Expression d et
wrap = uncurry $ flip Expression

-- |
--
highestShape :: [(ExpressionMap, Int)] -> Shape
highestShape = last . sortOn length . map (uncurry $ flip retrieveShape)

highestShapeWithContext :: ExpressionMap -> [Int] -> Shape
highestShapeWithContext mp = last . sortOn length . map (`retrieveShape` mp)

-- | R < C < Covector
--
highestElementType :: [(ExpressionMap, Int)] -> ET
highestElementType = maximum . map (uncurry $ flip retrieveElementType)

highestElementTypeWithContext :: ExpressionMap -> [Int] -> ET
highestElementTypeWithContext mp = maximum . map (`retrieveElementType` mp)

-- | The apply function that is used everywhere
--
apply :: OperationOption -> [(ExpressionMap, Int)] -> (ExpressionMap, Int)
apply option exprs =
    addEntryWithContext mergedMap mergedMap option (map snd exprs)
  where
    mergedMap = IM.unions . map fst $ exprs

-- |
--
addEntryWithContext ::
       ExpressionMap
    -> ExpressionMap
    -> OperationOption
    -> [Int]
    -> (ExpressionMap, Int)
addEntryWithContext contextMp mp (Normal nodeOutcome shapeOutcome) ns =
    let shape =
            case shapeOutcome of
                ShapeSpecific s -> s
                _ -> highestShapeWithContext contextMp ns
        elementType elementOutcome =
            case elementOutcome of
                ElementSpecific et -> et
                _ -> highestElementTypeWithContext contextMp ns
        node =
            case (nodeOutcome, ns) of
                (OpOne op, [arg]) -> op arg
                (OpOneElement op elm, [arg]) -> op (elementType elm) arg
                (OpTwo op, [arg1, arg2]) -> op arg1 arg2
                (OpTwoElement op elm, [arg1, arg2]) ->
                    op (elementType elm) arg1 arg2
                (OpMany op, args) -> op args
                (OpManyElement op elm, args) -> op (elementType elm) args
                _ -> error "HashedInner.applySameScope"
     in addInternal mp (shape, node)
addEntryWithContext contextMp mp (Condition op) ns@(conditionN:branchesNs) =
    let headBranchN = head branchesNs
        shape = retrieveShape headBranchN contextMp
        node = op conditionN branchesNs
     in addInternal mp (shape, node)

-- | General multiplication and sum
--
mulMany :: [(ExpressionMap, Int)] -> (ExpressionMap, Int)
mulMany = apply $ naryET Mul ElementDefault

sumMany :: [(ExpressionMap, Int)] -> (ExpressionMap, Int)
sumMany = apply $ naryET Sum ElementDefault

-- |
--
hasShape :: OperationOption -> Shape -> OperationOption
hasShape (Normal nodeOutcome _) specificShape =
    Normal nodeOutcome (ShapeSpecific specificShape)
hasShape option _ = option

applyBinary ::
       OperationOption
    -> Expression d1 et1
    -> Expression d2 et2
    -> Expression d3 et3
applyBinary option e1 e2 = wrap . apply option $ [unwrap e1, unwrap e2]

applyUnary :: OperationOption -> Expression d1 et1 -> Expression d2 et2
applyUnary option e1 = wrap . apply option $ [unwrap e1]

applyNary :: OperationOption -> [Expression d1 et1] -> Expression d2 et2
applyNary option = wrap . apply option . map unwrap

applyConditionAry ::
       OperationOption
    -> Expression d et1
    -> [Expression d et2]
    -> Expression d et2
applyConditionAry option e branches =
    wrap . apply option $ unwrap e : map unwrap branches

-- | binary operations
--
binary :: (Arg -> Arg -> Node) -> OperationOption
binary op = Normal (OpTwo op) ShapeDefault

binaryET :: (ET -> Arg -> Arg -> Node) -> ElementOutcome -> OperationOption
binaryET op elm = Normal (OpTwoElement op elm) ShapeDefault

-- | unary operations
--
unary :: (Arg -> Node) -> OperationOption
unary op = Normal (OpOne op) ShapeDefault

unaryET :: (ET -> Arg -> Node) -> ElementOutcome -> OperationOption
unaryET op elm = Normal (OpOneElement op elm) ShapeDefault

-- | n-ary operations
--
nary :: (Args -> Node) -> OperationOption
nary op = Normal (OpMany op) ShapeDefault

naryET :: (ET -> Args -> Node) -> ElementOutcome -> OperationOption
naryET op elm = Normal (OpManyElement op elm) ShapeDefault

-- | branch operation
--
conditionAry :: (ConditionArg -> [BranchArg] -> Node) -> OperationOption
conditionAry = Condition

-- | Simplification will return an ExpressionDiff instead of the whole Expression to speed things up
--
data ExpressionDiff =
    ExpressionDiff
        { extraEntries :: ExpressionMap -- Extra entries we need to add to the original Expression Map
        , newRootId :: Int -- New root of the expression (can change, can be the same)
        }
    deriving (Eq, Ord, Show)

-- |
--
diffConst :: Shape -> Double -> ExpressionDiff
diffConst shape val = ExpressionDiff mp n
  where
    (mp, n) = aConst shape val

-- |
--
mulManyDiff :: ExpressionMap -> [ExpressionDiff] -> ExpressionDiff
mulManyDiff contextMp = applyDiff contextMp (naryET Mul ElementDefault)

-- |
--
sumManyDiff :: ExpressionMap -> [ExpressionDiff] -> ExpressionDiff
sumManyDiff contextMp = applyDiff contextMp (naryET Sum ElementDefault)

-- |
--
applyDiff ::
       ExpressionMap -> OperationOption -> [ExpressionDiff] -> ExpressionDiff
applyDiff contextMp option operands = ExpressionDiff resExtraEntries resRootId
  where
    mergedExtraEntries = IM.unions . map extraEntries $ operands
    updatedContextMp = IM.union mergedExtraEntries contextMp
    ns = map newRootId operands
    (resExtraEntries, resRootId) =
        addEntryWithContext updatedContextMp mergedExtraEntries option ns

-- |
--
withoutExtraEntry :: Int -> ExpressionDiff
withoutExtraEntry = ExpressionDiff IM.empty

-- | Topological sort the expression map, all the dependencies will appear before the depended node
--
topologicalSort :: (ExpressionMap, Int) -> [Int]
topologicalSort expr@(mp, n) =
    reverse . mapMaybe (`IM.lookup` vertices2nId) $ topSort graph
  where
    nId2vertices = IM.fromList $ zip (IM.keys mp) [0 ..]
    vertices2nId = IM.fromList $ zip [0 ..] (IM.keys mp)
    exNodeEdges = expressionEdges expr
    verticesEdges =
        mapMaybe
            (bringMaybeOut . mapBoth (`IM.lookup` nId2vertices))
            exNodeEdges
    graph = buildG (0, IM.size mp - 1) verticesEdges

-- | Get all the edges of the expressions
--
expressionEdges :: (ExpressionMap, Int) -> [(Int, Int)]
expressionEdges (mp, n) = Set.toList $ edges n
  where
    edges :: Int -> Set (Int, Int)
    edges nId =
        let args = nodeArgs $ retrieveNode nId mp
            thisNode = Set.fromList . map (nId, ) $ args
         in Set.unions $ thisNode : map edges args
