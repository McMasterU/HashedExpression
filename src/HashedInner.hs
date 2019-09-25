-------------------------------------------------------------------------------
-- | Inner Hashed functions, there are functions as
-- how to combine expression with options
--
--
-------------------------------------------------------------------------------
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}

module HashedInner where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.ST.Strict
import Data.Array.MArray
import Data.Array.ST
import qualified Data.Array.Unboxed as UA
import Data.Data (Typeable)
import Data.Graph (buildG, topSort)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', groupBy, sort, sortBy, sortOn)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.STRef.Strict
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as Set
import Debug.Trace (traceShowId)
import GHC.Exts (sortWith)
import GHC.Stack (HasCallStack)
import HashedExpression
import HashedHash
import HashedNode
import HashedUtils
import Prelude hiding ((+), (-))

-- | Enable this when we want to check for conflict when merging two expressions
-- Different node of two maps could have the same hash (which we hope never happens)
--
checkMergeConflict :: Bool
checkMergeConflict = False

-- | TODO: Check if 2 different nodes from 2 maps have the same hash
--
safeUnion :: ExpressionMap -> ExpressionMap -> ExpressionMap
safeUnion = IM.union

-- | Placeholder for any dimension type
--
data D_
    deriving (Typeable, DimensionType)

-- | Placeholder for any element type
--
data ET_
    deriving (Typeable, ElementType)

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
     in addInternal mp (shape, node)
--                _ -> error "HashedInner.applySameScope"
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
       HasCallStack
    => OperationOption
    -> Expression d1 et1
    -> Expression d2 et2
    -> Expression d3 et3
applyBinary option e1 e2 = wrap . apply option $ [unwrap e1, unwrap e2]

applyUnary ::
       HasCallStack => OperationOption -> Expression d1 et1 -> Expression d2 et2
applyUnary option e1 = wrap . apply option $ [unwrap e1]

applyNary ::
       HasCallStack
    => OperationOption
    -> [Expression d1 et1]
    -> Expression d2 et2
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

-- |
--
diffConst :: Shape -> Double -> ExpressionDiff
diffConst shape val = ExpressionDiff mp n
  where
    (mp, n) = aConst shape val

-- | Topological sort the expression map, all the dependencies will appear before the depended node, and all
-- unreachable nodes will be ignored
--
topologicalSort :: (ExpressionMap, Int) -> [Int]
topologicalSort (mp, n) = topologicalSortManyRoots (mp, [n])

-- | Topological sort, but with many roots
--
topologicalSortManyRoots :: (ExpressionMap, [Int]) -> [Int]
topologicalSortManyRoots (mp, ns) = filter (/= -1) . UA.elems $ topoOrder
  where
    n2Pos = IM.fromList $ zip (IM.keys mp) [0 ..]
    toPos nId = fromJust $ IM.lookup nId n2Pos
    len = IM.size n2Pos
    adj nId = nodeArgs $ retrieveNode nId mp
    topoOrder =
        runSTUArray $ do
            marked <- newArray (0, len - 1) False :: ST s (STUArray s Int Bool)
            order <- newArray (0, len - 1) (-1) :: ST s (STUArray s Int Int)
            cnt <- newSTRef 0 :: ST s (STRef s Int)
            let dfs u = do
                    let arrayPos = toPos u
                    writeArray marked arrayPos True
                    forM_ (adj u) $ \v -> do
                        isMarked <- readArray marked (toPos v)
                        unless isMarked $ dfs v
                    cntVal <- readSTRef cnt
                    writeArray order cntVal u
                    writeSTRef cnt (cntVal + 1)
            forM_ ns $ \n -> do
                isMarked <- readArray marked (toPos n)
                unless isMarked $ dfs n
            return order

-- | Modification will return an ExpressionDiff instead of the whole Expression to speed things up
-- What ExpressionDiff stands for?
data ExpressionDiff =
    ExpressionDiff
        { extraEntries :: ExpressionMap -- Extra entries we need to add to the original Expression Map
        , newRootId :: Int -- New root of the expression (can change, can be the same)
        }
    deriving (Eq, Ord, Show)

-- | Transformation type, we can combine them, chain them, apply them n times using nest, ...
--
type Transformation = (ExpressionMap, Int) -> (ExpressionMap, Int)

-- | Modification type, given an expression, it will give a difference (i.e, extraEntries in the ExpressionMap, and
-- the new index of the root expression) between the modified and original expression
--
type Modification = (ExpressionMap, Int) -> ExpressionDiff

-- |
--
toTransformation :: Modification -> Transformation
toTransformation normalizer exp@(mp, n) =
    let diff = normalizer exp
        newMp = IM.union mp (extraEntries diff)
        newN = newRootId diff
     in (newMp, newN)

-- | Apply maximum k times, or stop if the expression doesn't change
--
multipleTimes :: Int -> Transformation -> Transformation
multipleTimes outK smp exp = go (outK - 1) exp (smp exp)
  where
    go 0 _ curExp = curExp
    go k lastExp curExp
        | snd lastExp == snd curExp = curExp
        | otherwise = go (k - 1) curExp (smp curExp)

-- | Operand order in the operation
--
data OperandOrder
    = Reorder
    | NoReorder
    deriving (Eq)

-- | Turn a Modification to a recursive one, i.e, apply rules to every node in the expression bottom up
--
toRecursiveModification :: OperandOrder -> Modification -> Modification
toRecursiveModification operandOrder smp exp@(mp, headN) =
    fromJust $ IM.lookup headN diffs
  where
    topoOrder = topologicalSort exp
    f :: IM.IntMap ExpressionDiff -> Int -> IM.IntMap ExpressionDiff
    f diffs nId =
        let children = nodeArgs $ retrieveNode nId mp
            childrenDiffs = map (fromJust . flip IM.lookup diffs) children
            nodeDiff = combineChildrenDiffs operandOrder mp nId childrenDiffs
            newExp = (IM.union mp $ extraEntries nodeDiff, newRootId nodeDiff)
            ExpressionDiff exEntries newId = smp newExp
            diff =
                ExpressionDiff
                    (IM.union exEntries (extraEntries nodeDiff))
                    newId
         in IM.insert nId diff diffs
    diffs = foldl' f IM.empty topoOrder

-- | Same node type (Mul, Sum, Negate, ...), but children may changed, now make the same node type with new children
-- and return the combined difference
--
combineChildrenDiffs ::
       OperandOrder
    -> ExpressionMap
    -> Int
    -> [ExpressionDiff]
    -> ExpressionDiff
combineChildrenDiffs operandOrder contextMp n childrenDiffs
    | Sum et _ <- oldNode
    , operandOrder == Reorder = sortAndCombine (naryET Sum (ElementSpecific et))
    | Mul et _ <- oldNode
    , operandOrder == Reorder = sortAndCombine (naryET Mul (ElementSpecific et))
    | InnerProd R arg1 arg2 <- oldNode
    , operandOrder == Reorder =
        sortAndCombine (binaryET InnerProd (ElementSpecific R))
    | oldChildren == newChildren &&
          all (== IM.empty) (map extraEntries childrenDiffs) = noChange n
    | otherwise =
        case oldNode of
            Var _ -> noChange n
            DVar _ -> noChange n
            Const _ -> noChange n
            Sum et _ -> combine (naryET Sum (ElementSpecific et))
            Mul et _ -> combine (naryET Mul (ElementSpecific et))
            Power x _ -> combine (unary (Power x))
            Neg et _ -> combine (unaryET Neg (ElementSpecific et))
            Scale et _ _ -> combine (binaryET Scale (ElementSpecific et))
            Div _ _ -> combine (binary Div)
            Sqrt _ -> combine (unary Sqrt)
            Sin _ -> combine (unary Sin)
            Cos _ -> combine (unary Cos)
            Tan _ -> combine (unary Tan)
            Exp _ -> combine (unary Exp)
            Log _ -> combine (unary Log)
            Sinh _ -> combine (unary Sinh)
            Cosh _ -> combine (unary Cosh)
            Tanh _ -> combine (unary Tanh)
            Asin _ -> combine (unary Asin)
            Acos _ -> combine (unary Acos)
            Atan _ -> combine (unary Atan)
            Asinh _ -> combine (unary Asinh)
            Acosh _ -> combine (unary Acosh)
            Atanh _ -> combine (unary Atanh)
            RealImag _ _ -> combine (binary RealImag)
            RealPart _ -> combine (unary RealPart)
            ImagPart _ -> combine (unary ImagPart)
            InnerProd et _ _ ->
                combine (binaryET InnerProd (ElementSpecific et))
            Piecewise marks _ _ -> combine (conditionAry (Piecewise marks))
            Rotate amount _ -> combine (unary (Rotate amount))
            ReFT _ -> combine (unary ReFT)
            ImFT _ -> combine (unary ImFT)
  where
    (oldShape, oldNode) = retrieveInternal n contextMp
    oldChildren = nodeArgs oldNode
    newChildren = map newRootId childrenDiffs
    combinedExtraEntries = IM.unions . map extraEntries $ childrenDiffs
    combine option =
        applyDiff contextMp (option `hasShape` oldShape) childrenDiffs
    sortAndCombine option =
        let getNode diff
                | Just (_, node) <- IM.lookup (newRootId diff) contextMp = node
                | Just (_, node) <-
                     IM.lookup (newRootId diff) combinedExtraEntries = node
            nodeType diff1 diff2 = sameNodeType (getNode diff1) (getNode diff2)
            weight diff = nodeTypeWeight $ getNode diff
            sortArgs =
                concatMap (sortWith newRootId) .
                groupBy nodeType . sortWith weight
            sortedChildrenDiffs = sortArgs childrenDiffs
         in if oldChildren == map newRootId sortedChildrenDiffs &&
               all (== IM.empty) (map extraEntries sortedChildrenDiffs)
                then noChange n
                else applyDiff
                         contextMp
                         (option `hasShape` oldShape)
                         sortedChildrenDiffs

-- | Remove unreachable nodes
--
removeUnreachable :: Transformation
removeUnreachable (mp, n) =
    let reachableNodes = IS.fromList . topologicalSort $ (mp, n)
        reducedMap =
            IM.filterWithKey (\nId _ -> IS.member nId reachableNodes) mp -- Only keep those in reachable nodes
     in (reducedMap, n)

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

-- | The ExpressionDiff corresponding to no change in this node
--
noChange :: Int -> ExpressionDiff
noChange = ExpressionDiff IM.empty

-- | All variables in the Expression
--
expressionVarNodes ::
       (DimensionType d, ElementType et) => Expression d et -> [(String, Int)]
expressionVarNodes (Expression n mp) = mapMaybe collect ns
  where
    ns = topologicalSort (mp, n)
    collect nId
        | Var varName <- retrieveNode nId mp = Just (varName, nId)
        | DVar varName <- retrieveNode nId mp = Just (varName, nId)
        | otherwise = Nothing

varsAndShape :: ExpressionMap -> [(String, Int)]
varsAndShape mp = mapMaybe collect . IM.keys $ mp
  where
    collect nId
        | Var varName <- retrieveNode nId mp = Just (varName, nId)
        | otherwise = Nothing
