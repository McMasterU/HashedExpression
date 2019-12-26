-------------------------------------------------------------------------------
-- | Inner Hashed functions, there are functions as
-- how to combine expression with options
--
--
-------------------------------------------------------------------------------
module HashedInner where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Reader (Reader, ask, runReader)
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
import Prelude hiding
    ( (*)
    , (+)
    , (-)
    , (/)
    , (^)
    , acos
    , acosh
    , asin
    , asinh
    , atan
    , atanh
    , const
    , const
    , cos
    , cosh
    , exp
    , log
    , negate
    , product
    , sin
    , sinh
    , sqrt
    , sum
    , tan
    , tanh
    )

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

const_ :: Shape -> Double -> Change
const_ shape val mp = ExpressionDiff mp n
  where
    (mp, n) = aConst shape val

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

-- | Change w.r.t original expression map
--
type Change = ExpressionMap -> ExpressionDiff

-- | Modification type, given an expression, it will give a difference (i.e, extraEntries in the ExpressionMap, and
-- the new index of the root expression) between the modified and original expression
--
type Modification = (ExpressionMap, Int) -> Change

-- |
--
fromModification :: Modification -> ((ExpressionMap, Int) -> ExpressionDiff)
fromModification mkDiff exp@(mp, n) = mkDiff exp mp

-- |
--
withContext :: ExpressionMap -> Change -> ExpressionDiff
withContext = flip ($)

-- |
--
just :: Int -> Change
just nId _ = ExpressionDiff IM.empty nId

-- | 
--
instance AddableOp Change where
    (+) change1 change2 mp = sumManyDiff mp [change1 mp, change2 mp]

sum_ :: [Change] -> Change
sum_ changes mp = sumManyDiff mp . map ($ mp) $ changes

instance NegateOp Change where
    negate change mp = applyDiff mp (unaryET Neg ElementDefault) [change mp]

instance MultiplyOp Change where
    (*) change1 change2 mp = mulManyDiff mp [change1 mp, change2 mp]

product_ :: [Change] -> Change
product_ changes mp = mulManyDiff mp . map ($ mp) $ changes

instance PowerOp Change Int where
    (^) change alpha mp = applyDiff mp (unary (Power alpha)) [change mp]

instance VectorSpaceOp Change Change where
    scale change1 change2 mp =
        applyDiff mp (binaryET Scale ElementDefault) [change1 mp, change2 mp]

instance ComplexRealOp Change Change where
    (+:) change1 change2 mp =
        applyDiff mp (binary RealImag) [change1 mp, change2 mp]
    xRe change1 mp = applyDiff mp (unary RealPart) [change1 mp]
    xIm change1 mp = applyDiff mp (unary ImagPart) [change1 mp]

instance (DimensionType d) => NumOp Change where
    sqrt change mp = applyDiff mp (unary Sqrt) [change mp]
    exp change mp = applyDiff mp (unary Exp) [change mp]
    log change mp = applyDiff mp (unary Log) [change mp]
    sin change mp = applyDiff mp (unary Sin) [change mp]
    cos change mp = applyDiff mp (unary Cos) [change mp]
    tan change mp = applyDiff mp (unary Tan) [change mp]
    asin change mp = applyDiff mp (unary Asin) [change mp]
    acos change mp = applyDiff mp (unary Acos) [change mp]
    atan change mp = applyDiff mp (unary Atan) [change mp]
    sinh change mp = applyDiff mp (unary Sinh) [change mp]
    cosh change mp = applyDiff mp (unary Cosh) [change mp]
    tanh change mp = applyDiff mp (unary Tanh) [change mp]
    asinh change mp = applyDiff mp (unary Asinh) [change mp]
    acosh change mp = applyDiff mp (unary Acosh) [change mp]
    atanh change mp = applyDiff mp (unary Atanh) [change mp]
    (/) change1 change2 = change1 * (change2 ^ (-1))

instance InnerProductSpaceOp Change Change Change where
    (<.>) change1 change2 mp =
        applyDiff
            mp
            (binaryET InnerProd ElementDefault `hasShape` [])
            [change1 mp, change2 mp]

instance RotateOp RotateAmount Change where
    rotate ra change mp = applyDiff mp (unary (Rotate ra)) [change mp]

instance PiecewiseOp Change Change where
    piecewise marks condition branches mp =
        applyDiff mp (conditionAry (Piecewise marks)) . map ($ mp) $
        condition : branches

-- |
--
toTransformation :: ((ExpressionMap, Int) -> ExpressionDiff) -> Transformation
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

-- | Turn a a recursive one, i.e, apply rules to every node in the expression bottom up
--
toRecursive ::
       OperandOrder
    -> ((ExpressionMap, Int) -> ExpressionDiff)
    -> ((ExpressionMap, Int) -> ExpressionDiff)
toRecursive operandOrder smp exp@(mp, headN) =
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
            TwiceReFT _ -> combine (unary TwiceReFT)
            TwiceImFT _ -> combine (unary TwiceImFT)
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
        | otherwise = Nothing

varNodesWithId :: ExpressionMap -> [(String, Int)]
varNodesWithId mp = mapMaybe collect . IM.keys $ mp
  where
    collect nId
        | Var varName <- retrieveNode nId mp = Just (varName, nId)
        | otherwise = Nothing

-- | If expression map contains any FT node
--
containsFTNode :: ExpressionMap -> Bool
containsFTNode mp = any isFT $ IM.elems mp
  where
    isFT (_, node) =
        case node of
            ReFT _ -> True
            ImFT _ -> True
            _ -> False

-- | A function might be treated as function of variables that not appears in the function itself (like constraint of
-- optimization problem, so we want to pad zero partial derivative)
-- (( ... ) dx + ( .... ) dy) [z, t] ---> (( ... ) dx + ( ... ) dy + 0dz + 0dt)
--
introduceZeroPartialDerivatives ::
       [(String, Shape)]
    -> Expression Scalar Covector
    -> Expression Scalar Covector
introduceZeroPartialDerivatives varsAndShape (Expression n mp) =
    let isD name nId
            | DVar varName <- retrieveNode nId mp
            , varName == name = True
            | otherwise = False
        alreadyExist name = any (isD name) . IM.keys $ mp
        makePart (name, shape)
            | isScalarShape shape =
                mulMany [aConst shape 0, dVarWithShape shape name]
            | otherwise =
                apply
                    (binaryET InnerProd ElementDefault `hasShape` [])
                    [aConst shape 0, dVarWithShape shape name]
        listToInsert =
            map makePart . filter ((not . alreadyExist) . fst) $ varsAndShape
     in wrap $
        case retrieveNode n mp of
            Sum Covector ns -> sumMany $ map (mp, ) ns ++ listToInsert
            _ -> sumMany $ (mp, n) : listToInsert
