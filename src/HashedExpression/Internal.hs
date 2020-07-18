{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      :  HashedExpression.Internal
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- Inner HashedExpression functionality, contains transformations and combinators for manually manipulating HashedExpressions.
module HashedExpression.Internal where

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
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Hash
import HashedExpression.Internal.Node
import HashedExpression.Internal.OperationSpec
import HashedExpression.Internal.Utils
import Prelude hiding ((^))

-- | Unwrap 'Expression' to a 'ExpressionMap' and root 'NodeID'
unwrap :: Expression d et -> (ExpressionMap, NodeID)
unwrap (Expression n mp) = (mp, n)

-- | Wrap a 'ExpressionMap' and root 'NodeID' into an "Expression" type
wrap :: (ExpressionMap, NodeID) -> Expression d et
wrap = uncurry $ flip Expression

-------------------------------------------------------------------------------
addEntryWithContextTo ::
  HasCallStack =>
  ExpressionMap ->
  OperationSpec ->
  [NodeID] ->
  ExpressionMap ->
  (ExpressionMap, NodeID)
addEntryWithContextTo contextMp spec args mp =
  let shapeOf nID = retrieveShape nID contextMp
      etOf nID = retrieveElementType nID contextMp
      (shape, et, op) = case (spec, args) of
        (Unary (UnarySpec toOp decideShape decideET), [arg]) ->
          ( decideShape (shapeOf arg),
            decideET (etOf arg),
            toOp arg
          )
        (Binary (BinarySpec toOp decideShape decideET), [arg1, arg2]) ->
          ( decideShape (shapeOf arg1) (shapeOf arg2),
            decideET (etOf arg1) (etOf arg2),
            toOp arg1 arg2
          )
        (Nary (NarySpec toOp decideShape decideET), args) ->
          ( decideShape (map shapeOf args),
            decideET (map etOf args),
            toOp args
          )
        (ConditionAry (ConditionarySpec toOp decideShape decideET), condition : branches) ->
          ( decideShape (shapeOf condition) (map shapeOf branches),
            decideET (etOf condition) (map etOf branches),
            toOp condition branches
          )
        _ -> error "Unfaithful with operation spec"
   in addNode mp (shape, et, op)

-------------------------------------------------------------------------------

-- | Generic N-Ary multiplication operator, constructed using 'apply'
--   with 'ElementDefault' to default to the 'ElementType' of it's arguments
mulMany :: [(ExpressionMap, NodeID)] -> (ExpressionMap, NodeID)
mulMany = apply (Nary specMul)

-- | Generic N-Ary addition operator, constructed using 'apply'
--   with 'ElementDefault' to default to the 'ElementType' of it's arguments
sumMany :: [(ExpressionMap, NodeID)] -> (ExpressionMap, NodeID)
sumMany = apply (Nary specSum)

-------------------------------------------------------------------------------

-- | Helper function that generalizes the construction of 'Expression' combinators/operators by merging
--   a list of 'ExpressionMap' (operands) using context about the resulting 'Dimension' and 'ElementType'
--   provided via 'OperationOption'.
apply ::
  -- | describes changes in 'Dimension' or 'ElementType'
  OperationSpec ->
  -- | the operands (unwrapped 'Expression')
  [(ExpressionMap, NodeID)] ->
  -- | the resulting (unwrapped) 'Expression'
  (ExpressionMap, NodeID)
apply option exprs =
  addEntryWithContextTo mergedMap option (map snd exprs) mergedMap
  where
    mergedMap = IM.unions . map fst $ exprs

applyUnary ::
  HasCallStack =>
  UnarySpec ->
  -- | the operand
  Expression d1 et1 ->
  -- | the resulting 'Expression'
  Expression d2 et2
applyUnary spec e1 = wrap . apply (Unary spec) $ [unwrap e1]

applyNary ::
  HasCallStack =>
  -- | describes changes in 'Dimension' or 'ElementType'
  NarySpec ->
  -- | the operands
  [Expression d1 et1] ->
  -- | the resulting 'Expression'
  Expression d2 et2
applyNary spec = wrap . apply (Nary spec) . map unwrap

-- | Helper function that generalizes the construction of 'Expression' combinators/operators by merging
--   a two 'Expression' operands using context about the resulting 'Dimension' and 'ElementType'
--   provided via 'OperationOption'. Functionally the same as the 'apply' with automatic wrapping / unwrapping
--   of 'Expression' and a fixed (binary) arity
applyBinary ::
  HasCallStack =>
  -- | describes changes in 'Dimension' or 'ElementType'
  BinarySpec ->
  -- | the "left" operand
  Expression d1 et1 ->
  -- | the "right" operand
  Expression d2 et2 ->
  -- | the resulting 'Expression'
  Expression d3 et3
applyBinary spec e1 e2 = wrap . apply (Binary spec) $ [unwrap e1, unwrap e2]

applyConditionAry ::
  -- | describes changes in 'Dimension' or 'ElementType'
  ConditionarySpec ->
  -- | the conditional/selector operand
  Expression d et1 ->
  -- | operands (branches) that could be selected
  [Expression d et2] ->
  -- | the resulting 'Expression'
  Expression d et2
applyConditionAry spec e branches =
  wrap . apply (ConditionAry spec) $ unwrap e : map unwrap branches

-------------------------------------------------------------------------------

-- | Placeholder for any dimension type, useful for performing symbolic computation on an 'Expression'
--   that requires a fixed type in the dimension type parameter but the actual dimension is irrelavent
--   (such as exterior derivatives)
data D_
  deriving (Typeable, Dimension)

-- | 'D_' is a placeholder type with no real Shape (i.e dimension/size). The method
--   'toShape' should never actually be evaluated on this instance
instance ToShape D_ where
  toShape =
    error "D_ is a place holder, init variable for D_ is not applicable"

-- | Placeholder for any element type, useful for performing symbolic computation on an 'Expression'
--   that requires a fixed type in the element type parameter but the actual element type is irrelavent
data ET_
  deriving (Typeable, ElementType)

-------------------------------------------------------------------------------

-- --------------------------------------------------------------------------------------------------------------------

-- * Expression Transformations

-- --------------------------------------------------------------------------------------------------------------------

-- | Transformation type, take a (unwrapped) 'Expression' and return a transformed (unwrapped) 'Expression'.
--   Construct using the 'toTransformation' function
type Transformation = (ExpressionMap, NodeID) -> (ExpressionMap, NodeID)

-- | Take a function from a (unwrapped) 'Expression' to an 'ExpressionDiff' and create
--   a transformation. See 'fromModification' for turning a 'Modification' into a 'Transformation'
toTransformation ::
  -- | argument provided by 'fromModification'
  ((ExpressionMap, NodeID) -> ExpressionDiff) ->
  -- | resulting transformation
  Transformation
toTransformation normalizer exp@(mp, n) =
  let diff = normalizer exp
      newMp = IM.union mp (extraEntries diff)
      newN = newRootId diff
   in (newMp, newN)

-- | Operand order in the operation
data OperandOrder
  = Reorder
  | NoReorder
  deriving (Eq)

-- | Used to apply rules (which can be generated with 'fromModification') to every 'Node' in an 'Expression' bottom up
toRecursive ::
  -- | Reorder operands or not
  OperandOrder ->
  -- | rule applied to a single 'Node'
  ((ExpressionMap, NodeID) -> ExpressionDiff) ->
  ((ExpressionMap, NodeID) -> ExpressionDiff) -- resulting rule applied to every 'Node'
toRecursive operandOrder smp exp@(mp, headN) = fromJust $ IM.lookup headN diffs
  where
    topoOrder = topologicalSort exp
    f :: IM.IntMap ExpressionDiff -> NodeID -> IM.IntMap ExpressionDiff
    f diffs nId =
      let children = opArgs $ retrieveOp nId mp
          childrenDiffs = map (fromJust . flip IM.lookup diffs) children
          nodeDiff = combineChildrenDiffs operandOrder mp nId childrenDiffs
          -- TODO merge: (mp, nId) (nodeDiff extraEntries, newRootId)
          newExp = (IM.union mp $ extraEntries nodeDiff, newRootId nodeDiff)
          ExpressionDiff exEntries newId = smp newExp
          -- TODO merge: just exEntries ?
          diff = ExpressionDiff (IM.union exEntries (extraEntries nodeDiff)) newId
       in IM.insert nId diff diffs
    diffs = foldl' f IM.empty topoOrder

-- | Remove unreachable nodes
removeUnreachable :: Transformation
removeUnreachable (mp, n) =
  let reachableNodes = IS.fromList . topologicalSort $ (mp, n)
      reducedMap =
        IM.filterWithKey (\nId _ -> IS.member nId reachableNodes) mp -- Only keep those in reachable nodes
   in (reducedMap, n)

-- | Apply a 'Transformation' maximum k times, or stop if the expression doesn't change
multipleTimes ::
  -- | k number of max iterations
  Int ->
  -- | original 'Transformation'
  Transformation ->
  -- | resulting combined 'Transformation'
  Transformation
multipleTimes outK smp exp = go (outK - 1) exp (smp exp)
  where
    go 0 _ curExp = curExp
    go k lastExp curExp
      | snd lastExp == snd curExp = curExp
      | otherwise = go (k - 1) curExp (smp curExp)

-- | Multiply a list of 'ExpressionDiff' together into a single 'ExpressionDiff'
product_ :: HasCallStack => [ExpressionDiff] -> ExpressionDiff
product_ = applyDiff1 (Nary specMul)

-- | Sum a list of 'ExpressionDiff' together into a single 'ExpressionDiff'
sum_ :: HasCallStack => [ExpressionDiff] -> ExpressionDiff
sum_ = applyDiff1 (Nary specSum)

-- --------------------------------------------------------------------------------------------------------------------

-- * Expression Diffs

-- --------------------------------------------------------------------------------------------------------------------

-- | When performing a 'Change' on an 'ExpressionMap', the extra entries created by the change
--   (i.e not included in the original 'ExpressionMap') and a new root 'NodeID' are stored in the ExpressionDiff type
data ExpressionDiff = ExpressionDiff
  { -- | Extra entries we need to add to the original Expression Map
    extraEntries :: ExpressionMap,
    -- | New root of the expression (can change, can be the same)
    newRootId :: NodeID
  }
  deriving (Eq, Ord, Show)

--
---- | Compute a new 'ExpressionDiff' (with respect to a base 'ExpressionMap') when applying an operation to
----   other 'ExpressionDiff'
--applyDiff ::
--  -- | the base map to find diffs w.r.t
--  ExpressionMap ->
--  -- | the operation to apply
--  OperationSpec ->
--  -- | the operands (also in diff to the base map)
--  [ExpressionDiff] ->
--  -- | the result (combined diffs with new nodes)
--  ExpressionDiff
--applyDiff contextMp option operands = ExpressionDiff resExtraEntries resRootId
--  where
--    mergedExtraEntries = IM.unions . map extraEntries $ operands
--    updatedContextMp = IM.union mergedExtraEntries contextMp
--    ns = map newRootId operands
--    (resExtraEntries, resRootId) = addEntryWithContextTo updatedContextMp option ns mergedExtraEntries

applyDiff1 ::
  HasCallStack =>
  -- | the operation to apply
  OperationSpec ->
  -- | the operands (also in diff to the base map)
  [ExpressionDiff] ->
  -- | the result (combined diffs with new nodes)
  ExpressionDiff
applyDiff1 option operands = ExpressionDiff resExtraEntries resRootId
  where
    mergedExtraEntries = IM.unions . map extraEntries $ operands
    ns = map newRootId operands
    (resExtraEntries, resRootId) = addEntryWithContextTo mergedExtraEntries option ns mergedExtraEntries

instance Num ExpressionDiff where
  (+) change1 change2 = applyDiff1 (Nary specSum) [change1, change2]
  negate change = applyDiff1 (Unary specNeg) [change]
  (*) change1 change2 = applyDiff1 (Nary specMul) [change1, change2]

instance Fractional ExpressionDiff where
  (/) change1 change2 = change1 * (change2 ^ (-1))
  fromRational r = error "N/A"

instance Floating ExpressionDiff where
  sqrt change = applyDiff1 (Unary specSqrt) [change]
  exp change = applyDiff1 (Unary specExp) [change]
  log change = applyDiff1 (Unary specLog) [change]
  sin change = applyDiff1 (Unary specSin) [change]
  cos change = applyDiff1 (Unary specCos) [change]
  tan change = applyDiff1 (Unary specTan) [change]
  asin change = applyDiff1 (Unary specAsin) [change]
  acos change = applyDiff1 (Unary specAcos) [change]
  atan change = applyDiff1 (Unary specAtan) [change]
  sinh change = applyDiff1 (Unary specSinh) [change]
  cosh change = applyDiff1 (Unary specCosh) [change]
  tanh change = applyDiff1 (Unary specTanh) [change]
  asinh change = applyDiff1 (Unary specAsinh) [change]
  acosh change = applyDiff1 (Unary specAcosh) [change]
  atanh change = applyDiff1 (Unary specAtanh) [change]

instance PowerOp ExpressionDiff Int where
  (^) change alpha = applyDiff1 (Unary (specPower alpha)) [change]

instance VectorSpaceOp ExpressionDiff ExpressionDiff where
  scale change1 change2 =
    applyDiff1 (Binary specScale) [change1, change2]

instance ComplexRealOp ExpressionDiff ExpressionDiff where
  (+:) change1 change2 = applyDiff1 (Binary specRealImag) [change1, change2]
  xRe change1 = applyDiff1 (Unary specRealPart) [change1]
  xIm change1 = applyDiff1 (Unary specImagPart) [change1]

instance InnerProductSpaceOp ExpressionDiff ExpressionDiff ExpressionDiff where
  (<.>) change1 change2 =
    applyDiff1 (Binary specInnerProd) [change1, change2]

instance RotateOp RotateAmount ExpressionDiff where
  rotate ra change = applyDiff1 (Unary (specRotate ra)) [change]

instance PiecewiseOp ExpressionDiff ExpressionDiff where
  piecewise marks condition branches =
    applyDiff1 (ConditionAry (specPiecewise marks)) $ condition : branches

instance MulCovectorOp ExpressionDiff ExpressionDiff ExpressionDiff where
  (|*|) change1 change2 = applyDiff1 (Binary specMulD) [change1, change2]

instance ScaleCovectorOp ExpressionDiff ExpressionDiff ExpressionDiff where
  (|*.|) change1 change2 = applyDiff1 (Binary specScaleD) [change1, change2]

instance CovectorScaleOp ExpressionDiff ExpressionDiff ExpressionDiff where
  (|.*|) change1 change2 = applyDiff1 (Binary specDScale) [change1, change2]

instance InnerProductCovectorOp ExpressionDiff ExpressionDiff ExpressionDiff where
  (|<.>|) change1 change2 = applyDiff1 (Binary specInnerProdD) [change1, change2]

-- | The 'ExpressionDiff' when adding a constant is just the constant node
const_ :: Shape -> Double -> ExpressionDiff
const_ shape val = ExpressionDiff mp n
  where
    (mp, n) = aConst shape val
    
num_ :: Double -> ExpressionDiff
num_ = const_ []

dZeroWithShape :: Shape -> ExpressionDiff
dZeroWithShape shape = ExpressionDiff mp n
  where
    Expression n mp = fromNode (shape, Covector, DZero)


just :: ExpressionMap -> NodeID -> ExpressionDiff
just mp nID =
  ExpressionDiff (IM.singleton nID (retrieveNode nID mp)) nID

-- | Same node type (Mul, Sum, Negate, ...), same shape, same ElementType but with new children, now make the same node type with new children
--   and return the combined difference
combineChildrenDiffs ::
  OperandOrder ->
  ExpressionMap ->
  NodeID ->
  [ExpressionDiff] ->
  ExpressionDiff
combineChildrenDiffs operandOrder contextMp n childrenDiffs
  | Sum _ <- oldOp,
    operandOrder == Reorder =
    sortAndCombine (Nary specSum)
  | Mul _ <- oldOp,
    operandOrder == Reorder =
    sortAndCombine (Nary specMul)
  | InnerProd _ _ <- oldOp,
    R == oldET,
    operandOrder == Reorder =
    sortAndCombine (Binary specInnerProd)
  | oldChildrenIDs == newChildrenIDs  = just contextMp n
  | otherwise =
    case oldOp of
      Var _ -> just contextMp n
      DVar _ -> just contextMp n
      Const _ -> just contextMp n
      Sum _ -> combine (Nary specSum)
      Mul _ -> combine (Nary specMul)
      Power x _ -> combine (Unary (specPower x))
      Neg _ -> combine (Unary specNeg)
      Scale _ _ -> combine (Binary specScale)
      Div _ _ -> combine (Binary specDiv)
      Sqrt _ -> combine (Unary specSqrt)
      Sin _ -> combine (Unary specSin)
      Cos _ -> combine (Unary specCos)
      Tan _ -> combine (Unary specTan)
      Exp _ -> combine (Unary specExp)
      Log _ -> combine (Unary specLog)
      Sinh _ -> combine (Unary specSinh)
      Cosh _ -> combine (Unary specCosh)
      Tanh _ -> combine (Unary specTanh)
      Asin _ -> combine (Unary specAsin)
      Acos _ -> combine (Unary specAcos)
      Atan _ -> combine (Unary specAtan)
      Asinh _ -> combine (Unary specAsinh)
      Acosh _ -> combine (Unary specAcosh)
      Atanh _ -> combine (Unary specAtanh)
      RealPart _ -> combine (Unary specRealPart)
      ImagPart _ -> combine (Unary specImagPart)
      RealImag _ _ -> combine (Binary specRealImag)
      InnerProd _ _ -> combine (Binary specInnerProd)
      Piecewise marks _ _ -> combine (ConditionAry (specPiecewise marks))
      Rotate amount _ -> combine (Unary (specRotate amount))
      ReFT _ -> combine (Unary specReFT)
      ImFT _ -> combine (Unary specImFT)
      TwiceReFT _ -> combine (Unary specTwiceReFT)
      TwiceImFT _ -> combine (Unary specTwiceImFT)
      DZero -> just contextMp n
      MulD _ _ -> combine (Binary specMulD)
      ScaleD {} -> combine (Binary specScaleD)
      DScale {} -> combine (Binary specDScale)
      InnerProdD _ _ -> combine (Binary specInnerProdD)
  where
    (oldShape, oldET, oldOp) = retrieveNode n contextMp
    -------------------------------------------------------------------------------
    oldChildrenIDs = opArgs oldOp
    newChildrenIDs = map newRootId childrenDiffs
    -------------------------------------------------------------------------------
    -- TODO merge: safe merge
    combinedExtraEntries = IM.unions . map extraEntries $ childrenDiffs
    -------------------------------------------------------------------------------
    combine spec = applyDiff1 spec childrenDiffs
    sortAndCombine option =
      let getOp diff
            | Just (_, _, node) <- IM.lookup (newRootId diff) contextMp = node
            | Just (_, _, node) <- IM.lookup (newRootId diff) combinedExtraEntries = node
          opType diff1 diff2 = sameOp (getOp diff1) (getOp diff2)
          weight diff = nodeTypeWeight $ getOp diff
          sortArgs = concatMap (sortWith newRootId) . groupBy opType . sortWith weight
          sortedChildrenDiffs = sortArgs childrenDiffs
       in if oldChildrenIDs == map newRootId sortedChildrenDiffs
            && all ((== IM.empty) . extraEntries) sortedChildrenDiffs
            then just contextMp n
            else applyDiff1 option sortedChildrenDiffs

-- --------------------------------------------------------------------------------------------------------------------

-- * Other

-- --------------------------------------------------------------------------------------------------------------------

-- | Topological sort the expression map, all the dependencies will appear before the depended node, and all
--   unreachable nodes will be ignored
topologicalSort ::
  -- | unwrapped 'Expression'
  (ExpressionMap, NodeID) ->
  -- | list in topological order (independent to dependent)
  [NodeID]
topologicalSort (mp, n) = topologicalSortManyRoots (mp, [n])

-- | Topological sort the expression map (with multiple roots), all the dependencies will appear before the depended node, and all
--   unreachable nodes will be ignored
topologicalSortManyRoots ::
  -- | many rooted unwrapped 'Expression'
  (ExpressionMap, [NodeID]) ->
  -- | list in topological order (independent to dependent)
  [NodeID]
topologicalSortManyRoots (mp, ns) = filter (/= -1) . UA.elems $ topoOrder
  where
    n2Pos = IM.fromList $ zip (IM.keys mp) [0 ..]
    toPos nId = fromJust $ IM.lookup nId n2Pos
    len = IM.size n2Pos
    adj nId = opArgs $ retrieveOp nId mp
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

-- | Retrieves all 'Var' nodes in an 'Expression'
expressionVarNodes :: (DimensionType d, ElementType et) => Expression d et -> [(String, NodeID)]
expressionVarNodes (Expression n mp) = mapMaybe collect ns
  where
    ns = topologicalSort (mp, n)
    collect nId
      | Var varName <- retrieveOp nId mp = Just (varName, nId)
      | otherwise = Nothing

-- | Retrieves all 'Var' nodes in an (unwrapped) 'Expression'
varNodesWithId :: ExpressionMap -> [(String, NodeID)]
varNodesWithId mp = mapMaybe collect . IM.keys $ mp
  where
    collect nId
      | Var varName <- retrieveOp nId mp = Just (varName, nId)
      | otherwise = Nothing

-- | Predicate determining if a 'ExpressionMap' contains a FT operation
containsFTNode :: ExpressionMap -> Bool
containsFTNode mp = any isFT $ IM.elems mp
  where
    isFT (_, _, op) =
      case op of
        ReFT _ -> True
        ImFT _ -> True
        TwiceImFT _ -> True
        TwiceReFT _ -> True
        _ -> False
