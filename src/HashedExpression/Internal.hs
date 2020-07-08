{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Module      :  HashedExpression.Internal
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- Inner HashedExpression functionality, contains transformations and combinators for manually manipulating HashedExpressions.
module HashedExpression.Internal
  ( -- * Expression Builders
    apply,
    applyBinary,
    applyNary,
    applyConditionAry,
    unwrap,
    wrap,
    mulMany,
    sumMany,

    -- * Operation Outcomes
    ElementOutcome (..),
    D_,
    ET_,
    OperationOption (..),
    NodeOutcome (..),
    binary,
    binaryET,
    applyUnary,
    unary,
    unaryET,
    naryET,
    conditionAry,
    hasShape,

    -- * Expression Transformations
    Transformation,
    toTransformation,
    Modification,
    fromModification,
    OperandOrder (..),
    toRecursive,
    multipleTimes,
    removeUnreachable,

    -- * Expression Changes
    Change,
    const_,
    num_,
    product_,
    sum_,
    just,

    -- * Expression Diffs
    ExpressionDiff (..),
    applyDiff,
    diffConst,
    mulManyDiff,
    sumManyDiff,
    noChange,
    combineChildrenDiffs,

    -- * Other
    topologicalSort,
    topologicalSortManyRoots,
    expressionVarNodes,
    varNodesWithId,
    containsFTNode,
  )
where

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
import HashedExpression.Internal.Utils
import Prelude hiding ((^))

-- --------------------------------------------------------------------------------------------------------------------

-- * Expression Builders
-- --------------------------------------------------------------------------------------------------------------------

-- | Helper function that generalizes the construction of 'Expression' combinators/operators by merging
--   a list of 'ExpressionMap' (operands) using context about the resulting 'Dimension' and 'ElementType'
--   provided via 'OperationOption'.
apply ::
  -- | describes changes in 'Dimension' or 'ElementType'
  OperationOption ->
  -- | the operands (unwrapped 'Expression')
  [(ExpressionMap, NodeID)] ->
  -- | the resulting (unwrapped) 'Expression'
  (ExpressionMap, NodeID)
apply option exprs =
  addEntryWithContext mergedMap mergedMap option (map snd exprs)
  where
    mergedMap = IM.unions . map fst $ exprs

-- | Helper function that generalizes the construction of 'Expression' combinators/operators by merging
--   a list of 'Expression' (operands) using context about the resulting 'Dimension' and 'ElementType'
--   provided via 'OperationOption'. Functionally the same as the 'apply' with automatic wrapping / unwrapping
--   of 'Expression'
applyNary ::
  HasCallStack =>
  -- | describes changes in 'Dimension' or 'ElementType'
  OperationOption ->
  -- | the operands
  [Expression d1 et1] ->
  -- | the resulting 'Expression'
  Expression d2 et2
applyNary option = wrap . apply option . map unwrap

-- | Helper function that generalizes the construction of 'Expression' combinators/operators by merging
--   a two 'Expression' operands using context about the resulting 'Dimension' and 'ElementType'
--   provided via 'OperationOption'. Functionally the same as the 'apply' with automatic wrapping / unwrapping
--   of 'Expression' and a fixed (binary) arity
applyBinary ::
  HasCallStack =>
  -- | describes changes in 'Dimension' or 'ElementType'
  OperationOption ->
  -- | the "left" operand
  Expression d1 et1 ->
  -- | the "right" operand
  Expression d2 et2 ->
  -- | the resulting 'Expression'
  Expression d3 et3
applyBinary option e1 e2 = wrap . apply option $ [unwrap e1, unwrap e2]

-- | Helper function that generalizes the construction of 'Expression' operators that transform
--   a input 'Expression' using context about the resulting 'Dimension' and 'ElementType'
--   provided via 'OperationOption'. Functionally the same as the 'apply' with automatic wrapping / unwrapping
--   of 'Expression' and a fixed (unary) arity
applyUnary ::
  HasCallStack =>
  -- | describes changes in 'Dimension' or 'ElementType'
  OperationOption ->
  -- | the operand
  Expression d1 et1 ->
  -- | the resulting 'Expression'
  Expression d2 et2
applyUnary option e1 = wrap . apply option $ [unwrap e1]

-- | Helper function that generalizes the construction of 'Expression' functions that
--   are piecewise, requiring a operand 'Expression' to serve as a conditional (or selector)
--   of other operands (branches)
applyConditionAry ::
  -- | describes changes in 'Dimension' or 'ElementType'
  OperationOption ->
  -- | the conditional/selector operand
  Expression d et1 ->
  -- | operands (branches) that could be selected
  [Expression d et2] ->
  -- | the resulting 'Expression'
  Expression d et2
applyConditionAry option e branches =
  wrap . apply option $ unwrap e : map unwrap branches

-- | Unwrap 'Expression' to a 'ExpressionMap' and root 'NodeID'
unwrap :: Expression d et -> (ExpressionMap, NodeID)
unwrap (Expression n mp) = (mp, n)

-- | Wrap a 'ExpressionMap' and root 'NodeID' into an "Expression" type
wrap :: (ExpressionMap, NodeID) -> Expression d et
wrap = uncurry $ flip Expression

-- | Generic N-Ary multiplication operator, constructed using 'apply'
--   with 'ElementDefault' to default to the 'ElementType' of it's arguments
mulMany :: [(ExpressionMap, NodeID)] -> (ExpressionMap, NodeID)
mulMany = apply $ naryET Mul ElementDefault

-- | Generic N-Ary addition operator, constructed using 'apply'
--   with 'ElementDefault' to default to the 'ElementType' of it's arguments
sumMany :: [(ExpressionMap, NodeID)] -> (ExpressionMap, NodeID)
sumMany = apply $ naryET Sum ElementDefault

-- TODO is this needed?
-- Check if 2 different nodes from 2 maps have the same hash
safeUnion :: ExpressionMap -> ExpressionMap -> ExpressionMap
safeUnion = IM.union

-- | Generate a new 'ExpressionMap' with a new root 'Node' and 'NodeID' from a merged 'ExpressionMap' (of arguments)
addEntryWithContext ::
  -- | merged map of all
  ExpressionMap ->
  -- | TODO Haddock: is this not redundant?
  ExpressionMap ->
  -- | describes how an operators may change 'Dimension' or 'ElementType'
  OperationOption ->
  -- | list of root 'NodeID' of arguments
  [NodeID] ->
  -- | the resulting 'Expression' (unwrapped)
  (ExpressionMap, NodeID)
addEntryWithContext contextMp mp (Normal nodeOutcome shapeOutcome) ns =
  let highestShape :: [(ExpressionMap, NodeID)] -> Shape
      highestShape = last . sortOn length . map (uncurry $ flip retrieveShape)
      highestShapeWithContext :: ExpressionMap -> [NodeID] -> Shape
      highestShapeWithContext mp = last . sortOn length . map (`retrieveShape` mp)
      highestElementType :: [(ExpressionMap, NodeID)] -> ET
      highestElementType = maximum . map (uncurry $ flip retrieveElementType)
      highestElementTypeWithContext :: ExpressionMap -> [NodeID] -> ET
      highestElementTypeWithContext mp = maximum . map (`retrieveElementType` mp)
      shape :: Shape
      shape = case shapeOutcome of
        ShapeSpecific s -> s
        _ -> highestShapeWithContext contextMp ns
      elementType :: ElementOutcome -> ET
      elementType elementOutcome = case elementOutcome of
        ElementSpecific et -> et
        _ -> highestElementTypeWithContext contextMp ns
      node :: Node
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
addEntryWithContext contextMp mp (Condition op) ns@(conditionN : branchesNs) =
  let headBranchN = head branchesNs
      shape = retrieveShape headBranchN contextMp
      node = op conditionN branchesNs
   in addInternal mp (shape, node)

-- --------------------------------------------------------------------------------------------------------------------

-- * Operation Outcomes
-- --------------------------------------------------------------------------------------------------------------------

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

-- | A union type to select either a default 'ElementType' or a specific one.
--   Default selection follows a priority chain 'R' < 'C' < 'Covector', choosing the first valid type
data ElementOutcome
  = -- | force a specifc 'ElementType'
    ElementSpecific ET
  | -- | select prioritized valid element ('R' < 'C' < 'Covector')
    ElementDefault

-- | Possible outcomes effecting 'Dimension' and 'ElementType' when applying an operator.
--   Because of piecewise functions, constructing combinators for 'Expression' can be complicated due to
--   operator application resulting in conditionals. See the functions 'binary', 'unary', 'nary' and 'conditionAry'
--   for constructing generic OperationOption's
data OperationOption
  = -- | application has a normal outcome
    Normal NodeOutcome ShapeOutcome
  | -- | application outcome depends on condition (piecewise function)
    Condition (ConditionArg -> [BranchArg] -> Node)

-- | Transform an 'OperationOption' to have a specific 'Shape'.
--   Useful for constructing an 'OperationOption' in conjunction with the 'unary', 'binary' and 'nary' functions that
--   default to 'ShapeDefault'. For example, one could construct an Inner Product operator for scalars like so
--
-- @
--  innerOp = binaryET InnerProd (ElementSpecific R) `hasShape` []
-- @
hasShape :: OperationOption -> Shape -> OperationOption
hasShape (Normal nodeOutcome _) specificShape =
  Normal nodeOutcome (ShapeSpecific specificShape)
hasShape option _ = option

-- | When merging 'ExpressionMap', like in the 'apply' function used to construct operators for 'Expression',
--   different cases for inferring 'DimensionType' and 'ElementType' of the resulting 'Node' need to be considered.
--   The following data type contains constructors covering these cases, the are used in normal control flow by
--   'OperationOption'
--   TODO Haddock: why is there an OpOne and OpOneElement? Explain this?
data NodeOutcome
  = -- | Unary Operator, 'ElementType' irrelavent
    OpOne (Arg -> Node)
  | -- | Unary Operator, 'ElementType' included
    OpOneElement (ET -> Arg -> Node) ElementOutcome
  | -- | Binary Operator, 'ElementType' irrelavent
    OpTwo (Arg -> Arg -> Node)
  | -- | Binary Operator, 'ElementType' included
    OpTwoElement (ET -> Arg -> Arg -> Node) ElementOutcome
  | -- | N-Ary Operator, 'ElementType' irrelavent
    OpMany (Args -> Node)
  | -- | N-Ary Operator, 'ElementType' included
    OpManyElement (ET -> Args -> Node) ElementOutcome

-- | When merging 'ExpressionMap', like in the 'apply' function used to construct operators for 'Expression',
--   different cases for inferring dimensions and sizes (i.e the 'Shape') of the resulting 'Node' need to be considered.
--   The following data type contains constructors covering these cases, the are used in normal control flow by
--   'OperationOption'
data ShapeOutcome
  = -- | provide a specific 'Shape'
    ShapeSpecific Shape
  | -- | automatically select the 'Shape' with the longest length
    ShapeDefault
  | -- | select the shape of a 'BranchArg' (used in piecewise functions)
    ShapeBranches

-- | Construct a 'OperationOption' for a generic binary operator (of implicit 'ElementType').
--   Always returns the 'Normal' constructor with the input 'Node' wrapped in the 'OpTwo' constructor.
--   Use 'hasShape' to select a different 'ShapeOutcome' than 'ShapDefault'
binary ::
  -- | a constructor of 'Node'
  (Arg -> Arg -> Node) ->
  OperationOption -- result
binary op = Normal (OpTwo op) ShapeDefault

-- | Construct a 'OperationOption' for a generic binary operator (of explicit 'ElementType').
--   Always returns the 'Normal' constructor with the input 'Node' wrapped in the 'OpTwoElement' constructor.
--   Use 'hasShape' to select a different 'ShapeOutcome' than 'ShapeDefault'
binaryET ::
  -- | constructor of 'Node'
  (ET -> Arg -> Arg -> Node) ->
  -- | specific 'ElementType' or default
  ElementOutcome ->
  -- | result
  OperationOption
binaryET op elm = Normal (OpTwoElement op elm) ShapeDefault

-- | Construct a 'OperationOption' for a generic unary operator (of implicit 'ElementType').
--   Always returns the 'Normal' constructor with the input 'Node' wrapped in the 'OpOne' constructor.
--   Use 'hasShape' to select a different 'ShapeOutcome' than 'ShapeDefault'
unary ::
  -- | constructor of 'Node'
  (Arg -> Node) ->
  -- | result
  OperationOption
unary op = Normal (OpOne op) ShapeDefault

-- | Construct a 'OperationOption' for a generic unary operator (of explicit 'ElementType').
--   Always returns the 'Normal' constructor with the input 'Node' wrapped in the 'OpOneElement' constructor.
--   Use 'hasShape' to select a different 'ShapeOutcome' than 'ShapeDefault'
unaryET ::
  -- | constructor of 'Node'
  (ET -> Arg -> Node) ->
  -- | specific 'ElementType' or default
  ElementOutcome ->
  -- | result
  OperationOption
unaryET op elm = Normal (OpOneElement op elm) ShapeDefault

-- | Construct a 'OperationOption' for a generic n-Ary operator (of implicit 'ElementType').
--   Always returns the 'Normal' constructor with the input 'Node' wrapped in the 'OpMany' constructor.
--   Use 'hasShape' to select a different 'ShapeOutcome' than 'ShapeDefault'
nary ::
  -- | constructor of 'Node'
  (Args -> Node) ->
  -- | result
  OperationOption
nary op = Normal (OpMany op) ShapeDefault

-- | Construct a 'OperationOption' for a generic unary operator (of explicit 'ElementType').
--   Always returns the 'Normal' constructor with the input 'Node' wrapped in the 'OpManyElement' constructor.
--   Use 'hasShape' to select a different 'ShapeOutcome' than 'ShapeDefault'
naryET ::
  -- | constructor of 'Node'
  (ET -> Args -> Node) ->
  -- | specific 'ElementType' or default
  ElementOutcome ->
  -- | result
  OperationOption
naryET op elm = Normal (OpManyElement op elm) ShapeDefault

-- | Construct a 'OperationOption' for a generic piecewise function (of implicit 'ElementType').
--   A simple alias for the 'Condition' construtor
conditionAry :: (ConditionArg -> [BranchArg] -> Node) -> OperationOption
conditionAry = Condition

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
  Transformation -- resulting transformation
toTransformation normalizer exp@(mp, n) =
  let diff = normalizer exp
      newMp = IM.union mp (extraEntries diff)
      newN = newRootId diff
   in (newMp, newN)

-- | A Modification type takes a base 'Expression' (in unwrapped form) to propogate through a 'Change' combinator.
--   Use 'fromModification' in conjunction with 'toTransformation' to turn a 'Modification' into a 'Transformation'
type Modification = (ExpressionMap, NodeID) -> Change

-- | Takes a 'Modification' and returns a corresponding function
fromModification :: Modification -> ((ExpressionMap, NodeID) -> ExpressionDiff)
fromModification mkDiff exp@(mp, n) = mkDiff exp mp

-- | Operand order in the operation
data OperandOrder
  = Reorder
  | NoReorder
  deriving (Eq)

-- | Used to apply rules (which can be generated with 'fromModification') to every 'Node' in an 'Expression' bottom up
toRecursive ::
  -- | apply in topological order or not
  OperandOrder ->
  -- | rule applied to a single 'Node'
  ((ExpressionMap, NodeID) -> ExpressionDiff) ->
  ((ExpressionMap, NodeID) -> ExpressionDiff) -- resulting rule applied to every 'Node'
toRecursive operandOrder smp exp@(mp, headN) = fromJust $ IM.lookup headN diffs
  where
    topoOrder = topologicalSort exp
    f :: IM.IntMap ExpressionDiff -> NodeID -> IM.IntMap ExpressionDiff
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

-- --------------------------------------------------------------------------------------------------------------------

-- * Expression Changes
-- --------------------------------------------------------------------------------------------------------------------

-- | The Change type allows the creation of combinators for expressing alterations to an 'Expression' with respect
--   to a base 'ExpressionMap'. For example, combinators like 'sum_' will create will create a new 'ExpressionMap' with
--   a new root 'NodeID' (contained inside a 'ExpressionDiff') from a list of other 'Change'. By passing along the
--   base 'ExpressionMap' in each 'Change', we can assure there's no overlap when generating new 'Node'
type Change = ExpressionMap -> ExpressionDiff

-- | The 'ExpressionDiff' when adding a constant is just the constant node (generate by 'aConst')
const_ :: Shape -> Double -> Change
const_ shape val mp = ExpressionDiff mp n
  where
    (mp, n) = aConst shape val

-- | The 'Change' created when adding a single 'Scalar' constant
num_ :: Double -> Change
num_ = const_ []

-- | Multiply a list of 'Change' together into a single 'Change'
product_ :: [Change] -> Change
product_ changes mp = mulManyDiff mp . map ($ mp) $ changes

-- | Sum a list of 'Change' together into a single 'Change'
sum_ :: [Change] -> Change
sum_ changes mp = sumManyDiff mp . map ($ mp) $ changes

-- | Creates just a 'ExpressionDiff' with a empty 'ExpressionMap' and the given 'NodeID' as the root
just :: NodeID -> Change
just nId _ = ExpressionDiff IM.empty nId

instance Num Change where
  (+) change1 change2 mp = sumManyDiff mp [change1 mp, change2 mp]
  negate change mp = applyDiff mp (unaryET Neg ElementDefault) [change mp]
  (*) change1 change2 mp = mulManyDiff mp [change1 mp, change2 mp]
  signum = error "The Change of signum is currently unimplemented"
  abs = error "The Change of abs is currently unimplemented"
  fromInteger = error "The change of fromInteger is currently unimplemented"

instance Fractional Change where
  (/) change1 change2 = change1 * (change2 ^ (-1))
  fromRational r = error "N/A"

instance Floating Change where
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

-- --------------------------------------------------------------------------------------------------------------------

-- * Expression Diffs
-- --------------------------------------------------------------------------------------------------------------------

-- | When performing a 'Change' on an 'ExpressionMap', the extra entries created by the change
--   (i.e not included in the original 'ExpressionMap') and a new root 'NodeID' are stored in the ExpressionDiff type
data ExpressionDiff
  = ExpressionDiff
      { -- | Extra entries we need to add to the original Expression Map
        extraEntries :: ExpressionMap,
        -- | New root of the expression (can change, can be the same)
        newRootId :: NodeID
      }
  deriving (Eq, Ord, Show)

-- | Compute a new 'ExpressionDiff' (with respect to a base 'ExpressionMap') when applying an operation to
--   other 'ExpressionDiff'
applyDiff ::
  -- | the base map to find diffs w.r.t
  ExpressionMap ->
  -- | the operation to apply
  OperationOption ->
  -- | the operands (also in diff to the base map)
  [ExpressionDiff] ->
  -- | the result (combined diffs with new nodes)
  ExpressionDiff
applyDiff contextMp option operands = ExpressionDiff resExtraEntries resRootId
  where
    mergedExtraEntries = IM.unions . map extraEntries $ operands
    updatedContextMp = IM.union mergedExtraEntries contextMp
    ns = map newRootId operands
    (resExtraEntries, resRootId) =
      addEntryWithContext updatedContextMp mergedExtraEntries option ns

-- | The 'ExpressionDiff' when adding a constant is just the constant node (generate by 'aConst')
diffConst :: Shape -> Double -> ExpressionDiff
diffConst shape val = ExpressionDiff mp n
  where
    (mp, n) = aConst shape val

-- | Combine a list of 'ExpressionDiff' using 'Mul', generate new nodes with respect to a base 'ExpressionMap'
mulManyDiff ::
  -- | base map to find diff w.r.t
  ExpressionMap ->
  -- | operands
  [ExpressionDiff] ->
  -- | combined operands with new entries
  ExpressionDiff
mulManyDiff contextMp = applyDiff contextMp (naryET Mul ElementDefault)

-- | Combine a list of 'ExpressionDiff' using 'Sum', generate new nodes with respect to a base 'ExpressionMap'
sumManyDiff ::
  -- | base map to find diff w.r.t
  ExpressionMap ->
  -- | operands
  [ExpressionDiff] ->
  -- | combined operands with new entries
  ExpressionDiff
sumManyDiff contextMp = applyDiff contextMp (naryET Sum ElementDefault)

-- | The ExpressionDiff corresponding to no change in this node
noChange :: NodeID -> ExpressionDiff
noChange = ExpressionDiff IM.empty

-- | Same node type (Mul, Sum, Negate, ...), but children may changed, now make the same node type with new children
--   and return the combined difference
combineChildrenDiffs ::
  OperandOrder ->
  ExpressionMap ->
  NodeID ->
  [ExpressionDiff] ->
  ExpressionDiff
combineChildrenDiffs operandOrder contextMp n childrenDiffs
  | Sum et _ <- oldNode,
    operandOrder == Reorder =
    sortAndCombine (naryET Sum (ElementSpecific et))
  | Mul et _ <- oldNode,
    operandOrder == Reorder =
    sortAndCombine (naryET Mul (ElementSpecific et))
  | InnerProd R arg1 arg2 <- oldNode,
    operandOrder == Reorder =
    sortAndCombine (binaryET InnerProd (ElementSpecific R))
  | oldChildren == newChildren && all ((== IM.empty) . extraEntries) childrenDiffs = noChange n
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
                IM.lookup (newRootId diff) combinedExtraEntries =
              node
          nodeType diff1 diff2 = sameNodeType (getNode diff1) (getNode diff2)
          weight diff = nodeTypeWeight $ getNode diff
          sortArgs =
            concatMap (sortWith newRootId)
              . groupBy nodeType
              . sortWith weight
          sortedChildrenDiffs = sortArgs childrenDiffs
       in if oldChildren == map newRootId sortedChildrenDiffs
            && all (== IM.empty) (map extraEntries sortedChildrenDiffs)
            then noChange n
            else
              applyDiff
                contextMp
                (option `hasShape` oldShape)
                sortedChildrenDiffs

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

-- | Retrieves all 'Var' nodes in an 'Expression'
expressionVarNodes :: (DimensionType d, ElementType et) => Expression d et -> [(String, NodeID)]
expressionVarNodes (Expression n mp) = mapMaybe collect ns
  where
    ns = topologicalSort (mp, n)
    collect nId
      | Var varName <- retrieveNode nId mp = Just (varName, nId)
      | otherwise = Nothing

-- | Retrieves all 'Var' nodes in an (unwrapped) 'Expression'
varNodesWithId :: ExpressionMap -> [(String, NodeID)]
varNodesWithId mp = mapMaybe collect . IM.keys $ mp
  where
    collect nId
      | Var varName <- retrieveNode nId mp = Just (varName, nId)
      | otherwise = Nothing

-- | Predicate determining if a 'ExpressionMap' contains a FT operation
containsFTNode :: ExpressionMap -> Bool
containsFTNode mp = any isFT $ IM.elems mp
  where
    isFT (_, node) =
      case node of
        ReFT _ -> True
        ImFT _ -> True
        TwiceImFT _ -> True
        TwiceReFT _ -> True
        _ -> False
