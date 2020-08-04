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
-- Inner HashedExpression functionality, contains combinators for manually manipulating HashedExpressions including merging
-- expression maps, create new entry, etc.
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
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
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
apply option exps = (IM.insert resID resNode mergedMap, resID)
  where
    (mergedMap, nIDs) = safeMerges exps
    operands = zip nIDs $ map (`retrieveNode` mergedMap) nIDs
    resNode = createEntry option operands
    resID = hashNode (checkHashFromMap mergedMap) resNode

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
    len :: Int
    len = IM.size mp
    -------------------------------------------------------------------------------
    n2Pos :: IM.IntMap Int
    n2Pos = IM.fromList $ zip (IM.keys mp) [0 ..]
    -------------------------------------------------------------------------------
    toPos :: NodeID -> Int
    toPos nId = fromJust $ IM.lookup nId n2Pos
    -------------------------------------------------------------------------------
    adj :: NodeID -> [NodeID]
    adj nId = opArgs $ retrieveOp nId mp
    -------------------------------------------------------------------------------
    topoOrder =
      runSTUArray $ do
        -- if a node is visited
        marked <- newArray (0, len - 1) False :: ST s (STUArray s Int Bool)
        -- order of completing traversing NodeIDs
        order <- newArray (0, len - 1) (-1) :: ST s (STUArray s Int Int)
        -- count next index of order
        cnt <- newSTRef 0 :: ST s (STRef s Int)
        let dfs u = do
              let arrayPos = toPos u
              writeArray marked arrayPos True
              forM_ (adj u) $ \v -> do
                when (isJust $ IM.lookup v mp) $ do
                  isMarked <- readArray marked (toPos v)
                  unless isMarked $ dfs v
              cntVal <- readSTRef cnt
              writeArray order cntVal u
              writeSTRef cnt (cntVal + 1)
        forM_ ns $ \n -> do
          when (isJust $ IM.lookup n mp) $ do
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

toTotal :: IM.IntMap NodeID -> (NodeID -> NodeID)
toTotal mp nID = case IM.lookup nID mp of
  Just other -> other
  _ -> nID

-- | Merge the second map into the first map, resolve hash collision if occur
safeMerge :: ExpressionMap -> (ExpressionMap, NodeID) -> (ExpressionMap, NodeID)
safeMerge accMp (mp, n) =
  -- Merge the subexpression to main expression map
  -- produce `sub` as the map from old hash to new hash if there is any hash-collision
  let f :: (ExpressionMap, IM.IntMap NodeID) -> NodeID -> (ExpressionMap, IM.IntMap NodeID)
      f (acc, sub) nodeID =
        let -- the node needed to be added to accMp
            node = mapNode (toTotal sub) (retrieveNode nodeID mp)
            -- get the new hash that is collision-free to both acc and contextMp
            newNodeID = hashNode (checkHashFromMap accMp) node
         in ( IM.insert newNodeID node acc,
              if newNodeID == nodeID
                then sub
                else IM.insert nodeID newNodeID sub
            )
      -- Fold over all sub-expressions by topological order
      (mergedMap, finalSub) = foldl' f (accMp, IM.empty) $ topologicalSort (mp, n)
   in (mergedMap, toTotal finalSub n)

safeMerges :: [(ExpressionMap, NodeID)] -> (ExpressionMap, [NodeID])
safeMerges [] = (IM.empty, [])
safeMerges ((mp, n) : xs) = foldl' f (mp, [n]) xs
  where
    f :: (ExpressionMap, [NodeID]) -> (ExpressionMap, NodeID) -> (ExpressionMap, [NodeID])
    f (acc, accIds) (valMP, valNID) =
      let (mergedMap, nID) = safeMerge acc (valMP, valNID)
       in (mergedMap, accIds ++ [nID])

-- |
-- Create an node out of operation spec and its operands.
-- Operands must follow the spec in terms of input and output's element types & shape
createEntry ::
  -- | Operation specification
  OperationSpec ->
  -- | Operands
  [(NodeID, Node)] ->
  -- | Result Node
  Node
createEntry spec args =
  let shapeOf (nID, (shape, et, _)) = shape
      etOf (nID, (shape, et, _)) = et
      idOf (nID, (shape, et, _)) = nID
   in case (spec, args) of
        (Unary (UnarySpec toOp decideShape decideET), [arg]) ->
          ( decideShape (shapeOf arg),
            decideET (etOf arg),
            toOp (idOf arg)
          )
        (Binary (BinarySpec toOp decideShape decideET), [arg1, arg2]) ->
          ( decideShape (shapeOf arg1) (shapeOf arg2),
            decideET (etOf arg1) (etOf arg2),
            toOp (idOf arg1) (idOf arg2)
          )
        (Nary (NarySpec toOp decideShape decideET), args) ->
          ( decideShape (map shapeOf args),
            decideET (map etOf args),
            toOp (map idOf args)
          )
        (ConditionAry (ConditionarySpec toOp decideShape decideET), condition : branches) ->
          ( decideShape (shapeOf condition) (map shapeOf branches),
            decideET (etOf condition) (map etOf branches),
            toOp (idOf condition) (map idOf branches)
          )
        _ -> error "Unfaithful with operation spec"

-- | Operations such as ImFT, ReFT, etc are for internal used only.
class FTRelatedOp a b | a -> b where
  imFT :: a -> b
  reFT :: a -> b
  twiceImFT :: a -> b
  twiceReFT :: a -> b
