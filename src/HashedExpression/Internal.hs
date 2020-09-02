{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
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
import Data.Graph (buildG, topSort)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', groupBy, sort, sortBy, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Data.STRef.Strict
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace (traceShowId)
import GHC.Exts (sortWith)
import GHC.Stack (HasCallStack)
import HashedExpression.Internal.Base
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Hash
import HashedExpression.Internal.Node
import HashedExpression.Internal.OperationSpec
import HashedExpression.Internal.Utils
import HashedExpression.Prettify
import Prelude hiding ((^))

-- | Unwrap 'Expression' to a 'ExpressionMap' and root 'NodeID'
unwrap :: Expression d et -> (ExpressionMap, NodeID)
unwrap (Expression n mp) = (mp, n)

-- | Wrap a 'ExpressionMap' and root 'NodeID' into an "Expression" type
wrap :: (ExpressionMap, NodeID) -> Expression d et
wrap = uncurry $ flip Expression

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
apply option exps = (IM.insert resID resNode mergedMap, NodeID resID)
  where
    (mergedMap, nIDs) = safeMerges exps
    operands = zip nIDs $ map (`retrieveNode` mergedMap) nIDs
    resNode = createNode option operands
    resID = hashNode (checkCollisionMap mergedMap) resNode

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
-- --------------------------------------------------------------------------------------------------------------------

-- * Expression Transformations

-- --------------------------------------------------------------------------------------------------------------------

-- | Transformation type, take a (unwrapped) 'Expression' and return a transformed (unwrapped) 'Expression'.
--   Construct using the 'toTransformation' function
type Transformation = (ExpressionMap, NodeID) -> (ExpressionMap, NodeID)

-- | Remove unreachable nodes
removeUnreachable :: Transformation
removeUnreachable (mp, n) =
  let reachableNodes = IS.fromList . map unNodeID . topologicalSort $ (mp, n)
      reducedMap =
        IM.filterWithKey (\nId _ -> IS.member nId reachableNodes) mp -- Only keep those in reachable nodes
   in (reducedMap, n)

removeUnreachableManyRoots :: (ExpressionMap, [NodeID]) -> ExpressionMap
removeUnreachableManyRoots (mp, nodeIDs) =
  let reachableNodes = IS.fromList . map unNodeID . topologicalSortManyRoots $ (mp, nodeIDs)
   in IM.filterWithKey (\nId _ -> IS.member nId reachableNodes) mp -- Only keep those in reachable nodes

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
topologicalSortManyRoots (mp, ns) = map NodeID $ filter (/= -1) . UA.elems $ topoOrder
  where
    len :: Int
    len = IM.size mp
    -------------------------------------------------------------------------------
    n2Pos :: IM.IntMap Int
    n2Pos = IM.fromList $ zip (IM.keys mp) [0 ..]
    -------------------------------------------------------------------------------
    toPos :: NodeID -> Int
    toPos (NodeID nId) = fromJust $ IM.lookup nId n2Pos
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
                isMarked <- readArray marked (toPos v)
                unless isMarked $ dfs v
              cntVal <- readSTRef cnt
              writeArray order cntVal (unNodeID u)
              writeSTRef cnt (cntVal + 1)
        forM_ ns $ \n -> do
          isMarked <- readArray marked (toPos n)
          unless isMarked $ dfs n
        return order

-- | Retrieves all 'Var' nodes in an 'Expression'
expressionVarNodes :: (Dimension d) => Expression d et -> [(String, NodeID)]
expressionVarNodes (Expression n mp) = mapMaybe collect ns
  where
    ns = topologicalSort (mp, n)
    collect nId
      | Var varName <- retrieveOp nId mp = Just (varName, nId)
      | otherwise = Nothing

toTotal :: Map NodeID NodeID -> (NodeID -> NodeID)
toTotal mp nID = case Map.lookup nID mp of
  Just other -> other
  _ -> nID

safeMergeManyRoots :: ExpressionMap -> (ExpressionMap, [NodeID]) -> (ExpressionMap, [NodeID])
safeMergeManyRoots accMp (mp, ns) =
  -- Merge the subexpression to main expression map
  -- produce `sub` as the map from old hash to new hash if there is any hash-collision
  let f :: (ExpressionMap, Map NodeID NodeID) -> NodeID -> (ExpressionMap, Map NodeID NodeID)
      f (acc, sub) nodeID =
        let -- the node needed to be added to accMp
            node = mapNode (toTotal sub) (retrieveNode nodeID mp)
            -- get the new hash that is collision-free to both acc and contextMp
            newNodeID@(NodeID h) = NodeID $ hashNode (checkCollisionMap accMp) node
         in ( IM.insert h node acc,
              if newNodeID == nodeID
                then sub
                else Map.insert nodeID newNodeID sub
            )
      -- Fold over all sub-expressions by topological order
      (mergedMap, finalSub) = foldl' f (accMp, Map.empty) $ topologicalSortManyRoots (mp, ns)
   in (mergedMap, map (toTotal finalSub) ns)

-- | Merge the second map into the first map, resolve hash collision if occur
safeMerge :: ExpressionMap -> (ExpressionMap, NodeID) -> (ExpressionMap, NodeID)
safeMerge accMp (mp, n) =
  let (resMp, [resN]) = safeMergeManyRoots accMp (mp, [n])
   in (resMp, resN)

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
createNode ::
  -- | Operation specification
  OperationSpec ->
  -- | Operands
  [(NodeID, Node)] ->
  -- | Result Node
  Node
createNode spec args =
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

-------------------------------------------------------------------------------

-- | Create an Expression from a standalone 'Node'
fromNode :: Node -> Expression d et
fromNode node = Expression (NodeID h) (IM.insert h node IM.empty)
  where
    checkCollision = checkCollisionMap IM.empty
    h = hashNode checkCollision node

-- | Create an unwrapped Expresion from a standalone 'Node'
fromNodeUnwrapped :: Node -> (ExpressionMap, NodeID)
fromNodeUnwrapped = unwrap . fromNode

extract :: ExpressionMap -> ((Int, Node) -> Maybe a) -> [a]
extract mp collect = mapMaybe collect $ IM.toList mp

-- | Retrieves all 'Var' nodes in an (unwrapped) 'Expression'
varsWithNodeID :: ExpressionMap -> [(String, NodeID)]
varsWithNodeID mp = extract
  mp
  \case
    (nId, (_, _, Var name)) -> Just (name, NodeID nId)
    _ -> Nothing

paramsWithNodeID :: ExpressionMap -> [(String, NodeID)]
paramsWithNodeID mp = extract mp $
  \case
    (nId, (_, _, Param name)) -> Just (name, NodeID nId)
    _ -> Nothing

varNodes :: ExpressionMap -> [(String, Shape, NodeID)]
varNodes mp = extract mp $
  \case
    (nID, (shape, _, Var varName)) -> Just (varName, shape, NodeID nID)
    _ -> Nothing

paramNodes :: ExpressionMap -> [(String, Shape, NodeID)]
paramNodes mp = extract mp $
  \case
    (nID, (shape, _, Param varName)) -> Just (varName, shape, NodeID nID)
    _ -> Nothing

-- | Predicate determining if a 'ExpressionMap' contains a FT operation
containsFTNode :: ExpressionMap -> Bool
containsFTNode mp = any isFT $ IM.elems mp
  where
    isFT (_, _, FT _) = True
    isFT (_, _, IFT _) = True
    isFT _ = False

nodeIDs :: ExpressionMap -> [NodeID]
nodeIDs = map NodeID . IM.keys

-------------------------------------------------------------------------------

-- | All the entries of the expression
allEntries :: Expression d et -> [(NodeID, String)]
allEntries (Expression n mp) =
  zip (nodeIDs mp) . map (T.unpack . hiddenPrettify False . (mp,)) $ nodeIDs mp

-- | Print every entry (individually) of an 'Expression', in a format that (in general) you should be able to enter into ghci
allEntriesDebug :: (ExpressionMap, NodeID) -> [(NodeID, String)]
allEntriesDebug (mp, n) =
  zip (nodeIDs mp) . map (T.unpack . hiddenPrettify False . (mp,)) $ nodeIDs mp
