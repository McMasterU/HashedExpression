module CollisionSpec where

import Control.Monad (foldM, forM_, replicateM)
import Control.Monad.HT (nest)
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate, sort)
import qualified Data.Set as Set
import HashedExpression.Internal
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Hash
import HashedExpression.Prettify
import Test.HUnit (assertBool, assertEqual, (@=?))
import Test.Hspec
import Test.QuickCheck

makeDumbHash :: [Int] -> IO (CheckHash -> Node -> NodeID)
makeDumbHash candidates = do
  listIDs <- generate $ shuffle candidates
  let dumbHash :: CheckHash -> Node -> NodeID
      dumbHash checkHash node = case dropWhile (== IsClash) $ map (checkHash node) listIDs of
        (IsOk h : _) -> h
  return dumbHash

-- | (contextMp, accMp, diff, hash space)
--   * accMp and diff are collision-free to contextMp
--   * diff is possibly not collision-free to accMp
--   * hash space should be enough to index all nodes
data CollisionSample
  = CollisionSample
      ExpressionMap -- contextMp
      ExpressionMap -- accMp
      ExpressionDiff -- diff
      [Int] -- hashSpace
  deriving (Show, Eq, Ord)

instance Arbitrary CollisionSample where
  arbitrary = do
    -- generate context map
    varNames <- sublistOf $ map show ['a' .. 'z']
    let baseNodes :: [(NodeID, Node)]
        baseNodes = zip [1 ..] $ map (([],R,) . Var) varNames ++ map (([],R,) . Const) [1 .. 15]
    let f :: [(NodeID, Node)] -> Gen [(NodeID, Node)]
        f previous = do
          ls <- vectorOf 3 $ elements $ map fst previous
          let [x, y, z] = ls
          nextOp <- elements [Sum [x, y, z], Sum [x, y], Mul [x, y], InnerProdD x y, Div x y, Rotate [] x, ReFT x, ReFT y]
          return $ previous ++ [(1 + maximum (map fst previous), ([], R, nextOp))]
    numExtra <- elements [15 .. 20]
    contextEntries <- nest numExtra f baseNodes
    -- generate accumulate expression map
    -- accumulate is built on top of contextEntries so no hash-collision to contextMp
    numExtra <- elements [15 .. 20]
    accEntries <- drop (length contextEntries - 10) <$> nest numExtra f contextEntries
    -- diff
    -- diff is built on top of contextEntries so no hash-collision to contextMp (but there are hash-collisions to accMp)
    numExtra <- elements [15 .. 20]
    diffEntries <- drop (length contextEntries - 10) <$> nest numExtra f contextEntries
    let (diffExtraEntries, diffRootID) = removeUnreachable (IM.fromList diffEntries, fst $ last diffEntries)
    -- hash space
    let hashSpace = [1 .. length contextEntries + length accEntries + length diffEntries]
    return $
      CollisionSample
        (IM.fromList contextEntries)
        (IM.fromList accEntries)
        (ExpressionDiff diffExtraEntries diffRootID)
        hashSpace

prop_safeMergeDiffShouldResolveCollision :: CollisionSample -> Expectation
prop_safeMergeDiffShouldResolveCollision (CollisionSample contextMp accMp diff hashSpace) = do
  results <- replicateM 50 $ do
    dumbHash <- makeDumbHash hashSpace
    let (mp, _) = safeMergeDiff dumbHash contextMp accMp diff
    let ids = IM.keys mp
    -- All the subexpresisons as strings (thus only expression's structure and semantic matters, NodeIDs are irrelevant) concatenate
    return $ intercalate "\n" $ Set.toList $ Set.fromList $ map (debugPrint . (mp `IM.union` contextMp,)) ids
  -- Result should be the same regardless of how hashing played out
  forM_ (zip results (tail results)) $ \(x, y) -> do
    x @=? y

spec :: Spec
spec = describe "Handle hash collision spec" $
  specify "safe merge diff should resolve collision and preserve diff's semantic" $ do
    property prop_safeMergeDiffShouldResolveCollision
