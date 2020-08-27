module HashedExpression.Internal.Utils where

import Data.Array
import Data.Complex (Complex (..))
import qualified Data.Complex as Complex
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time (diffUTCTime, getCurrentTime)
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.Stack (HasCallStack)
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Hash
import HashedExpression.Internal.Node
import HashedExpression.Prettify
import Prelude hiding ((^))
import qualified Prelude

-- | Forward pipe operator in Elm
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

infixl 1 |>

-- | Chain a list of endomorphisms to a endomorphism
chain :: [a -> a] -> a -> a
chain = flip $ foldl (|>)

-- |
measureTime :: IO a -> IO ()
measureTime action = do
  beforeTime <- getCurrentTime
  action
  afterTime <- getCurrentTime
  putStrLn $ "Took " ++ show (diffUTCTime afterTime beforeTime) ++ " seconds"

-- | Check if all elements of the list is equal
allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual [x] = True
allEqual (x : y : xs) = x == y && allEqual (y : xs)

fromR :: Double -> Complex Double
fromR x = x :+ 0

varWithShape :: Shape -> String -> (ExpressionMap, NodeID)
varWithShape shape name = fromNodeUnwrapped (shape, R, Var name)

paramWithShape :: Shape -> String -> (ExpressionMap, NodeID)
paramWithShape shape name = fromNodeUnwrapped (shape, R, Param name)

-- |
pullConstant :: ExpressionMap -> NodeID -> Maybe (Shape, Double)
pullConstant mp n
  | (shape, R, Const c) <- retrieveNode n mp = Just (shape, c)
  | otherwise = Nothing

-- |
pullConstants :: ExpressionMap -> [NodeID] -> Maybe (Shape, [Double])
pullConstants mp ns
  | xs@(x : _) <- mapMaybe (pullConstant mp) ns = Just (fst x, map snd xs)
  | otherwise = Nothing

-- |
isZero :: ExpressionMap -> NodeID -> Bool
isZero mp nId
  | Const 0 <- retrieveOp nId mp = True
  | RealImag arg1 arg2 <- retrieveOp nId mp,
    Const 0 <- retrieveOp arg1 mp,
    Const 0 <- retrieveOp arg2 mp =
    True
  | otherwise = False

-- |
isOne :: ExpressionMap -> NodeID -> Bool
isOne mp nId
  | Const 1 <- retrieveOp nId mp = True
  | RealImag arg1 arg2 <- retrieveOp nId mp,
    Const 1 <- retrieveOp arg1 mp,
    Const 0 <- retrieveOp arg2 mp =
    True
  | otherwise = False

-- |
isConstant :: ExpressionMap -> NodeID -> Bool
isConstant mp nId
  | Const _ <- retrieveOp nId mp = True
  | otherwise = False

-- |
pullSumOperands :: ExpressionMap -> NodeID -> [NodeID]
pullSumOperands mp nId
  | Sum operands <- retrieveOp nId mp = operands
  | otherwise = [nId]

-- |
pullProdOperands :: ExpressionMap -> NodeID -> [NodeID]
pullProdOperands mp nId
  | Mul operands <- retrieveOp nId mp = operands
  | otherwise = [nId]

-- |
aConst :: Shape -> Double -> (ExpressionMap, NodeID)
aConst shape val = fromNodeUnwrapped (shape, R, Const val)

showT :: Show a => a -> T.Text
showT = T.pack . show

-- |
maybeVariable :: Dimension d => Expression d R -> Maybe (String, Shape)
maybeVariable (Expression nID mp) = case retrieveNode nID mp of
  (shape, R, Var name) -> Just (name, shape)
  _ -> Nothing

-- | Retrieves all 'Var' nodes in an (unwrapped) 'Expression'
varNodesWithId :: ExpressionMap -> [(String, NodeID)]
varNodesWithId mp = mapMaybe (collect . NodeID) . IM.keys $ mp
  where
    collect nId
      | Var name <- retrieveOp nId mp = Just (name, nId)
      | otherwise = Nothing

paramNodesWithId :: ExpressionMap -> [(String, NodeID)]
paramNodesWithId mp = mapMaybe (collect . NodeID) . IM.keys $ mp
  where
    collect nId
      | Param name <- retrieveOp nId mp = Just (name, nId)
      | otherwise = Nothing

varNodes :: ExpressionMap -> [(String, Shape, NodeID)]
varNodes mp = mapMaybe collect $ IM.toList mp
  where
    collect (nID, (shape, _, Var varName)) = Just (varName, shape, NodeID nID)
    collect _ = Nothing

-- | Retrieves all 'Var' nodes in an (unwrapped) 'Expression'
varNodesWithShape :: ExpressionMap -> [(String, Shape)]
varNodesWithShape mp = mapMaybe collect $ IM.toList mp
  where
    collect (nID, (shape, _, Var varName)) = Just (varName, shape)
    collect _ = Nothing

-- | Predicate determining if a 'ExpressionMap' contains a FT operation
containsFTNode :: ExpressionMap -> Bool
containsFTNode mp = any isFT $ IM.elems mp
  where
    isFT (_, _, op) =
      case op of
        FT _ -> True
        IFT _ -> True
        _ -> False

toRange :: DimSelector -> Int -> (Int, Int, Int)
toRange (At i) size = (i, i, 1)
toRange (Range start end step) size = (start, if end >= start then end else end + size, step)

mkIndices :: DimSelector -> Int -> [Int]
mkIndices (At i) size = [i]
mkIndices (Range start end step) size =
  map (`mod` size) [start, start + step .. if end >= start then end else end + size]

-------------------------------------------------------------------------------
zipMp :: forall a b c. Ord a => Map a b -> Map a c -> Map a (b, c)
zipMp mp1 mp2 = foldl' f Map.empty $ Map.keys mp1
  where
    f acc k = case (Map.lookup k mp1, Map.lookup k mp2) of
      (Just v1, Just v2) -> Map.insert k (v1, v2) acc
      _ -> acc

-------------------------------------------------------------------------------
zipMp3 :: forall a b c d. Ord a => Map a b -> Map a c -> Map a d -> Map a (b, c, d)
zipMp3 mp1 mp2 mp3 = foldl' f Map.empty $ Map.keys mp1
  where
    f acc k = case (Map.lookup k mp1, Map.lookup k mp2, Map.lookup k mp3) of
      (Just v1, Just v2, Just v3) -> Map.insert k (v1, v2, v3) acc
      _ -> acc

instance PowerOp Double Int where
  (^) x y = x Prelude.^ y

instance ComplexRealOp Double (Complex Double) where
  (+:) = (:+)
  xRe = Complex.realPart
  xIm = Complex.imagPart
  conjugate = Complex.conjugate

-------------------------------------------------------------------------------
class ToText a where
  tt :: a -> T.Text
  ttq :: a -> T.Text
  ttq s = "\"" <> tt s <> "\""

instance ToText Double where
  tt s = T.pack $ show s

instance ToText Int where
  tt s = T.pack $ show s

instance ToText String where
  tt = T.pack

instance ToText T.Text where
  tt = id
