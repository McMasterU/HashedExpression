module HashedExpression.Utils where

import Data.Array
import Data.Complex (Complex (..))
import qualified Data.Complex as Complex
import Data.Function ((&))
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
import HashedExpression.Internal.Base
import HashedExpression.Internal.Node
import Prelude hiding ((^))
import qualified Prelude

-- | Forward pipe operator
(|>) :: a -> (a -> b) -> b
(|>) = (&)

infixl 1 |>

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

toRange :: DimSelector -> Int -> (Int, Int, Int)
toRange (At i) size = (i, i, 1)
toRange (Range start end step) size = (start, if end >= start then end else end + size, step)

mkIndices :: DimSelector -> Int -> [Int]
mkIndices (At i) size = [i]
mkIndices (Range start end step) size =
  map (`mod` size) [start, start + step .. if end >= start then end else end + size]

breakSub :: Int -> [a] -> [([a], [a], [a])]
breakSub len [] = []
breakSub len xs | len > length xs = []
breakSub len xs@(y : ys) =
  ([], take len xs, drop len xs) :
  map (\(prefix, sub, suffix) -> (y : prefix, sub, suffix)) (breakSub len ys)

coercible :: Shape -> Shape -> Bool
coercible sml target =
  any
    ( \(prefix, sub, suffix) ->
        sub == sml && all (== 1) prefix && all (== 1) suffix
    )
    $ breakSub (length sml) target

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
