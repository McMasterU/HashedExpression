module HashedUtils where

import Data.Array
import qualified Data.IntMap.Strict as IM
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import qualified Data.Set as Set
import HashedExpression
import HashedHash
import HashedNode

import Data.Complex
import Data.Maybe
import GHC.IO.Unsafe (unsafePerformIO)

-- | Forward pipe operator in Elm
--
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

infixl 1 |>

-- |
--
mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

-- |
--
bringMaybeOut :: (Maybe a, Maybe b) -> Maybe (a, b)
bringMaybeOut (Just x, Just y) = Just (x, y)
bringMaybeOut _ = Nothing

-- | Check if all elements of the list is equal
--
allEqual :: (Eq a) => [a] -> Bool
allEqual xs = and $ zipWith (==) (safeTail xs) xs
  where
    safeTail [] = []
    safeTail (x:xs) = xs

fromR :: Double -> Complex Double
fromR x = x :+ 0

ensureSameShape :: Expression d et1 -> Expression d et2 -> a -> a
ensureSameShape e1 e2 after
    | expressionShape e1 == expressionShape e2 = after
    | otherwise =
        error $
        "Ensure same shape failed " ++
        show (expressionShape e1) ++ " " ++ show (expressionShape e2)

ensureSameShapeList :: [Expression d et] -> a -> a
ensureSameShapeList es after
    | allEqual (map expressionShape es) = after
    | otherwise =
        error $ "Ensure same shape failed " ++ show (map expressionShape es)

constWithShape :: Shape -> Double -> Expression d R
constWithShape shape val = Expression h (IM.fromList [(h, node)])
  where
    node = (shape, Const val)
    h = hash node

pullConstant :: ExpressionMap -> Int -> Maybe (Shape, Double)
pullConstant mp n
    | (shape, Const c) <- retrieveInternal n mp = Just (shape, c)
    | otherwise = Nothing

pullConstants :: ExpressionMap -> [Int] -> Maybe (Shape, [Double])
pullConstants mp ns
    | xs@(x:_) <- mapMaybe (pullConstant mp) ns = Just (fst x, map snd xs)
    | otherwise = Nothing

isZero :: ExpressionMap -> Int -> Bool
isZero mp nId
    | Const 0 <- retrieveNode nId mp = True
    | otherwise = False

isOne :: ExpressionMap -> Int -> Bool
isOne mp nId
    | Const 1 <- retrieveNode nId mp = True
    | otherwise = False

isConstant :: ExpressionMap -> Int -> Bool
isConstant mp nId
    | Const _ <- retrieveNode nId mp = True
    | otherwise = False

pullSumOperands :: ExpressionMap -> Int -> [Int]
pullSumOperands mp nId
    | Sum _ operands <- retrieveNode nId mp = operands
    | otherwise = [nId]

pullProdOperands :: ExpressionMap -> Int -> [Int]
pullProdOperands mp nId
    | Mul _ operands <- retrieveNode nId mp = operands
    | otherwise = [nId]

aConst :: Shape -> Double -> (ExpressionMap, Int)
aConst shape val = (IM.fromList [(h, node)], h)
  where
    node = (shape, Const val)
    h = hash node

-- |
--
data ValMaps =
    ValMaps
        { vm0 :: Map String Double
        , vm1 :: Map String (Array Int Double)
        , vm2 :: Map String (Array (Int, Int) Double)
        , vm3 :: Map String (Array (Int, Int, Int) Double)
        }
    deriving (Eq, Show, Ord)

emptyVms :: ValMaps
emptyVms =
    ValMaps {vm0 = Map.empty, vm1 = Map.empty, vm2 = Map.empty, vm3 = Map.empty}

-- | Helpers so we can write things like
-- emptyVms |> withVm0 (..) |> withVm1 (..) |> withVM2 (..)
--
withVm0 :: Map String Double -> ValMaps -> ValMaps
withVm0 vm0 (ValMaps _ vm1 vm2 vm3) = ValMaps vm0 vm1 vm2 vm3

withVm1 :: Map String (Array Int Double) -> ValMaps -> ValMaps
withVm1 vm1 (ValMaps vm0 _ vm2 vm3) = ValMaps vm0 vm1 vm2 vm3

withVm2 :: Map String (Array (Int, Int) Double) -> ValMaps -> ValMaps
withVm2 vm2 (ValMaps vm0 vm1 _ vm3) = ValMaps vm0 vm1 vm2 vm3

withVm3 :: Map String (Array (Int, Int, Int) Double) -> ValMaps -> ValMaps
withVm3 vm3 (ValMaps vm0 vm1 vm2 _) = ValMaps vm0 vm1 vm2 vm3
