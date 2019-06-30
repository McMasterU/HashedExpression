module HashedUtils where

import qualified Data.IntMap.Strict as IM
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
    | otherwise = error "Ensure same shape failed"

ensureSameShapeList :: [Expression d et] -> a -> a
ensureSameShapeList es after
    | allEqual es = after
    | otherwise = error "Ensure same shape failed"

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

inspect :: Show a => a -> a
inspect x = unsafePerformIO $ do
    print x
    return x

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
