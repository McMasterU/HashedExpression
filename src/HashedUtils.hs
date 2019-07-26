{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module HashedUtils where

import Data.Array
import qualified Data.IntMap.Strict as IM
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import qualified Data.Set as Set
import HashedExpression
import HashedHash
import HashedNode
import HashedPrettify
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
    , cos
    , cosh
    , exp
    , negate
    , sin
    , sinh
    , sqrt
    , tan
    , tanh
    )
import qualified Prelude

import Data.Complex
import Data.Maybe
import Data.Time (diffUTCTime, getCurrentTime)
import GHC.IO.Unsafe (unsafePerformIO)

-- | Forward pipe operator in Elm
--
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

infixl 1 |>

-- | Chain a list of endomorphisms to a endomorphism
--
chain :: [a -> a] -> a -> a
chain = flip $ foldl (|>)

-- |
--
mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

-- |
--
measureTime :: IO a -> IO ()
measureTime action = do
    beforeTime <- getCurrentTime
    action
    afterTime <- getCurrentTime
    putStrLn $ "Took " ++ show (diffUTCTime afterTime beforeTime) ++ " seconds"

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
        show (prettifyDebug e1) ++ " " ++ show (prettifyDebug e2)

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

-- |
--
isScalarShape :: Shape -> Bool
isScalarShape = null

-- |
--
pullConstant :: ExpressionMap -> Int -> Maybe (Shape, Double)
pullConstant mp n
    | (shape, Const c) <- retrieveInternal n mp = Just (shape, c)
    | otherwise = Nothing

-- |
--
pullConstants :: ExpressionMap -> [Int] -> Maybe (Shape, [Double])
pullConstants mp ns
    | xs@(x:_) <- mapMaybe (pullConstant mp) ns = Just (fst x, map snd xs)
    | otherwise = Nothing

-- |
--
isZero :: ExpressionMap -> Int -> Bool
isZero mp nId
    | Const 0 <- retrieveNode nId mp = True
    | RealImag arg1 arg2 <- retrieveNode nId mp
    , Const 0 <- retrieveNode arg1 mp
    , Const 0 <- retrieveNode arg2 mp = True
    | otherwise = False

-- |
--
isOne :: ExpressionMap -> Int -> Bool
isOne mp nId
    | Const 1 <- retrieveNode nId mp = True
    | RealImag arg1 arg2 <- retrieveNode nId mp
    , Const 1 <- retrieveNode arg1 mp
    , Const 0 <- retrieveNode arg2 mp = True
    | otherwise = False

-- |
--
isConstant :: ExpressionMap -> Int -> Bool
isConstant mp nId
    | Const _ <- retrieveNode nId mp = True
    | otherwise = False

-- |
--
pullSumOperands :: ExpressionMap -> Int -> [Int]
pullSumOperands mp nId
    | Sum _ operands <- retrieveNode nId mp = operands
    | otherwise = [nId]

-- |
--
pullProdOperands :: ExpressionMap -> Int -> [Int]
pullProdOperands mp nId
    | Mul _ operands <- retrieveNode nId mp = operands
    | otherwise = [nId]

-- |
--
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

mergeValMaps :: ValMaps -> ValMaps -> ValMaps
mergeValMaps (ValMaps vm10 vm11 vm12 vm13) (ValMaps vm20 vm21 vm22 vm23) =
    ValMaps
        (Map.union vm10 vm20)
        (Map.union vm11 vm21)
        (Map.union vm12 vm22)
        (Map.union vm13 vm23)

-- | Prelude version of * and +
--
times :: (Num a) => a -> a -> a
times a b = Prelude.product [a, b]

plus :: (Num a) => a -> a -> a
plus a b = Prelude.sum [a, b]

-------------------------------------------------------------------------------
-- | MARK: (+)
--
--
-------------------------------------------------------------------------------
instance {-# OVERLAPPABLE #-} Num a => AddableOp a where
    (+) = plus

instance AddableOp (Array Int Double) where
    (+) arr1 arr2 =
        listArray
            (0, size - 1)
            [x + y | i <- [0 .. size - 1], let x = arr1 ! i, let y = arr2 ! i]
      where
        size = length . elems $ arr1

instance AddableOp (Array Int (Complex Double)) where
    (+) arr1 arr2 =
        listArray
            (0, size - 1)
            [x + y | i <- [0 .. size - 1], let x = arr1 ! i, let y = arr2 ! i]
      where
        size = length . elems $ arr1

-------------------------------------------------------------------------------
-- | MARK: Negate
--
--
-------------------------------------------------------------------------------
instance {-# OVERLAPPABLE #-} Num a => NegateOp a where
    negate = Prelude.negate

instance NegateOp (Array Int Double) where
    negate arr =
        listArray (0, size - 1) [-x | i <- [0 .. size - 1], let x = arr ! i]
      where
        size = length . elems $ arr

instance NegateOp (Array Int (Complex Double)) where
    negate arr =
        listArray (0, size - 1) [-x | i <- [0 .. size - 1], let x = arr ! i]
      where
        size = length . elems $ arr

-------------------------------------------------------------------------------
-- | MARK: (*)
--
--
--
-------------------------------------------------------------------------------
instance {-# OVERLAPPABLE #-} Num a => MultiplyOp a where
    (*) = times

instance MultiplyOp (Array Int Double) where
    (*) arr1 arr2 =
        listArray
            (0, size - 1)
            [x * y | i <- [0 .. size - 1], let x = arr1 ! i, let y = arr2 ! i]
      where
        size = length . elems $ arr1

instance MultiplyOp (Array Int (Complex Double)) where
    (*) arr1 arr2 =
        listArray
            (0, size - 1)
            [x * y | i <- [0 .. size - 1], let x = arr1 ! i, let y = arr2 ! i]
      where
        size = length . elems $ arr1

-- | MARK: Other
--
instance PowerOp Double Int where
    (^) x y = x Prelude.^ y

instance {-# OVERLAPPABLE #-} (Num a, Floating a) => NumOp a where
    sqrt = Prelude.sqrt
    exp = Prelude.exp
    log = Prelude.log
    sin = Prelude.sin
    cos = Prelude.cos
    tan = Prelude.tan
    asin = Prelude.asin
    acos = Prelude.acos
    atan = Prelude.atan
    sinh = Prelude.sinh
    cosh = Prelude.cosh
    tanh = Prelude.tanh
    asinh = Prelude.asinh
    acosh = Prelude.acosh
    atanh = Prelude.atanh
    (/) x y = x Prelude./ y
