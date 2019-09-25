{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
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
import Data.List.Split (splitOn)
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
read2DValues :: FilePath -> IO (Array (Int, Int) Double)
read2DValues filePath = do
    rows <- lines <$> readFile filePath
    let doubleRows = map (map read . splitOn " ") rows
    let numRow = length doubleRows
        numCol = length . head $ doubleRows
        allDoubles = concat doubleRows
    return $ listArray ((0, 0), (numRow - 1, numCol - 1)) allDoubles

--        allDoubles = map read . concat $ rows
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
data Val
    = VScalar Double
    | V1D (Array Int Double)
    | V2D (Array (Int, Int) Double)
    | V3D (Array (Int, Int, Int) Double)
    deriving (Eq, Show, Ord)

type ValMaps = Map String Val

-- | 
--
valElems :: Val -> [Double]
valElems val =
    case val of
        VScalar v -> [v]
        V1D vs -> elems vs
        V2D vs -> elems vs
        V3D vs -> elems vs

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
