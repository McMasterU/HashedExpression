module HashedExpression.Internal.Utils where

import Data.Array
import Data.Complex
import qualified Data.IntMap.Strict as IM
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
mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

-- |
mapSecond :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSecond _ [] = []
mapSecond f ((x, y) : rest) = (x, f y) : mapSecond f rest

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

ensureSameShape :: Expression d et1 -> Expression d et2 -> a -> a
ensureSameShape e1 e2 after
  | expressionShape e1 == expressionShape e2 = after
  | otherwise =
    error $
      "Ensure same shape failed "
        ++ show (prettifyDebug e1)
        ++ " "
        ++ show (prettifyDebug e2)

ensureSameShapeList :: [Expression d et] -> a -> a
ensureSameShapeList es after
  | allEqual (map expressionShape es) = after
  | otherwise =
    error $ "Ensure same shape failed " ++ show (map expressionShape es)

constWithShape :: Shape -> Double -> Expression d R
constWithShape shape val = fromNode (shape, R, Const val)

varWithShape :: Shape -> String -> (ExpressionMap, NodeID)
varWithShape shape name = fromNodeUnwrapped (shape, R, Var name)

-- |
isScalarShape :: Shape -> Bool
isScalarShape = null

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
isDZero :: ExpressionMap -> NodeID -> Bool
isDZero mp nId = retrieveOp nId mp == DZero

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

-- |
dVarWithShape :: Shape -> String -> (ExpressionMap, NodeID)
dVarWithShape shape name = fromNodeUnwrapped (shape, Covector, DVar name)

showT :: Show a => a -> T.Text
showT = T.pack . show

-- |
maybeVariable :: DimensionType d => Expression d R -> Maybe (String, Shape)
maybeVariable (Expression nID mp) = case retrieveNode nID mp of
  (shape, R, Var name) -> Just (name, shape)
  _ -> Nothing


-- | Retrieves all 'Var' nodes in an (unwrapped) 'Expression'
varNodesWithId :: ExpressionMap -> [(String, NodeID)]
varNodesWithId mp = mapMaybe collect . IM.keys $ mp
  where
    collect nId
      | Var varName <- retrieveOp nId mp = Just (varName, nId)
      | otherwise = Nothing

-- | Predicate determining if a 'ExpressionMap' contains a FT operation
containsFTNode :: ExpressionMap -> Bool
containsFTNode mp = any isFT $ IM.elems mp
  where
    isFT (_, _, op) =
      case op of
        ReFT _ -> True
        ImFT _ -> True
        TwiceImFT _ -> True
        TwiceReFT _ -> True
        _ -> False

-------------------------------------------------------------------------------

-- | MARK: (+)

-------------------------------------------------------------------------------
instance Num (Array Int Double) where
  (+) arr1 arr2 =
    listArray
      (0, size - 1)
      [x + y | i <- [0 .. size - 1], let x = arr1 ! i, let y = arr2 ! i]
    where
      size = length . elems $ arr1
  negate arr =
    listArray (0, size - 1) [- x | i <- [0 .. size - 1], let x = arr ! i]
    where
      size = length . elems $ arr
  (*) arr1 arr2 =
    listArray
      (0, size - 1)
      [x * y | i <- [0 .. size - 1], let x = arr1 ! i, let y = arr2 ! i]
    where
      size = length . elems $ arr1
  abs = error "TODO"
  signum = error "N/A"
  fromInteger = error "TODO"

instance Num (Array Int (Complex Double)) where
  (+) arr1 arr2 =
    listArray
      (0, size - 1)
      [x + y | i <- [0 .. size - 1], let x = arr1 ! i, let y = arr2 ! i]
    where
      size = length . elems $ arr1
  negate arr =
    listArray (0, size - 1) [- x | i <- [0 .. size - 1], let x = arr ! i]
    where
      size = length . elems $ arr
  (*) arr1 arr2 =
    listArray
      (0, size - 1)
      [x * y | i <- [0 .. size - 1], let x = arr1 ! i, let y = arr2 ! i]
    where
      size = length . elems $ arr1
  abs = error "TODO"
  signum = error "N/A"
  fromInteger = error "TODO"

instance PowerOp Double Int where
  (^) x y = x Prelude.^ y
