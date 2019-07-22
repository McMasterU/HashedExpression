{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HashedUtils where

import Data.Array
import qualified Data.IntMap.Strict as IM
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import qualified Data.Set as Set
import HashedExpression
import HashedHash
import HashedNode
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
    | RealImag arg1 arg2 <- retrieveNode nId mp
    , Const 0 <- retrieveNode arg1 mp
    , Const 0 <- retrieveNode arg2 mp = True
    | otherwise = False

isOne :: ExpressionMap -> Int -> Bool
isOne mp nId
    | Const 1 <- retrieveNode nId mp = True
    | RealImag arg1 arg2 <- retrieveNode nId mp
    , Const 1 <- retrieveNode arg1 mp
    , Const 0 <- retrieveNode arg2 mp = True
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
-- | Instance of built-in types for our type classes
--
--
-------------------------------------------------------------------------------
-- | MARK: (+)
--
instance {-# OVERLAPPABLE #-} Num a => AddableOp a where
    (+) = plus
    negate = Prelude.negate

instance AddableOp (Array Int Double) where
    (+) arr1 arr2 =
        listArray
            (0, size - 1)
            [x + y | i <- [0 .. size - 1], let x = arr1 ! i, let y = arr2 ! i]
      where
        size = length . elems $ arr1
    negate arr =
        listArray (0, size - 1) [-x | i <- [0 .. size - 1], let x = arr ! i]
      where
        size = length . elems $ arr

instance AddableOp (Array Int (Complex Double)) where
    (+) arr1 arr2 =
        listArray
            (0, size - 1)
            [x + y | i <- [0 .. size - 1], let x = arr1 ! i, let y = arr2 ! i]
      where
        size = length . elems $ arr1
    negate arr =
        listArray (0, size - 1) [-x | i <- [0 .. size - 1], let x = arr ! i]
      where
        size = length . elems $ arr

-- | MARK: (*)
--
instance {-# OVERLAPPABLE #-} Num a => MultiplyOp a a a where
    (*) = times

instance MultiplyOp (Array Int Double) (Array Int Double) (Array Int Double) where
    (*) arr1 arr2 =
        listArray
            (0, size - 1)
            [x * y | i <- [0 .. size - 1], let x = arr1 ! i, let y = arr2 ! i]
      where
        size = length . elems $ arr1

instance MultiplyOp (Array Int (Complex Double)) (Array Int (Complex Double)) (Array Int (Complex Double)) where
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

--
--{--
--    =========================
--    ==  Rotate Operations  ==
--    =========================
---}
--
---- | Find out if it is going to be a righ or left shift and calculate the shift amount
--shiftAmount::
--    Int -- ^ Input : Position to be changes
--    -> Int -- ^ Input : Shift amount
--    -> Int -- ^ Input : First Element of Range
--    -> Int -- ^ Input : Last Element of Range
--    -> Int -- ^ Output : Calculated shift amount
--shiftAmount po shift first last
--  | shift == 0 = po -- There is not shift. Just return the position itself
--  | shift > 0 = rightShiftCalc po shift first last -- It is a right shift. Go and invoke the right shift function
--  | otherwise = leftShiftCalc po shift first last -- It is a left shift. Go and invoke the left shift function
--
--
---- | Calculate the right shift amount and pass it to shiftAmount function
--rightShiftCalc ::
--  Int -- ^ Input : Position to be changes
--  -> Int -- ^ Input : Shift amount
--  -> Int -- ^ Input : First Element of Range
--  -> Int -- ^ Input : Last Element of Range
--  -> Int -- ^ Output : Calculated shift amount
--rightShiftCalc po shift first last
--  | modShift == 0 = po -- If no shift happen, return the position itself
--  | otherwise = realShift `mod` rangeLength -- Calculate the real amount of shift based on the first index position
--  where rangeLength = (length . range) (first,last)   -- calculating the number or elements in index list
--        modShift= shift `mod` rangeLength -- calculating the shift amount based on mod function
--        realShift = po + modShift -- calculating the real shift amount after mod shift calculation
--
---- | Calculate the left shift amount and pass it to shiftAmount function
--leftShiftCalc ::
--  Int -- ^ Input : Position to be changes
--  -> Int -- ^ Input : Shift amount
--  -> Int -- ^ Input : First Element of Range
--  -> Int -- ^ Input : Last Element of Range
--  -> Int -- ^ Output : Calculated shift amount
--leftShiftCalc po shift first last
--  = rightShiftCalc po eqRightShift first last
--  where eqRightShift= rangeLength + shift
--        rangeLength = (length . range) (first,last)
--
--
--
--
---- | The 'oneDArrayRotateGenerator' Generates an array based on the input arguments
--oneDArrayRotateGenerator ::
--  Int -- ^ Range first element
--  -> Int -- ^ Range last element
--  -> Int -- ^ Shift Amount
--  -> (Array Int Double) -- ^ Target Array
--  -> (Array Int Double) -- ^ Result Array
--oneDArrayRotateGenerator n  m rotateAmountX xs =
--  array (n,m) ([((shiftAmount i rotateAmountX n m),xs!i)| i <- [n..m]])
--
---- | One dimension Array rotation
--oneDRotation ::
--  Int -- ^ Rotation Amount
--  -> (Array Int Double)  -- ^ Input Array
--  -> (Array Int Double)  -- ^ Output Array
--oneDRotation n xs =
-- oneDArrayRotateGenerator ((fst . bounds) xs) ((snd . bounds) xs) n xs
--
---- | The 'twoDArrayRotateGenerator' Generates an array based on the input arguments
--twoDArrayRotateGenerator ::
--  (Int,Int) -- ^ Range first Element
--  -> (Int,Int) -- ^ Range last element
--  -> (Int,Int) -- ^ Rotate Amount in each direction
--  -> (Array (Int,Int) Double) -- ^ Input Array
--  -> (Array (Int,Int) Double) -- ^ Output Array
--twoDArrayRotateGenerator (x1,y1) (x2,y2) (rotateAmountX,rotateAmountY) xs =
--  array ((x1,y1),(x2,y2)) ([(((shiftAmount i rotateAmountX x1 x2),(shiftAmount j rotateAmountY y1 y2)),xs!(i,j))| (i,j) <- range ((x1,y1),(x2,y2))])
--
--  -- | Two dimension Array rotation
--twoRotation ::
--  (Int,Int) -- ^ Rotation Amount
--  -> (Array (Int,Int) Double)  -- ^ Input Array
--  -> (Array (Int,Int) Double)  -- ^ Output Array
--twoRotation (x,y) xs =
-- twoDArrayRotateGenerator ((fst . bounds) xs) ((snd . bounds) xs) (x,y) xs
--
--
---- | The 'threeDArrayRotateGenerator' Generates an array based on the input with 3d indexing arguments
--threeDArrayRotateGenerator ::
--  (Int,Int,Int) -- ^ Range first Element
--  -> (Int,Int,Int) -- ^ Range last element
--  -> (Int,Int,Int) -- ^ Rotate Amount in each direction
--  -> (Array (Int,Int,Int) Double) -- ^ Input Array
--  -> (Array (Int,Int,Int) Double) -- ^ Output Array
--threeDArrayRotateGenerator (x1,y1,z1) (x2,y2,z2) (rotateAmountX,rotateAmountY,rotateAmountZ) xs =
--  array ((x1,y1,z1),(x2,y2,z2)) ([(((shiftAmount i rotateAmountX x1 x2),(shiftAmount j rotateAmountY y1 y2),(shiftAmount z rotateAmountZ z1 z2)),xs!(i,j,z))| (i,j,z) <- range ((x1,y1,z1),(x2,y2,z2))])
--
--  -- | three dimension Array rotation
--threeRotation ::
--  (Int,Int,Int) -- ^ Rotation Amount
--  -> (Array (Int,Int,Int) Double)  -- ^ Input Array
--  -> (Array (Int,Int,Int) Double)  -- ^ Output Array
--threeRotation (x,y,z) xs =
--  threeDArrayRotateGenerator ((fst . bounds) xs) ((snd . bounds) xs) (x,y,z) xs
--
--
---- | Rotation of a Array
---- Propertiy : Do the rotation based on the rotation amount. If the length of the list is 1 then it is a one dimension
---- rotation, if it is two, then this is a two dimension rotation. If it is three, then it is a three dimension rotation.
--rotationOpt1D ::
--  [Int] -- ^ Input : Rotation Amount as List. Length must be 1
--  -> Array Int Double  -- ^ Input : Input one dimension array
--  -> Array Int Double  -- ^ Output : Output Array (The dimension should match the input array)
--rotationOpt1D rotationAmount xs = oneDRotation (head rotationAmount) xs
--
---- | Rotation of a Array
---- Propertiy : Do the rotation based on the rotation amount. If the length of the list is 1 then it is a one dimension
---- rotation, if it is two, then this is a two dimension rotation. If it is three, then it is a three dimension rotation.
--rotationOpt2D ::
--  [Int] -- ^ Input : Rotation Amount as List. Length must be 2
--  -> Array (Int,Int) Double  -- ^ Input : Input two dimension array
--  -> Array (Int,Int) Double  -- ^ Output : Output Array (The dimension should match the input array)
--rotationOpt2D rotationAmount xs = twoRotation (head rotationAmount,rotationAmount !! 1) xs
--
---- | Rotation of a Array
---- Propertiy : Do the rotation based on the rotation amount. If the length of the list is 1 then it is a one dimension
---- rotation, if it is two, then this is a two dimension rotation. If it is three, then it is a three dimension rotation.
--rotationOpt3D ::
--  [Int] -- ^ Input : Rotation Amount as List. Length must be 3
--  -> Array (Int,Int,Int) Double  -- ^ Input : Input three dimenstion Array
--  -> Array (Int,Int,Int) Double  -- ^ Output : Output Array (The dimension should match the input array)
--rotationOpt3D rotationAmount xs = threeRotation (head rotationAmount,rotationAmount !! 1,rotationAmount !! 2) xs
--
--
--
