{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module HashedInterp where

import Data.Array
import Data.Complex
import qualified Data.IntMap.Strict as IM
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Debug.Trace (traceId, traceShowId)
import HashedExpression
    ( C
    , ET(..)
    , Expression(..)
    , ExpressionMap
    , Node(..)
    , One
    , R
    , Three
    , Two
    , Zero
    )
import HashedNode
import HashedPrettify (prettify, showExp)
import HashedUtils

-- | This operation emulates the mathematical operation
{--
    =========================
    ==  Rotate Operations  ==
    =========================
-}

-- | Calculate the real value of shift base on the range and index of the to be changes array
shiftCalc ::
  Int -- ^ Input : Position to be changes
  -> Int -- ^ Input : Shift amount
  -> Int -- ^ Input : First Element of Range
  -> Int -- ^ Input : Last Element of Range
  -> Int -- ^ Output : Calculated shift amount
shiftCalc po shift first last
  | ((po + shift) <= last) = (+) po shift
  | otherwise = (first -1) + ((po + shift) - last)

-- | The 'oneDArrayRotateGenerator' Generates an array based on the input arguments
oneDArrayRotateGenerator ::
  Int -- ^ Range first element
  -> Int -- ^ Range last element
  -> Int -- ^ Shift Amount
  -> (Array Int Double) -- ^ Target Array
  -> (Array Int Double) -- ^ Result Array
oneDArrayRotateGenerator n  m rotateAmountX xs =
  array (n,m) ([((shiftCalc i rotateAmountX n m),xs!i)| i <- [n..m]])

-- | One dimension Array rotation
oneDRotation ::
  Int -- ^ Rotation Amount
  -> (Array Int Double)  -- ^ Input Array
  -> (Array Int Double)  -- ^ Output Array
oneDRotation n xs =
 oneDArrayRotateGenerator ((fst . bounds) xs) ((snd . bounds) xs) n xs

-- | The 'twoDArrayRotateGenerator' Generates an array based on the input arguments
twoDArrayRotateGenerator ::
  (Int,Int) -- ^ Range first Element
  -> (Int,Int) -- ^ Range last element
  -> (Int,Int) -- ^ Rotate Amount in each direction
  -> (Array (Int,Int) Double) -- ^ Input Array
  -> (Array (Int,Int) Double) -- ^ Output Array
twoDArrayRotateGenerator (x1,y1) (x2,y2) (rotateAmountX,rotateAmountY) xs =
  array ((x1,y1),(x2,y2)) ([(((shiftCalc i rotateAmountX x1 x2),(shiftCalc j rotateAmountY y1 y2)),xs!(i,j))| (i,j) <- range ((x1,y1),(x2,y2))])

  -- | Two dimension Array rotation
twoRotation ::
  (Int,Int) -- ^ Rotation Amount
  -> (Array (Int,Int) Double)  -- ^ Input Array
  -> (Array (Int,Int) Double)  -- ^ Output Array
twoRotation (x,y) xs =
 twoDArrayRotateGenerator ((fst . bounds) xs) ((snd . bounds) xs) (x,y) xs


-- | The 'threeDArrayRotateGenerator' Generates an array based on the input with 3d indexing arguments
threeDArrayRotateGenerator ::
  (Int,Int,Int) -- ^ Range first Element
  -> (Int,Int,Int) -- ^ Range last element
  -> (Int,Int,Int) -- ^ Rotate Amount in each direction
  -> (Array (Int,Int,Int) Double) -- ^ Input Array
  -> (Array (Int,Int,Int) Double) -- ^ Output Array
threeDArrayRotateGenerator (x1,y1,z1) (x2,y2,z2) (rotateAmountX,rotateAmountY,rotateAmountZ) xs =
  array ((x1,y1,z1),(x2,y2,z2)) ([(((shiftCalc i rotateAmountX x1 x2),(shiftCalc j rotateAmountY y1 y2),(shiftCalc z rotateAmountZ z1 z2)),xs!(i,j,z))| (i,j,z) <- range ((x1,y1,z1),(x2,y2,z2))])

  -- | three dimension Array rotation
threeRotation ::
  (Int,Int,Int) -- ^ Rotation Amount
  -> (Array (Int,Int,Int) Double)  -- ^ Input Array
  -> (Array (Int,Int,Int) Double)  -- ^ Output Array
threeRotation (x,y,z) xs =
  threeDArrayRotateGenerator ((fst . bounds) xs) ((snd . bounds) xs) (x,y,z) xs


-- | Rotation of a Array
-- Propertiy : Do the rotation based on the rotation amount. If the length of the list is 1 then it is a one dimension
-- rotation, if it is two, then this is a two dimension rotation. If it is three, then it is a three dimension rotation.
rotationOpt1D ::
  [Int] -- ^ Input : Rotation Amount as List. Length must be 1
  -> Array Int Double  -- ^ Input : Input one dimension array
  -> Array Int Double  -- ^ Output : Output Array (The dimension should match the input array)
rotationOpt1D rotationAmount xs = oneDRotation (head rotationAmount) xs

-- | Rotation of a Array
-- Propertiy : Do the rotation based on the rotation amount. If the length of the list is 1 then it is a one dimension
-- rotation, if it is two, then this is a two dimension rotation. If it is three, then it is a three dimension rotation.
rotationOpt2D ::
  [Int] -- ^ Input : Rotation Amount as List. Length must be 2
  -> Array (Int,Int) Double  -- ^ Input : Input two dimension array
  -> Array (Int,Int) Double  -- ^ Output : Output Array (The dimension should match the input array)
rotationOpt2D rotationAmount xs = twoRotation (head rotationAmount,rotationAmount !! 1) xs

-- | Rotation of a Array
-- Propertiy : Do the rotation based on the rotation amount. If the length of the list is 1 then it is a one dimension
-- rotation, if it is two, then this is a two dimension rotation. If it is three, then it is a three dimension rotation.
rotationOpt3D ::
  [Int] -- ^ Input : Rotation Amount as List. Length must be 3
  -> Array (Int,Int,Int) Double  -- ^ Input : Input three dimenstion Array
  -> Array (Int,Int,Int) Double  -- ^ Output : Output Array (The dimension should match the input array)
rotationOpt3D rotationAmount xs = threeRotation (head rotationAmount,rotationAmount !! 1,rotationAmount !! 2) xs




-- | Turn expression to the right type
--
expZeroR :: ExpressionMap -> Int -> Expression Zero R
expZeroR = flip Expression

expOneR :: ExpressionMap -> Int -> Expression One R
expOneR = flip Expression

expTwoR :: ExpressionMap -> Int -> Expression Two R
expTwoR = flip Expression

expThreeR :: ExpressionMap -> Int -> Expression Three R
expThreeR = flip Expression

expZeroC :: ExpressionMap -> Int -> Expression Zero C
expZeroC = flip Expression

expOneC :: ExpressionMap -> Int -> Expression One C
expOneC = flip Expression

expTwoC :: ExpressionMap -> Int -> Expression Two C
expTwoC = flip Expression

expThreeC :: ExpressionMap -> Int -> Expression Three C
expThreeC = flip Expression

-- | Choose branch base on condition value
--
chooseBranch :: [Double] -> Double -> [a] -> a
chooseBranch marks val branches
    | val < head marks = head branches
    | otherwise =
        snd . last . filter ((val >=) . fst) $ zip marks (tail branches)

-- | Approximable class
--
class Approximable a where
    (~=) :: a -> a -> Bool

infix 4 ~=

-- |
--
relativeError :: Double -> Double -> Double
relativeError a b = abs (a - b) / max (abs a) (abs b)

instance Approximable Double where
    (~=) :: Double -> Double -> Bool
    a ~= b
        | abs (a - b) < 1.0e-5 = True
        | a == b = True
        | otherwise = relativeError a b < 0.01

instance Approximable (Complex Double) where
    (~=) :: Complex Double -> Complex Double -> Bool
    a ~= b = (realPart a ~= realPart b) && (imagPart a ~= imagPart b)

instance Approximable (Array Int Double) where
    (~=) :: Array Int Double -> Array Int Double -> Bool
    a ~= b = (indices a == indices b) && and (zipWith (~=) (elems a) (elems b))

instance Approximable (Array Int (Complex Double)) where
    (~=) :: Array Int (Complex Double) -> Array Int (Complex Double) -> Bool
    a ~= b = (indices a == indices b) && and (zipWith (~=) (elems a) (elems b))

instance Approximable (Array (Int, Int) Double) where
    (~=) :: Array (Int, Int) Double -> Array (Int, Int) Double -> Bool
    a ~= b = (indices a == indices b) && and (zipWith (~=) (elems a) (elems b))

instance Approximable (Array (Int, Int) (Complex Double)) where
    (~=) ::
           Array (Int, Int) (Complex Double)
        -> Array (Int, Int) (Complex Double)
        -> Bool
    a ~= b = (indices a == indices b) && and (zipWith (~=) (elems a) (elems b))

instance Approximable (Array (Int, Int, Int) Double) where
    (~=) :: Array (Int, Int, Int) Double -> Array (Int, Int, Int) Double -> Bool
    a ~= b = (indices a == indices b) && and (zipWith (~=) (elems a) (elems b))

instance Approximable (Array (Int, Int, Int) (Complex Double)) where
    (~=) ::
           Array (Int, Int, Int) (Complex Double)
        -> Array (Int, Int, Int) (Complex Double)
        -> Bool
    a ~= b = (indices a == indices b) && and (zipWith (~=) (elems a) (elems b))

-- |
--
class Evaluable d rc output | d rc -> output where
    eval :: ValMaps -> Expression d rc -> output

-- |
--
instance Evaluable Zero R Double where
    eval :: ValMaps -> Expression Zero R -> Double
    eval valMap e@(Expression n mp)
        | [] <- retrieveShape n mp =
            case retrieveNode n mp of
                Var name ->
                    case Map.lookup name $ vm0 valMap of
                        Just val -> val
                        _ -> error "no value associated with the variable"
                Const val -> val
                Sum R args -> sum . map (eval valMap . expZeroR mp) $ args
                Mul R args -> product . map (eval valMap . expZeroR mp) $ args
                Neg R arg -> -(eval valMap $ expZeroR mp arg)
                Scale R arg1 arg2 ->
                    eval valMap (expZeroR mp arg1) *
                    eval valMap (expZeroR mp arg2)
                Power x arg -> eval valMap (expZeroR mp arg) ^ x
                Div arg1 arg2 ->
                    eval valMap (expZeroR mp arg1) /
                    eval valMap (expZeroR mp arg2)
                Sqrt arg -> sqrt (eval valMap (expZeroR mp arg))
                Sin arg -> sin (eval valMap (expZeroR mp arg))
                Cos arg -> cos (eval valMap (expZeroR mp arg))
                Tan arg -> tan (eval valMap (expZeroR mp arg))
                Exp arg -> exp (eval valMap (expZeroR mp arg))
                Log arg -> log (eval valMap (expZeroR mp arg))
                Sinh arg -> sinh (eval valMap (expZeroR mp arg))
                Cosh arg -> cosh (eval valMap (expZeroR mp arg))
                Tanh arg -> tanh (eval valMap (expZeroR mp arg))
                Asin arg -> asin (eval valMap (expZeroR mp arg))
                Acos arg -> acos (eval valMap (expZeroR mp arg))
                Atan arg -> atan (eval valMap (expZeroR mp arg))
                Asinh arg -> asinh (eval valMap (expZeroR mp arg))
                Acosh arg -> acosh (eval valMap (expZeroR mp arg))
                Atanh arg -> atanh (eval valMap (expZeroR mp arg))
                RealPart arg -> realPart (eval valMap (expZeroC mp arg))
                ImagPart arg -> imagPart (eval valMap (expZeroC mp arg))
                InnerProd R arg1 arg2 ->
                    case retrieveShape arg1 mp of
                        [] ->
                            eval valMap (expZeroR mp arg1) *
                            eval valMap (expZeroR mp arg2)
                        [size] ->
                            let res1 = eval valMap $ expOneR mp arg1
                                res2 = eval valMap $ expOneR mp arg2
                             in sum [ x * y
                                    | i <- [0 .. size - 1]
                                    , let x = res1 ! i
                                    , let y = res2 ! i
                                    ]
                        [size1, size2] ->
                            let res1 = eval valMap $ expTwoR mp arg1
                                res2 = eval valMap $ expTwoR mp arg2
                             in sum [ x * y
                                    | i <- [0 .. size1 - 1]
                                    , j <- [0 .. size2 - 1]
                                    , let x = res1 ! (i, j)
                                    , let y = res2 ! (i, j)
                                    ]
                        [size1, size2, size3] ->
                            let res1 = eval valMap $ expThreeR mp arg1
                                res2 = eval valMap $ expThreeR mp arg2
                             in sum [ x * y
                                    | i <- [0 .. size1 - 1]
                                    , j <- [0 .. size2 - 1]
                                    , k <- [0 .. size3 - 1]
                                    , let x = res1 ! (i, j, k)
                                    , let y = res2 ! (i, j, k)
                                    ]
                        _ -> error "4D shape?"
                Piecewise marks conditionArg branchArgs ->
                    let cdt = eval valMap $ expZeroR mp conditionArg
                        branches = map (eval valMap . expZeroR mp) branchArgs
                     in chooseBranch marks cdt branches
                _ ->
                    error
                        ("expression structure Scalar R is wrong " ++ prettify e)
        | otherwise = error "one r but shape is not [] ??"

instance Evaluable Zero C (Complex Double) where
    eval :: ValMaps -> Expression Zero C -> Complex Double
    eval valMap e@(Expression n mp)
        | [] <- retrieveShape n mp =
            case retrieveNode n mp of
                Sum C args -> sum . map (eval valMap . expZeroC mp) $ args
                Mul C args -> product . map (eval valMap . expZeroC mp) $ args
                Power x arg -> eval valMap (expZeroC mp arg) ^ x
                Neg C arg -> -(eval valMap $ expZeroC mp arg)
                Scale C arg1 arg2 ->
                    case retrieveElementType arg1 mp of
                        R ->
                            fromR (eval valMap (expZeroR mp arg1)) *
                            eval valMap (expZeroC mp arg2)
                        C ->
                            eval valMap (expZeroC mp arg1) *
                            eval valMap (expZeroC mp arg2)
                RealImag arg1 arg2 ->
                    eval valMap (expZeroR mp arg1) :+
                    eval valMap (expZeroR mp arg2)
                InnerProd C arg1 arg2 ->
                    case retrieveShape arg1 mp of
                        [] ->
                            eval valMap (expZeroC mp arg1) *
                            eval valMap (expZeroC mp arg2)
                        [size] ->
                            let res1 = eval valMap $ expOneC mp arg1
                                res2 = eval valMap $ expOneC mp arg2
                             in sum [ x * y
                                    | i <- [0 .. size - 1]
                                    , let x = res1 ! i
                                    , let y = res2 ! i
                                    ]
                        [size1, size2] ->
                            let res1 = eval valMap $ expTwoC mp arg1
                                res2 = eval valMap $ expTwoC mp arg2
                             in sum [ x * y
                                    | i <- [0 .. size1 - 1]
                                    , j <- [0 .. size2 - 1]
                                    , let x = res1 ! (i, j)
                                    , let y = res2 ! (i, j)
                                    ]
                        [size1, size2, size3] ->
                            let res1 = eval valMap $ expThreeC mp arg1
                                res2 = eval valMap $ expThreeC mp arg2
                             in sum [ x * y
                                    | i <- [0 .. size1 - 1]
                                    , j <- [0 .. size2 - 1]
                                    , k <- [0 .. size3 - 1]
                                    , let x = res1 ! (i, j, k)
                                    , let y = res2 ! (i, j, k)
                                    ]
                        _ -> error "4D shape?"
                Piecewise marks conditionArg branchArgs ->
                    let cdt = eval valMap $ expZeroR mp conditionArg
                        branches = map (eval valMap . expZeroC mp) branchArgs
                     in chooseBranch marks cdt branches
                _ ->
                    error
                        ("expression structure Scalar C is wrong " ++ prettify e)
        | otherwise = error "One C but shape is not [] ??"

-- |
--
instance Evaluable One R (Array Int Double) where
    eval :: ValMaps -> Expression One R -> Array Int Double
    eval valMap e@(Expression n mp)
        | [size] <- retrieveShape n mp =
            let fmap :: (a -> c) -> Array Int a -> Array Int c
                fmap f arr =
                    listArray
                        (0, size - 1)
                        [f x | i <- [0 .. size - 1], let x = arr ! i]
                zipWith ::
                       (a -> b -> c)
                    -> Array Int a
                    -> Array Int b
                    -> Array Int c
                zipWith f arr1 arr2 =
                    listArray
                        (0, size - 1)
                        [ f x y
                        | i <- [0 .. size - 1]
                        , let x = arr1 ! i
                        , let y = arr2 ! i
                        ]
                foldl1' :: (a -> a -> a) -> [Array Int a] -> Array Int a
                foldl1' f [x] = x
                foldl1' f (x:xs) = zipWith f x (foldl1' f xs)
             in case retrieveNode n mp of
                    Var name ->
                        case Map.lookup name $ vm1 valMap of
                            Just val -> val
                            _ -> error "no value associated with the variable"
                    Const val -> listArray (0, size - 1) $ replicate size val
                    Sum R args ->
                        foldl1' (+) . map (eval valMap . expOneR mp) $ args
                    Mul R args ->
                        foldl1' (+) . map (eval valMap . expOneR mp) $ args
                    Power x arg -> fmap (^ x) (eval valMap $ expOneR mp arg)
                    Neg R arg -> fmap negate . eval valMap $ expOneR mp arg
                    Scale R arg1 arg2 ->
                        let scalar = eval valMap $ expZeroR mp arg1
                         in fmap (scalar *) . eval valMap $ expOneR mp arg2
                    Div arg1 arg2 ->
                        zipWith
                            (/)
                            (eval valMap $ expOneR mp arg2)
                            (eval valMap $ expOneR mp arg2)
                    Sqrt arg -> fmap sqrt . eval valMap $ expOneR mp arg
                    Sin arg -> fmap sin . eval valMap $ expOneR mp arg
                    Cos arg -> fmap cos . eval valMap $ expOneR mp arg
                    Tan arg -> fmap tan . eval valMap $ expOneR mp arg
                    Exp arg -> fmap exp . eval valMap $ expOneR mp arg
                    Log arg -> fmap log . eval valMap $ expOneR mp arg
                    Sinh arg -> fmap sinh . eval valMap $ expOneR mp arg
                    Cosh arg -> fmap cosh . eval valMap $ expOneR mp arg
                    Tanh arg -> fmap tanh . eval valMap $ expOneR mp arg
                    Asin arg -> fmap asin . eval valMap $ expOneR mp arg
                    Acos arg -> fmap acos . eval valMap $ expOneR mp arg
                    Atan arg -> fmap atan . eval valMap $ expOneR mp arg
                    Asinh arg -> fmap asinh . eval valMap $ expOneR mp arg
                    Acosh arg -> fmap acosh . eval valMap $ expOneR mp arg
                    Atanh arg -> fmap atanh . eval valMap $ expOneR mp arg
                    RealPart arg -> fmap realPart . eval valMap $ expOneC mp arg
                    ImagPart arg -> fmap imagPart . eval valMap $ expOneC mp arg
                    -- Rotate rA arg ->
                    Piecewise marks conditionArg branchArgs ->
                        let cdt = eval valMap $ expOneR mp conditionArg
                            branches = map (eval valMap . expOneR mp) branchArgs
                         in listArray
                                (0, size - 1)
                                [ chosen ! i
                                | i <- [0 .. size - 1]
                                , let chosen =
                                          chooseBranch marks (cdt ! i) branches
                                ]
                    Rotate [x] arg -> rotationOpt1D [x] (eval valMap $ expOneR mp arg)
                    _ -> error "expression structure One R is wrong"
        | otherwise = error "one r but shape is not [size] ??"

--                         in chooseBranch marks cdt branches
-- |
--


instance Evaluable One C (Array Int (Complex Double)) where
    eval :: ValMaps -> Expression One C -> Array Int (Complex Double)
    eval valMap e@(Expression n mp)
        | [size] <- retrieveShape n mp =
            let fmap :: (a -> b) -> Array Int a -> Array Int b
                fmap f arr =
                    listArray
                        (0, size - 1)
                        [f x | i <- [0 .. size - 1], let x = arr ! i]
                zipWith ::
                       (a -> b -> c)
                    -> Array Int a
                    -> Array Int b
                    -> Array Int c
                zipWith f arr1 arr2 =
                    listArray
                        (0, size - 1)
                        [ f x y
                        | i <- [0 .. size - 1]
                        , let x = arr1 ! i
                        , let y = arr2 ! i
                        ]
                foldl1' :: (a -> a -> a) -> [Array Int a] -> Array Int a
                foldl1' f [x] = x
                foldl1' f (x:xs) = zipWith f x (foldl1' f xs)
             in case retrieveNode n mp of
                    Sum C args ->
                        foldl1' (+) . map (eval valMap . expOneC mp) $ args
                    Mul C args ->
                        foldl1' (+) . map (eval valMap . expOneC mp) $ args
                    Power x arg -> fmap (^ x) (eval valMap $ expOneC mp arg)
                    Neg C arg -> fmap negate . eval valMap $ expOneC mp arg
                    Scale C arg1 arg2 ->
                        case retrieveElementType arg1 mp of
                            R ->
                                let scalar =
                                        fromR . eval valMap $ expZeroR mp arg1
                                 in fmap (scalar *) . eval valMap $
                                    expOneC mp arg2
                            C ->
                                let scalar = eval valMap $ expZeroC mp arg1
                                 in fmap (scalar *) . eval valMap $
                                    expOneC mp arg2
                    RealImag arg1 arg2 ->
                        zipWith
                            (:+)
                            (eval valMap $ expOneR mp arg1)
                            (eval valMap $ expOneR mp arg2)
                    Piecewise marks conditionArg branchArgs ->
                        let cdt = eval valMap $ expOneR mp conditionArg
                            branches = map (eval valMap . expOneC mp) branchArgs
                         in listArray
                                (0, size - 1)
                                [ chosen ! i
                                | i <- [0 .. size - 1]
                                , let chosen =
                                          chooseBranch marks (cdt ! i) branches
                                ]
                    _ -> error "expression structure One C is wrong"
        | otherwise = error "one C but shape is not [size] ??"

instance Evaluable Two R (Array (Int, Int) Double) where
    eval :: ValMaps -> Expression Two R -> Array (Int, Int) Double
    eval valMap e@(Expression n mp)
        | [size1, size2] <- retrieveShape n mp =
            let fmap :: (a -> c) -> Array (Int, Int) a -> Array (Int, Int) c
                fmap f arr =
                    listArray
                        ((0, 0), (size1 - 1, size2 - 1))
                        [ f x
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        , let x = arr ! (i, j)
                        ]
                zipWith ::
                       (a -> b -> c)
                    -> Array (Int, Int) a
                    -> Array (Int, Int) b
                    -> Array (Int, Int) c
                zipWith f arr1 arr2 =
                    listArray
                        ((0, 0), (size1 - 1, size2 - 1))
                        [ f x y
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        , let x = arr1 ! (i, j)
                        , let y = arr2 ! (i, j)
                        ]
                foldl1' ::
                       (a -> a -> a)
                    -> [Array (Int, Int) a]
                    -> Array (Int, Int) a
                foldl1' f [x] = x
                foldl1' f (x:xs) = zipWith f x (foldl1' f xs)
             in case retrieveNode n mp of
                    Var name ->
                        case Map.lookup name $ vm2 valMap of
                            Just val -> val
                            _ -> error "no value associated with the variable"
                    Const val ->
                        listArray ((0, 0), (size1 - 1, size2 - 1)) $
                        replicate (size1 * size2) val
                    Sum R args ->
                        foldl1' (+) . map (eval valMap . expTwoR mp) $ args
                    Mul R args ->
                        foldl1' (+) . map (eval valMap . expTwoR mp) $ args
                    Power x arg -> fmap (^ x) (eval valMap $ expTwoR mp arg)
                    Neg R arg -> fmap negate . eval valMap $ expTwoR mp arg
                    Scale R arg1 arg2 ->
                        let scalar = eval valMap $ expZeroR mp arg1
                         in fmap (scalar *) . eval valMap $ expTwoR mp arg2
                    Div arg1 arg2 ->
                        zipWith
                            (/)
                            (eval valMap $ expTwoR mp arg2)
                            (eval valMap $ expTwoR mp arg2)
                    Sqrt arg -> fmap sqrt . eval valMap $ expTwoR mp arg
                    Sin arg -> fmap sin . eval valMap $ expTwoR mp arg
                    Cos arg -> fmap cos . eval valMap $ expTwoR mp arg
                    Tan arg -> fmap tan . eval valMap $ expTwoR mp arg
                    Exp arg -> fmap exp . eval valMap $ expTwoR mp arg
                    Log arg -> fmap log . eval valMap $ expTwoR mp arg
                    Sinh arg -> fmap sinh . eval valMap $ expTwoR mp arg
                    Cosh arg -> fmap cosh . eval valMap $ expTwoR mp arg
                    Tanh arg -> fmap tanh . eval valMap $ expTwoR mp arg
                    Asin arg -> fmap asin . eval valMap $ expTwoR mp arg
                    Acos arg -> fmap acos . eval valMap $ expTwoR mp arg
                    Atan arg -> fmap atan . eval valMap $ expTwoR mp arg
                    Asinh arg -> fmap asinh . eval valMap $ expTwoR mp arg
                    Acosh arg -> fmap acosh . eval valMap $ expTwoR mp arg
                    Atanh arg -> fmap atanh . eval valMap $ expTwoR mp arg
                    RealPart arg -> fmap realPart . eval valMap $ expTwoC mp arg
                    ImagPart arg -> fmap imagPart . eval valMap $ expTwoC mp arg
                    Piecewise marks conditionArg branchArgs ->
                        let cdt = eval valMap $ expTwoR mp conditionArg
                            branches = map (eval valMap . expTwoR mp) branchArgs
                         in listArray
                                ((0, 0), (size1 - 1, size2 - 1))
                                [ chosen ! (i, j)
                                | i <- [0 .. size1 - 1]
                                , j <- [0 .. size2 - 1]
                                , let chosen =
                                          chooseBranch
                                              marks
                                              (cdt ! (i, j))
                                              branches
                                ]
                    Rotate [x] arg -> rotationOpt2D [x] (eval valMap $ expTwoR mp arg)
                    _ -> error "expression structure Two R is wrong"
        | otherwise = error "Two r but shape is not [size1, size2] ??"

instance Evaluable Two C (Array (Int, Int) (Complex Double)) where
    eval :: ValMaps -> Expression Two C -> Array (Int, Int) (Complex Double)
    eval valMap e@(Expression n mp)
        | [size1, size2] <- retrieveShape n mp =
            let fmap :: (a -> c) -> Array (Int, Int) a -> Array (Int, Int) c
                fmap f arr =
                    listArray
                        ((0, 0), (size1 - 1, size2 - 1))
                        [ f x
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        , let x = arr ! (i, j)
                        ]
                zipWith ::
                       (a -> b -> c)
                    -> Array (Int, Int) a
                    -> Array (Int, Int) b
                    -> Array (Int, Int) c
                zipWith f arr1 arr2 =
                    listArray
                        ((0, 0), (size1 - 1, size2 - 1))
                        [ f x y
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        , let x = arr1 ! (i, j)
                        , let y = arr2 ! (i, j)
                        ]
                foldl1' ::
                       (a -> a -> a)
                    -> [Array (Int, Int) a]
                    -> Array (Int, Int) a
                foldl1' f [x] = x
                foldl1' f (x:xs) = zipWith f x (foldl1' f xs)
             in case retrieveNode n mp of
                    Sum C args ->
                        foldl1' (+) . map (eval valMap . expTwoC mp) $ args
                    Mul C args ->
                        foldl1' (+) . map (eval valMap . expTwoC mp) $ args
                    Power x arg -> fmap (^ x) (eval valMap $ expTwoC mp arg)
                    Neg C arg -> fmap negate . eval valMap $ expTwoC mp arg
                    Scale C arg1 arg2 ->
                        case retrieveElementType arg1 mp of
                            R ->
                                let scalar =
                                        fromR . eval valMap $ expZeroR mp arg1
                                 in fmap (scalar *) . eval valMap $
                                    expTwoC mp arg2
                            C ->
                                let scalar = eval valMap $ expZeroC mp arg1
                                 in fmap (scalar *) . eval valMap $
                                    expTwoC mp arg2
                    RealImag arg1 arg2 ->
                        zipWith
                            (:+)
                            (eval valMap $ expTwoR mp arg1)
                            (eval valMap $ expTwoR mp arg2)
                    Piecewise marks conditionArg branchArgs ->
                        let cdt = eval valMap $ expTwoR mp conditionArg
                            branches = map (eval valMap . expTwoC mp) branchArgs
                         in listArray
                                ((0, 0), (size1 - 1, size2 - 1))
                                [ chosen ! (i, j)
                                | i <- [0 .. size1 - 1]
                                , j <- [0 .. size2 - 1]
                                , let chosen =
                                          chooseBranch
                                              marks
                                              (cdt ! (i, j))
                                              branches
                                ]
                    _ -> error "expression structure Two C is wrong"
        | otherwise = error "Two C but shape is not [size1, size2] ??"

instance Evaluable Three R (Array (Int, Int, Int) Double) where
    eval :: ValMaps -> Expression Three R -> Array (Int, Int, Int) Double
    eval valMap e@(Expression n mp)
        | [size1, size2, size3] <- retrieveShape n mp =
            let fmap ::
                       (a -> c)
                    -> Array (Int, Int, Int) a
                    -> Array (Int, Int, Int) c
                fmap f arr =
                    listArray
                        ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
                        [ f x
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        , k <- [0 .. size3 - 1]
                        , let x = arr ! (i, j, k)
                        ]
                zipWith ::
                       (a -> b -> c)
                    -> Array (Int, Int, Int) a
                    -> Array (Int, Int, Int) b
                    -> Array (Int, Int, Int) c
                zipWith f arr1 arr2 =
                    listArray
                        ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
                        [ f x y
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        , k <- [0 .. size3 - 1]
                        , let x = arr1 ! (i, j, k)
                        , let y = arr2 ! (i, j, k)
                        ]
                foldl1' ::
                       (a -> a -> a)
                    -> [Array (Int, Int, Int) a]
                    -> Array (Int, Int, Int) a
                foldl1' f [x] = x
                foldl1' f (x:xs) = zipWith f x (foldl1' f xs)
             in case retrieveNode n mp of
                    Var name ->
                        case Map.lookup name $ vm3 valMap of
                            Just val -> val
                            _ -> error "no value associated with the variable"
                    Const val ->
                        listArray ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1)) $
                        replicate (size1 * size2 * size3) val
                    Sum R args ->
                        foldl1' (+) . map (eval valMap . expThreeR mp) $ args
                    Mul R args ->
                        foldl1' (+) . map (eval valMap . expThreeR mp) $ args
                    Power x arg -> fmap (^ x) (eval valMap $ expThreeR mp arg)
                    Neg R arg -> fmap negate . eval valMap $ expThreeR mp arg
                    Scale R arg1 arg2 ->
                        let scalar = eval valMap $ expZeroR mp arg1
                         in fmap (scalar *) . eval valMap $ expThreeR mp arg2
                    Div arg1 arg2 ->
                        zipWith
                            (/)
                            (eval valMap $ expThreeR mp arg2)
                            (eval valMap $ expThreeR mp arg2)
                    Sqrt arg -> fmap sqrt . eval valMap $ expThreeR mp arg
                    Sin arg -> fmap sin . eval valMap $ expThreeR mp arg
                    Cos arg -> fmap cos . eval valMap $ expThreeR mp arg
                    Tan arg -> fmap tan . eval valMap $ expThreeR mp arg
                    Exp arg -> fmap exp . eval valMap $ expThreeR mp arg
                    Log arg -> fmap log . eval valMap $ expThreeR mp arg
                    Sinh arg -> fmap sinh . eval valMap $ expThreeR mp arg
                    Cosh arg -> fmap cosh . eval valMap $ expThreeR mp arg
                    Tanh arg -> fmap tanh . eval valMap $ expThreeR mp arg
                    Asin arg -> fmap asin . eval valMap $ expThreeR mp arg
                    Acos arg -> fmap acos . eval valMap $ expThreeR mp arg
                    Atan arg -> fmap atan . eval valMap $ expThreeR mp arg
                    Asinh arg -> fmap asinh . eval valMap $ expThreeR mp arg
                    Acosh arg -> fmap acosh . eval valMap $ expThreeR mp arg
                    Atanh arg -> fmap atanh . eval valMap $ expThreeR mp arg
                    RealPart arg ->
                        fmap realPart . eval valMap $ expThreeC mp arg
                    ImagPart arg ->
                        fmap imagPart . eval valMap $ expThreeC mp arg
                    Piecewise marks conditionArg branchArgs ->
                        let cdt = eval valMap $ expThreeR mp conditionArg
                            branches =
                                map (eval valMap . expThreeR mp) branchArgs
                         in listArray
                                ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
                                [ chosen ! (i, j, k)
                                | i <- [0 .. size1 - 1]
                                , j <- [0 .. size2 - 1]
                                , k <- [0 .. size3 - 1]
                                , let chosen =
                                          chooseBranch
                                              marks
                                              (cdt ! (i, j, k))
                                              branches
                                ]
                    Rotate [x] arg -> rotationOpt3D [x] (eval valMap $ expThreeR mp arg)
                    _ -> error "expression structure Three R is wrong"
        | otherwise = error "Three r but shape is not [size1, size2, size3] ??"

instance Evaluable Three C (Array (Int, Int, Int) (Complex Double)) where
    eval ::
           ValMaps
        -> Expression Three C
        -> Array (Int, Int, Int) (Complex Double)
    eval valMap e@(Expression n mp)
        | [size1, size2, size3] <- retrieveShape n mp =
            let fmap ::
                       (a -> c)
                    -> Array (Int, Int, Int) a
                    -> Array (Int, Int, Int) c
                fmap f arr =
                    listArray
                        ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
                        [ f x
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        , k <- [0 .. size3 - 1]
                        , let x = arr ! (i, j, k)
                        ]
                zipWith ::
                       (a -> b -> c)
                    -> Array (Int, Int, Int) a
                    -> Array (Int, Int, Int) b
                    -> Array (Int, Int, Int) c
                zipWith f arr1 arr2 =
                    listArray
                        ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
                        [ f x y
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        , k <- [0 .. size3 - 1]
                        , let x = arr1 ! (i, j, k)
                        , let y = arr2 ! (i, j, k)
                        ]
                foldl1' ::
                       (a -> a -> a)
                    -> [Array (Int, Int, Int) a]
                    -> Array (Int, Int, Int) a
                foldl1' f [x] = x
                foldl1' f (x:xs) = zipWith f x (foldl1' f xs)
             in case retrieveNode n mp of
                    Sum C args ->
                        foldl1' (+) . map (eval valMap . expThreeC mp) $ args
                    Mul C args ->
                        foldl1' (+) . map (eval valMap . expThreeC mp) $ args
                    Power x arg -> fmap (^ x) (eval valMap $ expThreeC mp arg)
                    Neg C arg -> fmap negate . eval valMap $ expThreeC mp arg
                    Scale C arg1 arg2 ->
                        case retrieveElementType arg1 mp of
                            R ->
                                let scalar =
                                        fromR . eval valMap $ expZeroR mp arg1
                                 in fmap (scalar *) . eval valMap $
                                    expThreeC mp arg2
                            C ->
                                let scalar = eval valMap $ expZeroC mp arg1
                                 in fmap (scalar *) . eval valMap $
                                    expThreeC mp arg2
                    RealImag arg1 arg2 ->
                        zipWith
                            (:+)
                            (eval valMap $ expThreeR mp arg1)
                            (eval valMap $ expThreeR mp arg2)
                    Piecewise marks conditionArg branchArgs ->
                        let cdt = eval valMap $ expThreeR mp conditionArg
                            branches =
                                map (eval valMap . expThreeC mp) branchArgs
                         in listArray
                                ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
                                [ chosen ! (i, j, k)
                                | i <- [0 .. size1 - 1]
                                , j <- [0 .. size2 - 1]
                                , k <- [0 .. size3 - 1]
                                , let chosen =
                                          chooseBranch
                                              marks
                                              (cdt ! (i, j, k))
                                              branches
                                ]
                    _ -> error "expression structure Three C is wrong"
        | otherwise = error "Three C but shape is not [size1, size2, size3] ??"


