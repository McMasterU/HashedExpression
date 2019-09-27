{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module HashedInterp where

import Data.Array
import Data.Complex
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Debug.Trace (traceId, traceShowId)
import GHC.TypeLits (KnownNat)
import HashedExpression
    ( C
    , ET(..)
    , Expression(..)
    , ExpressionMap
    , Node(..)
    , One
    , R
    , Scalar
    , Three
    , Two
    )
import HashedNode
import HashedPrettify (prettify, showExp)
import HashedUtils
import Text.Printf

-- | This operation emulates the mathematical operation
-- | Turn expression to the right type
--
expZeroR :: ExpressionMap -> Int -> Expression Scalar R
expZeroR = flip Expression

expOneR :: ExpressionMap -> Int -> Expression One R
expOneR = flip Expression

expTwoR :: ExpressionMap -> Int -> Expression Two R
expTwoR = flip Expression

expThreeR :: ExpressionMap -> Int -> Expression Three R
expThreeR = flip Expression

expZeroC :: ExpressionMap -> Int -> Expression Scalar C
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
class Show a =>
      Approximable a
    where
    (~=) :: a -> a -> Bool
    prettifyShow :: a -> String

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
    prettifyShow a
        | abs a < 1e-10 = "0"
        | otherwise = printf "%.2f" a

instance Approximable (Complex Double) where
    (~=) :: Complex Double -> Complex Double -> Bool
    a ~= b = (realPart a ~= realPart b) && (imagPart a ~= imagPart b)
    prettifyShow a =
        prettifyShow (realPart a) ++ " + " ++ prettifyShow (imagPart a) ++ "i"

instance Approximable (Array Int Double) where
    (~=) :: Array Int Double -> Array Int Double -> Bool
    a ~= b = (bounds a == bounds b) && and (zipWith (~=) (elems a) (elems b))
    prettifyShow a =
        "[" ++ (intercalate ", " . map prettifyShow . elems $ a) ++ "]"

instance Approximable (Array Int (Complex Double)) where
    (~=) :: Array Int (Complex Double) -> Array Int (Complex Double) -> Bool
    a ~= b = (bounds a == bounds b) && and (zipWith (~=) (elems a) (elems b))
    prettifyShow a =
        "[" ++ (intercalate ", " . map prettifyShow . elems $ a) ++ "]"

instance Approximable (Array (Int, Int) Double) where
    (~=) :: Array (Int, Int) Double -> Array (Int, Int) Double -> Bool
    a ~= b = (bounds a == bounds b) && and (zipWith (~=) (elems a) (elems b))
    prettifyShow a =
        "[" ++ (intercalate ", " . map prettifyShow . elems $ a) ++ "]"

instance Approximable (Array (Int, Int) (Complex Double)) where
    (~=) ::
           Array (Int, Int) (Complex Double)
        -> Array (Int, Int) (Complex Double)
        -> Bool
    a ~= b = (bounds a == bounds b) && and (zipWith (~=) (elems a) (elems b))
    prettifyShow a =
        "[" ++ (intercalate ", " . map prettifyShow . elems $ a) ++ "]"

instance Approximable (Array (Int, Int, Int) Double) where
    (~=) :: Array (Int, Int, Int) Double -> Array (Int, Int, Int) Double -> Bool
    a ~= b = (bounds a == bounds b) && and (zipWith (~=) (elems a) (elems b))
    prettifyShow a =
        "[" ++ (intercalate ", " . map prettifyShow . elems $ a) ++ "]"

instance Approximable (Array (Int, Int, Int) (Complex Double)) where
    (~=) ::
           Array (Int, Int, Int) (Complex Double)
        -> Array (Int, Int, Int) (Complex Double)
        -> Bool
    a ~= b = (bounds a == bounds b) && and (zipWith (~=) (elems a) (elems b))
    prettifyShow a =
        "[" ++ (intercalate ", " . map prettifyShow . elems $ a) ++ "]"

-- | These should be commented properly.
--
class Evaluable d rc output | d rc -> output where
    eval :: ValMaps -> Expression d rc -> output

-- |
--
instance Evaluable Scalar R Double where
    eval :: ValMaps -> Expression Scalar R -> Double
    eval valMap e@(Expression n mp)
        | [] <- retrieveShape n mp =
            case retrieveNode n mp of
                Var name ->
                    case Map.lookup name valMap of
                        Just (VScalar val) -> val
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

instance Evaluable Scalar C (Complex Double) where
    eval :: ValMaps -> Expression Scalar C -> Complex Double
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
                            conjugate (eval valMap (expZeroC mp arg2))
                        [size] ->
                            let res1 = eval valMap $ expOneC mp arg1
                                res2 = eval valMap $ expOneC mp arg2
                             in sum [ x * conjugate y
                                    | i <- [0 .. size - 1]
                                    , let x = res1 ! i
                                    , let y = res2 ! i
                                    ]
                        [size1, size2] ->
                            let res1 = eval valMap $ expTwoC mp arg1
                                res2 = eval valMap $ expTwoC mp arg2
                             in sum [ x * conjugate y
                                    | i <- [0 .. size1 - 1]
                                    , j <- [0 .. size2 - 1]
                                    , let x = res1 ! (i, j)
                                    , let y = res2 ! (i, j)
                                    ]
                        [size1, size2, size3] ->
                            let res1 = eval valMap $ expThreeC mp arg1
                                res2 = eval valMap $ expThreeC mp arg2
                             in sum [ x * conjugate y
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
zipWithA :: Ix x => (a -> b -> c) -> Array x a -> Array x b -> Array x c
zipWithA f xs ys = listArray (bounds xs) $ zipWith f (elems xs) (elems ys)

foldrElementwise :: Ix ix => (a -> a -> a) -> [Array ix a] -> Array ix a
foldrElementwise f [x] = x
foldrElementwise f (x:xs) = zipWithA f x (foldrElementwise f xs)

-- | 
--
evaluate1DReal :: ValMaps -> (ExpressionMap, Int) -> Array Int Double
evaluate1DReal valMap (mp, n)
    | [size] <- retrieveShape n mp =
        case retrieveNode n mp of
            Var name ->
                case Map.lookup name valMap of
                    Just (V1D val) -> val
                    _ -> error "no value associated with the variable"
            Const val -> listArray (0, size - 1) $ replicate size val
            Sum R args ->
                foldrElementwise (+) . map (eval valMap . expOneR mp) $ args
            Mul R args ->
                foldrElementwise (*) . map (eval valMap . expOneR mp) $ args
            Power x arg -> fmap (^ x) (eval valMap $ expOneR mp arg)
            Neg R arg -> fmap negate . eval valMap $ expOneR mp arg
            Scale R arg1 arg2 ->
                let scalar = eval valMap $ expZeroR mp arg1
                 in fmap (scalar *) . eval valMap $ expOneR mp arg2
            Div arg1 arg2 ->
                zipWithA
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
                        , let chosen = chooseBranch marks (cdt ! i) branches
                        ]
            Rotate [amount] arg ->
                rotate1D size amount (eval valMap $ expOneR mp arg)
            TwiceReFT arg ->
                let innerRes = eval valMap $ expOneR mp arg
                    scaleFactor = fromIntegral size / 2
                 in listArray
                        (0, size - 1)
                        [ scaleFactor *
                        (innerRes ! i + innerRes ! ((size - i) `mod` size))
                        | i <- [0 .. size - 1]
                        ]
            TwiceImFT arg ->
                let innerRes = eval valMap $ expOneR mp arg
                    scaleFactor = fromIntegral size / 2
                 in listArray
                        (0, size - 1)
                        [ scaleFactor *
                        (innerRes ! i - innerRes ! ((size - i) `mod` size))
                        | i <- [0 .. size - 1]
                        ]
            ReFT arg ->
                case retrieveElementType arg mp of
                    R ->
                        let inner = fmap (:+ 0) . eval valMap $ expOneR mp arg
                            ftResult = fourierTransform1D size inner
                         in fmap realPart ftResult
                    C ->
                        let inner = eval valMap $ expOneC mp arg
                            ftResult = fourierTransform1D size inner
                         in fmap realPart ftResult
            ImFT arg ->
                case retrieveElementType arg mp of
                    R ->
                        let inner = fmap (:+ 0) . eval valMap $ expOneR mp arg
                            ftResult = fourierTransform1D size inner
                         in fmap imagPart ftResult
                    C ->
                        let inner = eval valMap $ expOneC mp arg
                            ftResult = fourierTransform1D size inner
                         in fmap imagPart ftResult
            _ -> error "expression structure One R is wrong"
    | otherwise = error "one r but shape is not [size] ??"

-- |
--
instance Evaluable One R (Array Int Double) where
    eval :: ValMaps -> Expression One R -> Array Int Double
    eval valMap (Expression n mp) = evaluate1DReal valMap (mp, n)

instance (KnownNat n) => Evaluable n R (Array Int Double) where
    eval :: ValMaps -> Expression n R -> Array Int Double
    eval valMap (Expression n mp) = evaluate1DReal valMap (mp, n)

-- | 
--
evaluate1DComplex ::
       ValMaps -> (ExpressionMap, Int) -> Array Int (Complex Double)
evaluate1DComplex valMap (mp, n)
    | [size] <- retrieveShape n mp =
        case retrieveNode n mp of
            Sum C args ->
                foldrElementwise (+) . map (eval valMap . expOneC mp) $ args
            Mul C args ->
                foldrElementwise (*) . map (eval valMap . expOneC mp) $ args
            Power x arg -> fmap (^ x) (eval valMap $ expOneC mp arg)
            Neg C arg -> fmap negate . eval valMap $ expOneC mp arg
            Scale C arg1 arg2 ->
                case retrieveElementType arg1 mp of
                    R ->
                        let scalar = fromR . eval valMap $ expZeroR mp arg1
                         in fmap (scalar *) . eval valMap $ expOneC mp arg2
                    C ->
                        let scalar = eval valMap $ expZeroC mp arg1
                         in fmap (scalar *) . eval valMap $ expOneC mp arg2
            RealImag arg1 arg2 ->
                zipWithA
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
                        , let chosen = chooseBranch marks (cdt ! i) branches
                        ]
            Rotate [amount] arg ->
                rotate1D size amount (eval valMap $ expOneC mp arg)
            _ -> error "expression structure One C is wrong"
    | otherwise = error "one C but shape is not [size] ??"

--                         in chooseBranch marks cdt branches
-- |
--
instance Evaluable One C (Array Int (Complex Double)) where
    eval :: ValMaps -> Expression One C -> Array Int (Complex Double)
    eval valMap (Expression n mp) = evaluate1DComplex valMap (mp, n)

instance (KnownNat n) => Evaluable n C (Array Int (Complex Double)) where
    eval :: ValMaps -> Expression n C -> Array Int (Complex Double)
    eval valMap (Expression n mp) = evaluate1DComplex valMap (mp, n)

-- | 
--
evaluate2DReal :: ValMaps -> (ExpressionMap, Int) -> Array (Int, Int) Double
evaluate2DReal valMap (mp, n)
    | [size1, size2] <- retrieveShape n mp =
        case retrieveNode n mp of
            Var name ->
                case Map.lookup name valMap of
                    Just (V2D val) -> val
                    _ -> error $ "no value associated with the variable" ++ name
            Const val ->
                listArray ((0, 0), (size1 - 1, size2 - 1)) $
                replicate (size1 * size2) val
            Sum R args ->
                foldrElementwise (+) . map (eval valMap . expTwoR mp) $ args
            Mul R args ->
                foldrElementwise (*) . map (eval valMap . expTwoR mp) $ args
            Power x arg -> fmap (^ x) (eval valMap $ expTwoR mp arg)
            Neg R arg -> fmap negate . eval valMap $ expTwoR mp arg
            Scale R arg1 arg2 ->
                let scalar = eval valMap $ expZeroR mp arg1
                 in fmap (scalar *) . eval valMap $ expTwoR mp arg2
            Div arg1 arg2 ->
                zipWithA
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
                                  chooseBranch marks (cdt ! (i, j)) branches
                        ]
            Rotate [amount1, amount2] arg ->
                rotate2D
                    (size1, size2)
                    (amount1, amount2)
                    (eval valMap $ expTwoR mp arg)
            TwiceReFT arg ->
                let innerRes = eval valMap $ expTwoR mp arg
                    scaleFactor = fromIntegral size1 * fromIntegral size2 / 2
                 in listArray
                        ((0, 0), (size1 - 1, size2 - 1))
                        [ scaleFactor *
                        (innerRes ! (i, j) +
                         innerRes !
                         ((size1 - i) `mod` size1, (size2 - j) `mod` size2))
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        ]
            TwiceImFT arg ->
                let innerRes = eval valMap $ expTwoR mp arg
                    scaleFactor = fromIntegral size1 * fromIntegral size2 / 2
                 in listArray
                        ((0, 0), (size1 - 1, size2 - 1))
                        [ scaleFactor *
                        (innerRes ! (i, j) -
                         innerRes !
                         ((size1 - i) `mod` size1, (size2 - j) `mod` size2))
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        ]
            ReFT arg ->
                case retrieveElementType arg mp of
                    R ->
                        let inner = fmap (:+ 0) . eval valMap $ expTwoR mp arg
                            ftResult = fourierTransform2D (size1, size2) inner
                         in fmap realPart ftResult
                    C ->
                        let inner = eval valMap $ expTwoC mp arg
                            ftResult = fourierTransform2D (size1, size2) inner
                         in fmap realPart ftResult
            ImFT arg ->
                case retrieveElementType arg mp of
                    R ->
                        let inner = fmap (:+ 0) . eval valMap $ expTwoR mp arg
                            ftResult = fourierTransform2D (size1, size2) inner
                         in fmap imagPart ftResult
                    C ->
                        let inner = eval valMap $ expTwoC mp arg
                            ftResult = fourierTransform2D (size1, size2) inner
                         in fmap imagPart ftResult
            _ -> error "expression structure Two R is wrong"
    | otherwise = error "Two r but shape is not [size1, size2] ??"

instance Evaluable Two R (Array (Int, Int) Double) where
    eval :: ValMaps -> Expression Two R -> Array (Int, Int) Double
    eval valMap (Expression n mp) = evaluate2DReal valMap (mp, n)

instance (KnownNat m, KnownNat n) =>
         Evaluable '( m, n) R (Array (Int, Int) Double) where
    eval :: ValMaps -> Expression '( m, n) R -> Array (Int, Int) Double
    eval valMap (Expression n mp) = evaluate2DReal valMap (mp, n)

-- | 
--
evaluate2DComplex ::
       ValMaps -> (ExpressionMap, Int) -> Array (Int, Int) (Complex Double)
evaluate2DComplex valMap (mp, n)
    | [size1, size2] <- retrieveShape n mp =
        case retrieveNode n mp of
            Sum C args ->
                foldrElementwise (+) . map (eval valMap . expTwoC mp) $ args
            Mul C args ->
                foldrElementwise (*) . map (eval valMap . expTwoC mp) $ args
            Power x arg -> fmap (^ x) (eval valMap $ expTwoC mp arg)
            Neg C arg -> fmap negate . eval valMap $ expTwoC mp arg
            Scale C arg1 arg2 ->
                case retrieveElementType arg1 mp of
                    R ->
                        let scalar = fromR . eval valMap $ expZeroR mp arg1
                         in fmap (scalar *) . eval valMap $ expTwoC mp arg2
                    C ->
                        let scalar = eval valMap $ expZeroC mp arg1
                         in fmap (scalar *) . eval valMap $ expTwoC mp arg2
            RealImag arg1 arg2 ->
                zipWithA
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
                                  chooseBranch marks (cdt ! (i, j)) branches
                        ]
            Rotate [amount1, amount2] arg ->
                rotate2D
                    (size1, size2)
                    (amount1, amount2)
                    (eval valMap $ expTwoC mp arg)
            _ -> error "expression structure Two C is wrong"
    | otherwise = error "Two C but shape is not [size1, size2] ??"

instance Evaluable Two C (Array (Int, Int) (Complex Double)) where
    eval :: ValMaps -> Expression Two C -> Array (Int, Int) (Complex Double)
    eval valMap (Expression n mp) = evaluate2DComplex valMap (mp, n)

instance (KnownNat m, KnownNat n) =>
         Evaluable '( m, n) C (Array (Int, Int) (Complex Double)) where
    eval ::
           ValMaps -> Expression '( m, n) C -> Array (Int, Int) (Complex Double)
    eval valMap (Expression n mp) = evaluate2DComplex valMap (mp, n)

evaluate3DReal ::
       ValMaps -> (ExpressionMap, Int) -> Array (Int, Int, Int) Double
evaluate3DReal valMap (mp, n)
    | [size1, size2, size3] <- retrieveShape n mp =
        case retrieveNode n mp of
            Var name ->
                case Map.lookup name valMap of
                    Just (V3D val) -> val
                    _ -> error "no value associated with the variable"
            Const val ->
                listArray ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1)) $
                replicate (size1 * size2 * size3) val
            Sum R args ->
                foldrElementwise (+) . map (eval valMap . expThreeR mp) $ args
            Mul R args ->
                foldrElementwise (*) . map (eval valMap . expThreeR mp) $ args
            Power x arg -> fmap (^ x) (eval valMap $ expThreeR mp arg)
            Neg R arg -> fmap negate . eval valMap $ expThreeR mp arg
            Scale R arg1 arg2 ->
                let scalar = eval valMap $ expZeroR mp arg1
                 in fmap (scalar *) . eval valMap $ expThreeR mp arg2
            Div arg1 arg2 ->
                zipWithA
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
            RealPart arg -> fmap realPart . eval valMap $ expThreeC mp arg
            ImagPart arg -> fmap imagPart . eval valMap $ expThreeC mp arg
            Piecewise marks conditionArg branchArgs ->
                let cdt = eval valMap $ expThreeR mp conditionArg
                    branches = map (eval valMap . expThreeR mp) branchArgs
                 in listArray
                        ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
                        [ chosen ! (i, j, k)
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        , k <- [0 .. size3 - 1]
                        , let chosen =
                                  chooseBranch marks (cdt ! (i, j, k)) branches
                        ]
            Rotate [amount1, amount2, amount3] arg ->
                rotate3D
                    (size1, size2, size3)
                    (amount1, amount2, amount3)
                    (eval valMap $ expThreeR mp arg)
            TwiceReFT arg ->
                let innerRes = eval valMap $ expThreeR mp arg
                    scaleFactor =
                        fromIntegral size1 * fromIntegral size2 *
                        fromIntegral size3 /
                        2
                 in listArray
                        ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
                        [ scaleFactor *
                        (innerRes ! (i, j, k) +
                         innerRes !
                         ( (size1 - i) `mod` size1
                         , (size2 - j) `mod` size2
                         , (size3 - k) `mod` size3))
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        , k <- [0 .. size3 - 1]
                        ]
            TwiceImFT arg ->
                let innerRes = eval valMap $ expThreeR mp arg
                    scaleFactor =
                        fromIntegral size1 * fromIntegral size2 *
                        fromIntegral size3 /
                        2
                 in listArray
                        ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
                        [ scaleFactor *
                        (innerRes ! (i, j, k) -
                         innerRes !
                         ( (size1 - i) `mod` size1
                         , (size2 - j) `mod` size2
                         , (size3 - k) `mod` size3))
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        , k <- [0 .. size3 - 1]
                        ]
            ReFT arg ->
                case retrieveElementType arg mp of
                    R ->
                        let inner = fmap (:+ 0) . eval valMap $ expThreeR mp arg
                            ftResult =
                                fourierTransform3D (size1, size2, size3) inner
                         in fmap realPart ftResult
                    C ->
                        let inner = eval valMap $ expThreeC mp arg
                            ftResult =
                                fourierTransform3D (size1, size2, size3) inner
                         in fmap realPart ftResult
            ImFT arg ->
                case retrieveElementType arg mp of
                    R ->
                        let inner = fmap (:+ 0) . eval valMap $ expThreeR mp arg
                            ftResult =
                                fourierTransform3D (size1, size2, size3) inner
                         in fmap imagPart ftResult
                    C ->
                        let inner = eval valMap $ expThreeC mp arg
                            ftResult =
                                fourierTransform3D (size1, size2, size3) inner
                         in fmap imagPart ftResult
            _ -> error "expression structure Three R is wrong"
    | otherwise = error "Three r but shape is not [size1, size2, size3] ??"

-- | 
--
instance Evaluable Three R (Array (Int, Int, Int) Double) where
    eval :: ValMaps -> Expression Three R -> Array (Int, Int, Int) Double
    eval valMap (Expression n mp) = evaluate3DReal valMap (mp, n)

instance (KnownNat m, KnownNat n, KnownNat p) =>
         Evaluable '( m, n, p) R (Array (Int, Int, Int) Double) where
    eval :: ValMaps -> Expression '( m, n, p) R -> Array (Int, Int, Int) Double
    eval valMap (Expression n mp) = evaluate3DReal valMap (mp, n)

evaluate3DComplex ::
       ValMaps -> (ExpressionMap, Int) -> Array (Int, Int, Int) (Complex Double)
evaluate3DComplex valMap (mp, n)
    | [size1, size2, size3] <- retrieveShape n mp =
        case retrieveNode n mp of
            Sum C args ->
                foldrElementwise (+) . map (eval valMap . expThreeC mp) $ args
            Mul C args ->
                foldrElementwise (*) . map (eval valMap . expThreeC mp) $ args
            Power x arg -> fmap (^ x) (eval valMap $ expThreeC mp arg)
            Neg C arg -> fmap negate . eval valMap $ expThreeC mp arg
            Scale C arg1 arg2 ->
                case retrieveElementType arg1 mp of
                    R ->
                        let scalar = fromR . eval valMap $ expZeroR mp arg1
                         in fmap (scalar *) . eval valMap $ expThreeC mp arg2
                    C ->
                        let scalar = eval valMap $ expZeroC mp arg1
                         in fmap (scalar *) . eval valMap $ expThreeC mp arg2
            RealImag arg1 arg2 ->
                zipWithA
                    (:+)
                    (eval valMap $ expThreeR mp arg1)
                    (eval valMap $ expThreeR mp arg2)
            Piecewise marks conditionArg branchArgs ->
                let cdt = eval valMap $ expThreeR mp conditionArg
                    branches = map (eval valMap . expThreeC mp) branchArgs
                 in listArray
                        ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
                        [ chosen ! (i, j, k)
                        | i <- [0 .. size1 - 1]
                        , j <- [0 .. size2 - 1]
                        , k <- [0 .. size3 - 1]
                        , let chosen =
                                  chooseBranch marks (cdt ! (i, j, k)) branches
                        ]
            Rotate [amount1, amount2, amount3] arg ->
                rotate3D
                    (size1, size2, size3)
                    (amount1, amount2, amount3)
                    (eval valMap $ expThreeC mp arg)
            _ -> error "expression structure Three C is wrong"
    | otherwise = error "Three C but shape is not [size1, size2, size3] ??"

instance Evaluable Three C (Array (Int, Int, Int) (Complex Double)) where
    eval ::
           ValMaps
        -> Expression Three C
        -> Array (Int, Int, Int) (Complex Double)
    eval valMap (Expression n mp) = evaluate3DComplex valMap (mp, n)

instance (KnownNat m, KnownNat n, KnownNat p) =>
         Evaluable '( m, n, p) C (Array (Int, Int, Int) (Complex Double)) where
    eval ::
           ValMaps
        -> Expression '( m, n, p) C
        -> Array (Int, Int, Int) (Complex Double)
    eval valMap (Expression n mp) = evaluate3DComplex valMap (mp, n)

-- NOTE: `mod` in Haskell works with negative number, e.g, (-5) `mod` 3 = 1
-- | One dimension rotation
--
rotate1D ::
       Int -- ^ Size of the input array
    -> Int -- ^ amount of rotation
    -> Array Int a -- ^ Input array
    -> Array Int a -- ^ Rotated array
rotate1D size amount arr =
    listArray
        (0, size - 1)
        [arr ! ((i - amount) `mod` size) | i <- [0 .. size - 1]]

-- | Two dimension rotation
--
rotate2D ::
       (Int, Int) -- ^ Size of the 2d input array
    -> (Int, Int) -- ^ amount of rotation for 2d array
    -> Array (Int, Int) a -- ^ Input 2d array
    -> Array (Int, Int) a -- ^ Rotated 2d array
rotate2D (size1, size2) (amount1, amount2) arr =
    listArray
        ((0, 0), (size1 - 1, size2 - 1))
        [ arr ! ((i - amount1) `mod` size1, (j - amount2) `mod` size2)
        | i <- [0 .. size1 - 1]
        , j <- [0 .. size2 - 1]
        ]

-- | Three dimension rotation
--
rotate3D ::
       (Int, Int, Int) -- ^ Size of 3d input array
    -> (Int, Int, Int) -- ^ Amount of Rotation for 3d array
    -> Array (Int, Int, Int) a -- ^ Input 3d array
    -> Array (Int, Int, Int) a -- ^ Rotated 3d array
rotate3D (size1, size2, size3) (amount1, amount2, amount3) arr =
    listArray
        ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
        [ arr !
        ( (i - amount1) `mod` size1
        , (j - amount2) `mod` size2
        , (k - amount3) `mod` size3)
        | i <- [0 .. size1 - 1]
        , j <- [0 .. size2 - 1]
        , k <- [0 .. size3 - 1]
        ]

-- | 
--
fourierTransform1D ::
       Int -> Array Int (Complex Double) -> Array Int (Complex Double)
fourierTransform1D size arr =
    listArray (0, size - 1) [computeX i | i <- [0 .. size - 1]]
  where
    computeX i = sum $ zipWithA (*) arr (fourierBasis i)
    fourierBasis i =
        let frequency n = 2 * pi * fromIntegral (i * n) / fromIntegral size
         in listArray
                (0, size - 1)
                [ cos (frequency n) :+ (-sin (frequency n))
                | n <- [0 .. size - 1]
                ]

-- | 
--
fourierTransform2D ::
       (Int, Int)
    -> Array (Int, Int) (Complex Double)
    -> Array (Int, Int) (Complex Double)
fourierTransform2D (size1, size2) arr =
    listArray
        ((0, 0), (size1 - 1, size2 - 1))
        [computeX i j | i <- [0 .. size1 - 1], j <- [0 .. size2 - 1]]
  where
    computeX i j = sum $ zipWithA (*) arr (fourierBasis i j)
    fourierBasis i j =
        let frequency m n =
                2 * pi * fromIntegral (i * m) / fromIntegral size1 +
                2 * pi * fromIntegral (j * n) / fromIntegral size2
         in listArray
                ((0, 0), (size1 - 1, size2 - 1))
                [ cos (frequency m n) :+ (-sin (frequency m n))
                | m <- [0 .. size1 - 1]
                , n <- [0 .. size2 - 1]
                ]

-- |
--
fourierTransform3D ::
       (Int, Int, Int)
    -> Array (Int, Int, Int) (Complex Double)
    -> Array (Int, Int, Int) (Complex Double)
fourierTransform3D (size1, size2, size3) arr =
    listArray
        ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
        [ computeX i j k
        | i <- [0 .. size1 - 1]
        , j <- [0 .. size2 - 1]
        , k <- [0 .. size3 - 1]
        ]
  where
    computeX i j k = sum $ zipWithA (*) arr (fourierBasis i j k)
    fourierBasis i j k =
        let frequency m n p =
                2 * pi * fromIntegral (i * m) / fromIntegral size1 +
                2 * pi * fromIntegral (j * n) / fromIntegral size2 +
                2 * pi * fromIntegral (k * p) / fromIntegral size3
         in listArray
                ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
                [ cos (frequency m n p) :+ (-sin (frequency m n p))
                | m <- [0 .. size1 - 1]
                , n <- [0 .. size2 - 1]
                , p <- [0 .. size3 - 1]
                ]
