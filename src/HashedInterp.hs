{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module HashedInterp where

import Data.Array as A
import Data.Complex as DC
import qualified Data.IntMap.Strict as IM
import Data.Map (Map, fromList)
import qualified Data.Map as Map
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
import HashedUtils

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
                RealPart arg -> DC.realPart (eval valMap (expZeroC mp arg))
                ImagPart arg -> DC.imagPart (eval valMap (expZeroC mp arg))
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
                _ -> error "expression structure Scalar R is wrong"
        | otherwise = error "one r but shape is not [] ??"

instance Evaluable Zero C (DC.Complex Double) where
    eval :: ValMaps -> Expression Zero C -> DC.Complex Double
    eval valMap e@(Expression n mp)
        | [] <- retrieveShape n mp =
            case retrieveNode n mp of
                Sum C args -> sum . map (eval valMap . expZeroC mp) $ args
                Mul C args -> product . map (eval valMap . expZeroC mp) $ args
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
                _ -> error "expression structure Scalar C is wrong"
        | otherwise = error "One C but shape is not [] ??"

-- |
--
instance Evaluable One R (Array Int Double) where
    eval :: ValMaps -> Expression One R -> Array Int Double
    eval valMap e@(Expression n mp)
        | [size] <- retrieveShape n mp =
            let fmap :: (a -> Double) -> Array Int a -> Array Int Double
                fmap f arr =
                    listArray
                        (0, size - 1)
                        [f x | i <- [0 .. size - 1], let x = arr ! i]
                zipWith ::
                       (a -> b -> Double)
                    -> Array Int a
                    -> Array Int b
                    -> Array Int Double
                zipWith f arr1 arr2 =
                    listArray
                        (0, size - 1)
                        [ f x y
                        | i <- [0 .. size - 1]
                        , let x = arr1 ! i
                        , let y = arr2 ! i
                        ]
             in case retrieveNode n mp of
                    Var name ->
                        case Map.lookup name $ vm1 valMap of
                            Just val -> val
                            _ -> error "no value associated with the variable"
                    Const val -> listArray (0, size - 1) $ replicate size val
                    Sum R args -> undefined
                    Mul R args -> undefined
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
                    RealPart arg ->
                        fmap DC.realPart . eval valMap $ expOneC mp arg
                    ImagPart arg ->
                        fmap DC.imagPart . eval valMap $ expOneC mp arg
                    _ -> error "expression structure Scalar R is wrong"
        | otherwise = error "one r but shape is not [] ??"

-- |
--
instance Evaluable One C (Array Int (DC.Complex Double)) where
    eval :: ValMaps -> Expression One C -> Array Int (DC.Complex Double)
    eval valMap e@(Expression n mp) =
        case IM.lookup n mp of
            Just ([size], Sum C [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression One C
                    subExp2 = Expression node2 mp :: Expression One C
                    lst1 = A.elems $ eval valMap subExp1
                    lst2 = A.elems $ eval valMap subExp2
                    lstRes = zipWith (+) lst1 lst2
                 in A.listArray (0, size - 1) lstRes
            Just ([size], RealImag node1 node2) ->
                let subExp1 = Expression node1 mp :: Expression One R
                    subExp2 = Expression node2 mp :: Expression One R
                    lst1 = A.elems $ eval valMap subExp1
                    lst2 = A.elems $ eval valMap subExp2
                    lstRes = zipWith (:+) lst1 lst2
                 in A.listArray (0, size - 1) lstRes
            _ -> error "expression structure One C is wrong"

--            Just ([size], Mul C [node1, node2]) ->
--                let subExp1 = Expression node1 mp :: Expression One C
--                    subExp2 = Expression node2 mp :: Expression One C
--                    lst1 = A.elems $ eval valMap subExp1
--                    lst2 = A.elems $ eval valMap subExp2
--                    lstRes = zipWith (*) lst1 lst2
--                 in A.listArray (0, size - 1) lstRes
--            Just ([size], Scale C node1 node2) ->
--                let subExp2 = Expression node2 mp :: Expression One C
--                    lst = A.elems $ eval valMap subExp2
--                    scale =
--                        case nodeElementType . retrieveNode mp $ node1 of
--                            R ->
--                                fromR . eval valMap $
--                                (Expression node1 mp :: Expression Zero R)
--                            C ->
--                                eval
--                                    valMap
--                                    (Expression node1 mp :: Expression Zero C)
--                 in A.listArray (0, size - 1) $ map (* scale) lst
instance Evaluable Two R (Array (Int, Int) Double) where
    eval :: ValMaps -> Expression Two R -> Array (Int, Int) Double
    eval valMap e@(Expression n mp) = undefined

instance Evaluable Two C (Array (Int, Int) (DC.Complex Double)) where
    eval :: ValMaps -> Expression Two C -> Array (Int, Int) (DC.Complex Double)
    eval valMap e@(Expression n mp) = undefined

instance Evaluable Three R (Array (Int, Int, Int) Double) where
    eval :: ValMaps -> Expression Three R -> Array (Int, Int, Int) Double
    eval valMap e@(Expression n mp) = undefined

instance Evaluable Three C (Array (Int, Int, Int) (DC.Complex Double)) where
    eval ::
           ValMaps
        -> Expression Three C
        -> Array (Int, Int, Int) (DC.Complex Double)
    eval valMap e@(Expression n mp) = undefined
