{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-} -- allows constraining rc to R for example

module HashedOperation where

import Data.IntMap.Strict
import HashedExpression
import HashedHash
import Prelude hiding
    ( (*)
    , (+)
    , acos
    , acosh
    , asin
    , asinh
    , atan
    , atanh
    , cos
    , cosh
    , sin
    , sinh
    , tan
    , tanh
    )

-- | Create primitive expressions
--
var :: String -> Expression Scalar R
var name = Expression h (fromList [(h, node)])
  where
    node = ([], Var name)
    h = hash node

var1d :: Int -> String -> Expression One R
var1d size name = Expression h (fromList [(h, node)])
  where
    node = ([size], Var name)
    h = hash node

var2d :: (Int, Int) -> String -> Expression Two R
var2d (size1, size2) name = Expression h (fromList [(h, node)])
  where
    node = ([size1, size2], Var name)
    h = hash node

-- | Element-wise multiplication (like in MATLAB)
--
--(.*) :: (Ring d rc) => Expression d rc -> Expression d rc -> Expression d rc
--(.*) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
--    ensureSameShape e1 e2 $ Expression h newMap
--  where
--    numType = expressionElementType e1
--    shape = expressionShape e1
--    node = Mul numType [n1, n2]
--    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)
--
---- | Element-wise sum
----
--(+) :: Addable d rc => Expression d rc -> Expression d rc -> Expression d rc
--(+) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
--    ensureSameShape e1 e2 $ Expression h newMap
--  where
--    numType = expressionElementType e1
--    shape = expressionShape e1
--    node = Sum numType [n1, n2]
--    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)
--
--infixl 6 +
--
---- | Scale by scalar, TODO: put this inside typeclass with default implementation???
----
--(*) :: VectorSpace d rc s
--    => Expression Scalar s
--    -> Expression d rc
--    -> Expression d rc
--(*) e1@(Expression n1 mp1) e2@(Expression n2 mp2) = Expression h newMap
--  where
--    numType = expressionElementType e2
--    shape = expressionShape e2
--    node = Scale numType n1 n2
--    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)
--
--infixl 7 *
--
---- | Inner product in Inner Product Space
----
--(<.>) ::
--       InnerProductSpace d rc
--    => Expression d rc
--    -> Expression d rc
--    -> Expression Scalar rc
--(<.>) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
--    ensureSameShape e1 e2 $ Expression h newMap
--  where
--    numType = expressionElementType e1
--    shape = []
--    node = InnerProd numType n1 n2
--    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)
--
---- | From R to C two part
---- TODO: more constraint for this operation ? (Field d R, Field d C, ..)
----
--(+:) :: (DimensionType d) => Expression d R -> Expression d R -> Expression d C
--(+:) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
--    ensureSameShape e1 e2 $ Expression h newMap
--  where
--    shape = expressionShape e1
--    node = RealImg n1 n2
--    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)
--
--sin :: (Ring d R) => Expression d R -> Expression d R
--sin e@(Expression n mp) = Expression h newMap
--  where
--    shape = expressionShape e
--    node = Sin n
--    (newMap, h) = addEdge mp (shape, node)
--
--cos :: (Ring d R) => Expression d R -> Expression d R
--cos e@(Expression n mp) = Expression h newMap
--  where
--    shape = expressionShape e
--    node = Cos n
--    (newMap, h) = addEdge mp (shape, node)
--
--tan :: (Ring d R) => Expression d R -> Expression d R
--tan e@(Expression n mp) = Expression h newMap
--  where
--    shape = expressionShape e
--    node = Tan n
--    (newMap, h) = addEdge mp (shape, node)
--
--asin :: (Ring d R) => Expression d R -> Expression d R
--asin e@(Expression n mp) = Expression h newMap
--  where
--    shape = expressionShape e
--    node = Asin n
--    (newMap, h) = addEdge mp (shape, node)
--
--acos :: (Ring d R) => Expression d R -> Expression d R
--acos e@(Expression n mp) = Expression h newMap
--  where
--    shape = expressionShape e
--    node = Acos n
--    (newMap, h) = addEdge mp (shape, node)
--
--atan :: (Ring d R) => Expression d R -> Expression d R
--atan e@(Expression n mp) = Expression h newMap
--  where
--    shape = expressionShape e
--    node = Atan n
--    (newMap, h) = addEdge mp (shape, node)
--
--sinh :: (Ring d R) => Expression d R -> Expression d R
--sinh e@(Expression n mp) = Expression h newMap
--  where
--    shape = expressionShape e
--    node = Sinh n
--    (newMap, h) = addEdge mp (shape, node)
--
--cosh :: (Ring d R) => Expression d R -> Expression d R
--cosh e@(Expression n mp) = Expression h newMap
--  where
--    shape = expressionShape e
--    node = Cosh n
--    (newMap, h) = addEdge mp (shape, node)
--
--tanh :: (Ring d R) => Expression d R -> Expression d R
--tanh e@(Expression n mp) = Expression h newMap
--  where
--    shape = expressionShape e
--    node = Tanh n
--    (newMap, h) = addEdge mp (shape, node)
--
--asinh :: (Ring d R) => Expression d R -> Expression d R
--asinh e@(Expression n mp) = Expression h newMap
--  where
--    shape = expressionShape e
--    node = Asinh n
--    (newMap, h) = addEdge mp (shape, node)
--
--acosh :: (Ring d R) => Expression d R -> Expression d R
--acosh e@(Expression n mp) = Expression h newMap
--  where
--    shape = expressionShape e
--    node = Acosh n
--    (newMap, h) = addEdge mp (shape, node)
--
--atanh :: (Ring d R) => Expression d R -> Expression d R
--atanh e@(Expression n mp) = Expression h newMap
--  where
--    shape = expressionShape e
--    node = Atanh n
--    (newMap, h) = addEdge mp (shape, node)
--
--realPart :: (DimensionType d) => Expression d C -> Expression d R
--realPart e@(Expression n mp) = Expression h newMap
--  where
--    shape = expressionShape e
--    node = RealPart n
--    (newMap, h) = addEdge mp (shape, node)
--
--imagPart :: (DimensionType d) => Expression d C -> Expression d R
--imagPart e@(Expression n mp) = Expression h newMap
--  where
--    shape = expressionShape e
--    node = ImagPart n
--    (newMap, h) = addEdge mp (shape, node)
