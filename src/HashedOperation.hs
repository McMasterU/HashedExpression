{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HashedOperation where

import Data.IntMap.Strict
import HashedExpression
import HashedHash
import Prelude hiding ((*), (+))

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

-- | Element-wise sum
--
(+) :: (Ring d rc) => Expression d rc -> Expression d rc -> Expression d rc
(+) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    ensureSameShape e1 e2 $ Expression h newMap
  where
    numType = expressionNumType e1
    shape = expressionShape e1
    node = Sum numType [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

-- | Element-wise multiplication (like in MATLAB)
--
(.*) :: (Ring d rc) => Expression d rc -> Expression d rc -> Expression d rc
(.*) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    ensureSameShape e1 e2 $ Expression h newMap
  where
    numType = expressionNumType e1
    shape = expressionShape e1
    node = Mul numType [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

-- | Scale by scalar, TODO: put this inside typeclass with default implementation???
--
(*) :: VectorSpace d rc s
    => Expression Scalar s
    -> Expression d rc
    -> Expression d rc
(*) e1@(Expression n1 mp1) e2@(Expression n2 mp2) = Expression h newMap
  where
    numType = expressionNumType e2
    shape = expressionShape e2
    node = Scale numType n1 n2
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

-- | Inner product in Inner Product Space
--
(<.>) ::
       InnerProductSpace d rc
    => Expression d rc
    -> Expression d rc
    -> Expression Scalar rc
(<.>) e1@(Expression n1 mp1) e2@(Expression n2 mp2) = Expression h newMap
  where
    numType = expressionNumType e1
    shape = []
    node = InnerProd numType n1 n2
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

-- | From R to C two part
-- TODO: more constraint for this operation ? (Field d R, Field d C, ..)
--
(+:) :: (DimensionType d) => Expression d R -> Expression d R -> Expression d C
(+:) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    ensureSameShape e1 e2 $ Expression h newMap
  where
    shape = expressionShape e1
    node = RealImg n1 n2
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

