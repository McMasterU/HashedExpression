{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-} --

module HashedOperation where

import Data.IntMap.Strict (fromList, union)
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
var :: String -> Expression Zero R
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

var3d :: (Int, Int, Int) -> String -> Expression Three R
var3d (size1, size2, size3) name = Expression h (fromList [(h, node)])
  where
    node = ([size1, size2, size3], Var name)
    h = hash node

-- |
--
const :: Double -> Expression Zero R
const val = Expression h (fromList [(h, node)])
  where
    node = ([], Const val)
    h = hash node

const1d :: Int -> Double -> Expression One R
const1d size val = Expression h (fromList [(h, node)])
  where
    node = ([size], Const val)
    h = hash node

const2d :: (Int, Int) -> Double -> Expression Two R
const2d (size1, size2) val = Expression h (fromList [(h, node)])
  where
    node = ([size1, size2], Const val)
    h = hash node

const3d :: (Int, Int, Int) -> Double -> Expression Three R
const3d (size1, size2, size3) val = Expression h (fromList [(h, node)])
  where
    node = ([size1, size2, size3], Const val)
    h = hash node

-- | Element-wise sum
--
(+) :: Addable et => Expression d et -> Expression d et -> Expression d et
(+) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    ensureSameShape e1 e2 $ Expression h newMap
  where
    elementType = expressionElementType e1
    shape = expressionShape e1
    node = Sum elementType [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

-- | TODO: Should it return Maybe (Expression d et)
--
sum :: Addable et => [Expression d et] -> Maybe (Expression d et)
sum [] = Nothing
sum expressions = Just . ensureSameShapeList expressions $ Expression h newMap
  where
    sample = head expressions
    elementType = expressionElementType sample
    shape = expressionShape sample
    node = Sum elementType . map exIndex $ expressions
    mergedMap = foldl1 union . map exMap $ expressions
    (newMap, h) = addEdge mergedMap (shape, node)

infixl 6 +

infixl 7 *

-- | Element-wise multiplication
--
(*) :: (DimensionType d, NumType et)
    => Expression d et
    -> Expression d et
    -> Expression d et
(*) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    ensureSameShape e1 e2 $ Expression h newMap
  where
    elementType = expressionElementType e1
    shape = expressionShape e1
    node = Mul elementType [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

-- | Scale in vector space
--
scale ::
       (VectorSpace d et s)
    => Expression Zero s
    -> Expression d et
    -> Expression d et
scale e1@(Expression n1 mp1) e2@(Expression n2 mp2) = Expression h newMap
  where
    elementType = expressionElementType e2
    shape = expressionShape e2
    node = Mul elementType [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

---- | From R to C two part
----
(+:) :: (DimensionType d) => Expression d R -> Expression d R -> Expression d C
(+:) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    ensureSameShape e1 e2 $ Expression h newMap
  where
    shape = expressionShape e1
    node = RealImag n1 n2
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

realPart :: (DimensionType d) => Expression d C -> Expression d R
realPart e@(Expression n mp) = Expression h newMap
  where
    shape = expressionShape e
    node = RealPart n
    (newMap, h) = addEdge mp (shape, node)

imagPart :: (DimensionType d) => Expression d C -> Expression d R
imagPart e@(Expression n mp) = Expression h newMap
  where
    shape = expressionShape e
    node = ImagPart n
    (newMap, h) = addEdge mp (shape, node)

-- | Element-wise division for R
--
(/) :: (DimensionType d) => Expression d R -> Expression d R -> Expression d R
(/) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    ensureSameShape e1 e2 $ Expression h newMap
  where
    elementType = expressionElementType e1
    shape = expressionShape e1
    node = Div n1 n2
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

-- | Square root
--
sqrt :: (DimensionType d) => Expression d R -> Expression d R
sqrt e@(Expression n mp) = Expression h newMap
  where
    shape = expressionShape e
    node = Sqrt n
    (newMap, h) = addEdge mp (shape, node)

-- | Trigonometric operations
--
sin :: (DimensionType d) => Expression d R -> Expression d R
sin e@(Expression n mp) = Expression h newMap
  where
    shape = expressionShape e
    node = Sin n
    (newMap, h) = addEdge mp (shape, node)

cos :: (DimensionType d) => Expression d R -> Expression d R
cos e@(Expression n mp) = Expression h newMap
  where
    shape = expressionShape e
    node = Cos n
    (newMap, h) = addEdge mp (shape, node)

tan :: (DimensionType d) => Expression d R -> Expression d R
tan e@(Expression n mp) = Expression h newMap
  where
    shape = expressionShape e
    node = Tan n
    (newMap, h) = addEdge mp (shape, node)

asin :: (DimensionType d) => Expression d R -> Expression d R
asin e@(Expression n mp) = Expression h newMap
  where
    shape = expressionShape e
    node = Asin n
    (newMap, h) = addEdge mp (shape, node)

acos :: (DimensionType d) => Expression d R -> Expression d R
acos e@(Expression n mp) = Expression h newMap
  where
    shape = expressionShape e
    node = Acos n
    (newMap, h) = addEdge mp (shape, node)

atan :: (DimensionType d) => Expression d R -> Expression d R
atan e@(Expression n mp) = Expression h newMap
  where
    shape = expressionShape e
    node = Atan n
    (newMap, h) = addEdge mp (shape, node)

sinh :: (DimensionType d) => Expression d R -> Expression d R
sinh e@(Expression n mp) = Expression h newMap
  where
    shape = expressionShape e
    node = Sinh n
    (newMap, h) = addEdge mp (shape, node)

cosh :: (DimensionType d) => Expression d R -> Expression d R
cosh e@(Expression n mp) = Expression h newMap
  where
    shape = expressionShape e
    node = Cosh n
    (newMap, h) = addEdge mp (shape, node)

tanh :: (DimensionType d) => Expression d R -> Expression d R
tanh e@(Expression n mp) = Expression h newMap
  where
    shape = expressionShape e
    node = Tanh n
    (newMap, h) = addEdge mp (shape, node)

asinh :: (DimensionType d) => Expression d R -> Expression d R
asinh e@(Expression n mp) = Expression h newMap
  where
    shape = expressionShape e
    node = Asinh n
    (newMap, h) = addEdge mp (shape, node)

acosh :: (DimensionType d) => Expression d R -> Expression d R
acosh e@(Expression n mp) = Expression h newMap
  where
    shape = expressionShape e
    node = Acosh n
    (newMap, h) = addEdge mp (shape, node)

atanh :: (DimensionType d) => Expression d R -> Expression d R
atanh e@(Expression n mp) = Expression h newMap
  where
    shape = expressionShape e
    node = Atanh n
    (newMap, h) = addEdge mp (shape, node)
