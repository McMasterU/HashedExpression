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
    , (/)
    , acos
    , acosh
    , asin
    , asinh
    , atan
    , atanh
    , cos
    , cosh
    , exp
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
instance (DimensionType d, Addable et) =>
         AddableOp (Expression d et) (Expression d et) (Expression d et) where
    (+) :: Expression d et -> Expression d et -> Expression d et
    (+) = applyOpSameShapeSameElement (\et arg1 arg2 -> Sum et [arg1, arg2])

---- | TODO: Should it return Maybe (Expression d et)
----
--sum :: Addable et => [Expression d et] -> Maybe (Expression d et)
--sum [] = Nothing
--sum expressions = Just . ensureSameShapeList expressions $ Expression h newMap
--  where
--    sample = head expressions
--    elementType = expressionElementType sample
--    shape = expressionShape sample
--    node = Sum elementType . map exIndex $ expressions
--    mergedMap = foldl1 union . map exMap $ expressions
--    (newMap, h) = addEdge mergedMap (shape, node)
-- | Element-wise multiplication
--
instance (DimensionType d, NumType et) =>
         MultiplyOp (Expression d et) (Expression d et) (Expression d et) where
    (*) :: Expression d et -> Expression d et -> Expression d et
    (*) = applyOpSameShapeSameElement (\et arg1 arg2 -> Mul et [arg1, arg2])

-- | Scale in vector space
--
instance (VectorSpace d et s) =>
         VectorSpaceOp (Expression Zero s) (Expression d et) where
    scale :: Expression Zero s -> Expression d et -> Expression d et
    scale e1@(Expression n1 mp1) e2@(Expression n2 mp2) = Expression h newMap
      where
        elementType = expressionElementType e2
        shape = expressionShape e2
        node = Mul elementType [n1, n2]
        (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

---- | From R to C two part
----
instance (DimensionType d) =>
         ComplexRealOp (Expression d R) (Expression d C) where
    (+:) :: Expression d R -> Expression d R -> Expression d C
    (+:) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
        ensureSameShape e1 e2 $ Expression h newMap
      where
        shape = expressionShape e1
        node = RealImag n1 n2
        (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)
    realPart :: Expression d C -> Expression d R
    realPart = applyOp RealPart
    imagPart :: Expression d C -> Expression d R
    imagPart = applyOp ImagPart

-- | Element-wise division for R
--
(/) :: (DimensionType d) => Expression d R -> Expression d R -> Expression d R
(/) = applyOpSameShapeSameElement (\_ arg1 arg2 -> Div arg1 arg2)

-- | NumOp for R
--
instance (DimensionType d) => NumOp (Expression d R) where
    sqrt = applyOp Sqrt
    exp = applyOp Exp
    log = applyOp Log
    -- | Trigonometric operations
    --
    sin = applyOp Sin
    cos = applyOp Cos
    tan = applyOp Tan
    asin = applyOp Asin
    acos = applyOp Acos
    atan = applyOp Atan
    sinh = applyOp Sinh
    cosh = applyOp Cosh
    tanh = applyOp Tanh
    asinh = applyOp Asinh
    acosh = applyOp Acosh
    atanh = applyOp Atanh

-- | Utilities for writing operations
--
applyOp :: (Arg -> Node) -> Expression d et1 -> Expression d et2
applyOp op e@(Expression n mp) = Expression h newMap
  where
    shape = expressionShape e
    node = op n
    (newMap, h) = addEdge mp (shape, node)

applyOpSameShapeSameElement ::
       (ET -> Arg -> Arg -> Node)
    -> Expression d et1
    -> Expression d et1
    -> Expression d et1
applyOpSameShapeSameElement op e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    ensureSameShape e1 e2 $ Expression h newMap
  where
    elementType = expressionElementType e1
    shape = expressionShape e1
    node = op elementType n1 n2
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)
