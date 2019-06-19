{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-} --

module HashedOperation where

import Data.IntMap.Strict (fromList, union)
import HashedExpression
import HashedHash
import HashedInner
import HashedUtils
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
instance (DimensionType d, Addable et) => AddableOp (Expression d et) where
    (+) :: Expression d et -> Expression d et -> Expression d et
    (+) e1 e2 =
        ensureSameShape e1 e2 $
        binaryET
            (\et arg1 arg2 -> Sum et [arg1, arg2])
            ElementDefault
            ShapeDefault
            e1
            e2
    negate :: Expression d et -> Expression d et
    negate = monoryET Neg

-- | Element-wise multiplication
--
instance (DimensionType d, NumType et) =>
         MultiplyOp (Expression d et) (Expression d et) (Expression d et) where
    (*) :: Expression d et -> Expression d et -> Expression d et
    (*) e1 e2 =
        ensureSameShape e1 e2 $
        binaryET
            (\et arg1 arg2 -> Mul et [arg1, arg2])
            ElementDefault
            ShapeDefault
            e1
            e2

-- | Scale in vector space
--
instance (VectorSpace d et s) =>
         VectorSpaceOp (Expression Zero s) (Expression d et) where
    scale :: Expression Zero s -> Expression d et -> Expression d et
    scale e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
        binaryET
            (\et arg1 arg2 -> Mul et [arg1, arg2])
            (ElementSpecific $ expressionElementType e2)
            (ShapeSpecific $ expressionShape e2)
            e1
            e2

---- | From R to C two part
----
instance (DimensionType d) =>
         ComplexRealOp (Expression d R) (Expression d C) where
    (+:) :: Expression d R -> Expression d R -> Expression d C
    (+:) = binary RealImag ShapeDefault
    xRe :: Expression d C -> Expression d R
    xRe = monory RealPart
    xIm :: Expression d C -> Expression d R
    xIm = monory ImagPart

-- | Element-wise division for R
--
(/) :: (DimensionType d) => Expression d R -> Expression d R -> Expression d R
(/) e1 e2 = ensureSameShape e1 e2 $ binary Div ShapeDefault e1 e2

-- | NumOp for R
--
instance (DimensionType d) => NumOp (Expression d R) where
    sqrt = monory Sqrt
    exp = monory Exp
    log = monory Log
    sin = monory Sin
    cos = monory Cos
    tan = monory Tan
    asin = monory Asin
    acos = monory Acos
    atan = monory Atan
    sinh = monory Sinh
    cosh = monory Cosh
    tanh = monory Tanh
    asinh = monory Asinh
    acosh = monory Acosh
    atanh = monory Atanh
