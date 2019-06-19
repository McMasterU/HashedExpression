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
        let op = multiryET Sum ElementDefault `hasShape` expressionShape e1
         in ensureSameShape e1 e2 $ applyBinary op e1 e2
    negate :: Expression d et -> Expression d et
    negate =
        let op = monoryET Neg ElementDefault
         in applyMonory $ monoryET Neg ElementDefault

-- | Element-wise multiplication
--
instance (DimensionType d, NumType et) =>
         MultiplyOp (Expression d et) (Expression d et) (Expression d et) where
    (*) :: Expression d et -> Expression d et -> Expression d et
    (*) e1 e2 =
        let op = multiryET Mul ElementDefault `hasShape` expressionShape e1
         in ensureSameShape e1 e2 $ applyBinary op e1 e2

-- | Scale in vector space
--
instance (VectorSpace d et s) =>
         VectorSpaceOp (Expression Zero s) (Expression d et) where
    scale :: Expression Zero s -> Expression d et -> Expression d et
    scale e1 e2 =
        let op =
                multiryET Mul (ElementSpecific $ expressionElementType e2) `hasShape`
                expressionShape e2
         in applyBinary op e1 e2

---- | From R to C two part
----
instance (DimensionType d) =>
         ComplexRealOp (Expression d R) (Expression d C) where
    (+:) :: Expression d R -> Expression d R -> Expression d C
    (+:) e1 e2 =
        let op = binary RealImag
         in ensureSameShape e1 e2 $ applyBinary op e1 e2
    xRe :: Expression d C -> Expression d R
    xRe =
        let op = monory RealPart
         in applyMonory op
    xIm :: Expression d C -> Expression d R
    xIm =
        let op = monory RealPart
         in applyMonory op

-- | Element-wise division for R
--
(/) :: (DimensionType d) => Expression d R -> Expression d R -> Expression d R
(/) e1 e2 =
    let op = binary Div
     in ensureSameShape e1 e2 $ applyBinary op e1 e2

-- | NumOp for R
--
instance (DimensionType d) => NumOp (Expression d R) where
    sqrt = applyMonory (monory Sqrt)
    exp = applyMonory (monory Exp)
    log = applyMonory (monory Log)
    sin = applyMonory (monory Sin)
    cos = applyMonory (monory Cos)
    tan = applyMonory (monory Tan)
    asin = applyMonory (monory Asin)
    acos = applyMonory (monory Acos)
    atan = applyMonory (monory Atan)
    sinh = applyMonory (monory Sinh)
    cosh = applyMonory (monory Cosh)
    tanh = applyMonory (monory Tanh)
    asinh = applyMonory (monory Asinh)
    acosh = applyMonory (monory Acosh)
    atanh = applyMonory (monory Atanh)
