{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-} --

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
    , negate
    , sin
    , sinh
    , tan
    , tanh
    )
import qualified Prelude

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
        let op = naryET Sum ElementDefault `hasShape` expressionShape e1
         in ensureSameShape e1 e2 $ applyBinary op e1 e2
    negate :: Expression d et -> Expression d et
    negate =
        let op = unaryET Neg ElementDefault
         in applyUnary $ unaryET Neg ElementDefault

sum :: (DimensionType d, Addable et)
    => [Expression d et]
    -> Maybe (Expression d et)
sum [] = Nothing
sum es = Just . applyNary (naryET Sum ElementDefault) $ es

-- | Element-wise multiplication
--
instance (DimensionType d, NumType et) =>
         MultiplyOp (Expression d et) (Expression d et) (Expression d et) where
    (*) :: Expression d et -> Expression d et -> Expression d et
    (*) e1 e2 =
        let op = naryET Mul ElementDefault `hasShape` expressionShape e1
         in ensureSameShape e1 e2 $ applyBinary op e1 e2

product ::
       (DimensionType d, NumType et)
    => [Expression d et]
    -> Maybe (Expression d et)
product [] = Nothing
product es = Just . applyNary (naryET Mul ElementDefault) $ es

-- | Scale in vector space
--
instance (VectorSpace d et s) =>
         VectorSpaceOp (Expression Zero s) (Expression d et) where
    scale :: Expression Zero s -> Expression d et -> Expression d et
    scale e1 e2 =
        let op =
                binaryET Scale (ElementSpecific $ expressionElementType e2) `hasShape`
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
        let op = unary RealPart
         in applyUnary op
    xIm :: Expression d C -> Expression d R
    xIm =
        let op = unary ImagPart
         in applyUnary op

-- | Element-wise division for R
--
-- | NumOp for R
--
instance (DimensionType d) => NumOp (Expression d R) where
    sqrt = applyUnary (unary Sqrt)
    exp = applyUnary (unary Exp)
    log = applyUnary (unary Log)
    sin = applyUnary (unary Sin)
    cos = applyUnary (unary Cos)
    tan = applyUnary (unary Tan)
    asin = applyUnary (unary Asin)
    acos = applyUnary (unary Acos)
    atan = applyUnary (unary Atan)
    sinh = applyUnary (unary Sinh)
    cosh = applyUnary (unary Cosh)
    tanh = applyUnary (unary Tanh)
    asinh = applyUnary (unary Asinh)
    acosh = applyUnary (unary Acosh)
    atanh = applyUnary (unary Atanh)
    (/) e1 e2 =
        let op = binary Div
         in ensureSameShape e1 e2 $ applyBinary op e1 e2

instance (InnerProductSpace d s) =>
         InnerProductSpaceOp (Expression d s) (Expression d s) (Expression Zero s) where
    (<.>) :: Expression d s -> Expression d s -> Expression Zero s
    (<.>) e1 e2 =
        let scalarShape = []
            op =
                binaryET InnerProd (ElementSpecific $ expressionElementType e2) `hasShape`
                scalarShape
         in ensureSameShape e1 e2 $ applyBinary op e1 e2

-- | Prelude version of * and +
--
times :: (Num a) => a -> a -> a
times a b = Prelude.product [a, b]

plus :: (Num a) => a -> a -> a
plus a b = Prelude.sum [a, b]

instance {-# OVERLAPPABLE #-} Num a => AddableOp a where
    (+) = plus
    negate = Prelude.negate

instance {-# OVERLAPPABLE #-} Num a => MultiplyOp a a a where
    (*) = times

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
