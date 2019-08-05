{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-} --

module HashedOperation where

import Data.Array
import Data.IntMap.Strict (fromList, union, unions)
import HashedExpression
import HashedHash
import HashedInner
import HashedNode
import HashedUtils
import Prelude hiding
    ( (*)
    , (+)
    , (-)
    , (/)
    , (^)
    , acos
    , acosh
    , asin
    , asinh
    , atan
    , atanh
    , const
    , cos
    , cosh
    , exp
    , negate
    , sin
    , sinh
    , sqrt
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

instance (DimensionType d) => NegateOp (Expression d et) where
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
instance (DimensionType d, NumType et) => MultiplyOp (Expression d et) where
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

-- | Element-wise multiplication
--
instance (DimensionType d, NumType et) => PowerOp (Expression d et) Int where
    (^) :: Expression d et -> Int -> Expression d et
    (^) e1 x = applyUnary (unary (Power x) `hasShape` expressionShape e1) e1

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
    (/) e1 e2 = ensureSameShape e1 e2 $ e1 * e2 ^ (-1)

-- | inner product
--
instance (InnerProductSpace d s) =>
         InnerProductSpaceOp (Expression d s) (Expression d s) (Expression Zero s) where
    (<.>) :: Expression d s -> Expression d s -> Expression Zero s
    (<.>) e1 e2 =
        let scalarShape = []
            op =
                binaryET InnerProd (ElementSpecific $ expressionElementType e2) `hasShape`
                scalarShape
         in ensureSameShape e1 e2 $ applyBinary op e1 e2

-- | Huber loss: https://en.wikipedia.org/wiki/Huber_loss
--
huber ::
       forall d. (DimensionType d)
    => Double
    -> Expression d R
    -> Expression d R
huber delta e = piecewise [-delta, delta] e [lessThan, inRange, largerThan]
  where
    deltaVector =
        constWithShape (expressionShape e) (delta * delta) :: Expression d R
    inRange = const 0.5 *. (e * e)
    lessThan = negate (e * deltaVector) - const 0.5 *. deltaVector
    largerThan = e * deltaVector - const 0.5 *. deltaVector

huber2 ::
       forall d. (DimensionType d)
    => Double
    -> Expression d R
    -> Expression d R
huber2 delta e = piecewise [delta] (e * e) [lessThan, largerThan]
  where
    deltaVector =
        constWithShape (expressionShape e) (delta * delta) :: Expression d R
    lessThan = sqrt (e * e)
    largerThan = const 0.5 *. (e * e + deltaVector)

-- | Norm 2
--
norm2 :: (DimensionType d) => Expression d R -> Expression Zero R
norm2 expr = sqrt (expr <.> expr)

-- | Piecewise, with a condition expression and branch expressions
-- This is element corresponding, so condition and all branches should have the same dimension and shape
--
piecewise ::
       (DimensionType d)
    => [Double]
    -> Expression d R
    -> [Expression d et]
    -> Expression d et
piecewise marks conditionExp branchExps =
    guard $
    applyConditionAry (conditionAry (Piecewise marks)) conditionExp branchExps
  where
    guard =
        ensureSameShapeList branchExps .
        ensureSameShape conditionExp (head branchExps)

instance (ElementType et) => RotateOp Int (Expression One et) where
    rotate :: Int -> Expression One et -> Expression One et
    rotate x = applyUnary . unary $ Rotate [x]

instance (ElementType et) => RotateOp (Int, Int) (Expression Two et) where
    rotate :: (Int, Int) -> Expression Two et -> Expression Two et
    rotate (x, y) = applyUnary . unary $ Rotate [x, y]

instance (ElementType et) =>
         RotateOp (Int, Int, Int) (Expression Three et) where
    rotate :: (Int, Int, Int) -> Expression Three et -> Expression Three et
    rotate (x, y, z) = applyUnary . unary $ Rotate [x, y, z]
