{-# LANGUAGE AllowAmbiguousTypes #-}

module HashedExpression.Operation
    ( variable
    , constant
    , constant1D
    , constant2D
    , constant3D
    , variable1D
    , variable2D
    , variable3D
    , norm2square
    , sum
    , product
    , huber
    , huberNorm
    , norm1
    , norm2
    , sumElements
    , valueFromNat
    ) where

import Data.Array
import Data.IntMap.Strict (fromList, union, unions)
import Data.List (sort)
import Data.Proxy
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownNat, natVal)
import HashedExpression.Internal.Expression

import HashedExpression.Internal.Hash
import HashedExpression.Internal.Inner
import HashedExpression.Internal.Node
import HashedExpression.Internal.Utils
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
    , product
    , sin
    , sinh
    , sqrt
    , sum
    , tan
    , tanh
    )
import qualified Prelude

-- | Create primitive expressions
--
variable :: String -> Expression Scalar R
variable name = Expression h (fromList [(h, node)])
  where
    node = ([], Var name)
    h = hash node

-- |
--
constant :: Double -> Expression Scalar R
constant val = Expression h (fromList [(h, node)])
  where
    node = ([], Const val)
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

sum :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
sum = applyNary (naryET Sum ElementDefault)

-- | Element-wise multiplication
--
instance (DimensionType d, NumType et) => MultiplyOp (Expression d et) where
    (*) :: Expression d et -> Expression d et -> Expression d et
    (*) e1 e2 =
        let op = naryET Mul ElementDefault `hasShape` expressionShape e1
         in ensureSameShape e1 e2 $ applyBinary op e1 e2

product :: (DimensionType d, NumType et) => [Expression d et] -> Expression d et
product = applyNary (naryET Mul ElementDefault)

-- | Element-wise multiplication
--
instance (DimensionType d, NumType et) => PowerOp (Expression d et) Int where
    (^) :: Expression d et -> Int -> Expression d et
    (^) e1 x = applyUnary (unary (Power x) `hasShape` expressionShape e1) e1

-- | Scale in vector space
--
instance (VectorSpace d et s) =>
         VectorSpaceOp (Expression Scalar s) (Expression d et) where
    scale :: Expression Scalar s -> Expression d et -> Expression d et
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
         InnerProductSpaceOp (Expression d s) (Expression d s) (Expression Scalar s) where
    (<.>) :: Expression d s -> Expression d s -> Expression Scalar s
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
huber delta e = piecewise [-delta, delta] e [outerLeft, inner, outerRight]
  where
    one = constWithShape @d (expressionShape e) 1
    inner = constant 0.5 *. (e * e)
    outerLeft = constant (-delta) *. e - constant (delta * delta / 2) *. one
    outerRight = constant delta *. e - constant (delta * delta / 2) *. one

-- | Norm 2
--
norm2 :: (DimensionType d) => Expression d R -> Expression Scalar R
norm2 expr = sqrt (expr <.> expr)

-- | Norm 1
--
norm1 :: (DimensionType d) => Expression d R -> Expression Scalar R
norm1 expr = sumElements (sqrt (expr * expr))

-- | Norm 2 square
--
class Norm2SquareOp a b | a -> b where
    norm2square :: a -> b

instance (DimensionType d) =>
         Norm2SquareOp (Expression d R) (Expression Scalar R) where
    norm2square :: Expression d R -> Expression Scalar R
    norm2square exp = exp <.> exp

instance (DimensionType d) =>
         Norm2SquareOp (Expression d C) (Expression Scalar R) where
    norm2square :: Expression d C -> Expression Scalar R
    norm2square exp = (xRe exp <.> xRe exp) + (xIm exp <.> xIm exp)

-- |
--
huberNorm ::
       (DimensionType d) => Double -> Expression d R -> Expression Scalar R
huberNorm alpha = sumElements . huber alpha

-- | Discrete fourier transform
--
-- | Sum across
--
sumElements ::
       forall d. (DimensionType d)
    => Expression d R
    -> Expression Scalar R
sumElements expr = expr <.> one
  where
    one = constWithShape (expressionShape expr) 1 :: Expression d R

-- | Piecewise, with a condition expression and branch expressions
-- This is element corresponding, so condition and all branches should have the same dimension and shape
--
instance (DimensionType d, ElementType et) =>
         PiecewiseOp (Expression d R) (Expression d et) where
    piecewise ::
           HasCallStack
        => [Double]
        -> Expression d R
        -> [Expression d et]
        -> Expression d et
    piecewise marks conditionExp branchExps
        | not (null marks)
        , (Set.toList . Set.fromList $ marks) == marks
        , length marks + 1 == length branchExps =
            guard $
            applyConditionAry
                (conditionAry (Piecewise marks))
                conditionExp
                branchExps
        | otherwise =
            error $
            "Must satisfy number of marks = number of branches - 1, and marks are increasing " ++
            show marks
      where
        guard =
            ensureSameShapeList branchExps .
            ensureSameShape conditionExp (head branchExps)

instance (DimensionType d) => FTOp (Expression d C) (Expression d C) where
    ft :: Expression d C -> Expression d C
    ft e
        | isScalarShape $ expressionShape e = e
        | otherwise =
            let reFT = applyUnary (unary ReFT) e
                imFT = applyUnary (unary ImFT) e
             in reFT +: imFT

instance (DimensionType d) => FTOp (Expression d R) (Expression d C) where
    ft :: Expression d R -> Expression d C
    ft e = ft (e +: constWithShape (expressionShape e) 0)

-- | 
--
instance (ElementType et, KnownNat n) => RotateOp Int (Expression n et) where
    rotate :: Int -> Expression n et -> Expression n et
    rotate x = applyUnary . unary $ Rotate [x]

instance (ElementType et, KnownNat m, KnownNat n) =>
         RotateOp (Int, Int) (Expression '( m, n) et) where
    rotate :: (Int, Int) -> Expression '( m, n) et -> Expression '( m, n) et
    rotate (x, y) = applyUnary . unary $ Rotate [x, y]

instance (ElementType et, KnownNat m, KnownNat n, KnownNat p) =>
         RotateOp (Int, Int, Int) (Expression '( m, n, p) et) where
    rotate ::
           (Int, Int, Int)
        -> Expression '( m, n, p) et
        -> Expression '( m, n, p) et
    rotate (x, y, z) = applyUnary . unary $ Rotate [x, y, z]

-- | 
--
valueFromNat ::
       forall n. (KnownNat n)
    => Int
valueFromNat = fromIntegral $ natVal (Proxy :: Proxy n)

-- | Create primitive expressions using Nat kind
--
variable1D ::
       forall n. (KnownNat n)
    => String
    -> Expression n R
variable1D name = Expression h (fromList [(h, node)])
  where
    size = valueFromNat @n
    node = ([size], Var name)
    h = hash node

variable2D ::
       forall m n. (KnownNat m, KnownNat n)
    => String
    -> Expression '( m, n) R
variable2D name = Expression h (fromList [(h, node)])
  where
    size1 = valueFromNat @m
    size2 = valueFromNat @n
    node = ([size1, size2], Var name)
    h = hash node

variable3D ::
       forall m n p. (KnownNat m, KnownNat n, KnownNat p)
    => String
    -> Expression '( m, n, p) R
variable3D name = Expression h (fromList [(h, node)])
  where
    size1 = valueFromNat @m
    size2 = valueFromNat @n
    size3 = valueFromNat @p
    node = ([size1, size3], Var name)
    h = hash node

-- |
--
constant1D ::
       forall n. (KnownNat n)
    => Double
    -> Expression n R
constant1D val = Expression h (fromList [(h, node)])
  where
    size = valueFromNat @n
    node = ([size], Const val)
    h = hash node

constant2D ::
       forall m n. (KnownNat m, KnownNat n)
    => Double
    -> Expression '( m, n) R
constant2D val = Expression h (fromList [(h, node)])
  where
    size1 = valueFromNat @m
    size2 = valueFromNat @n
    node = ([size1, size2], Const val)
    h = hash node

constant3D ::
       forall m n p. (KnownNat m, KnownNat n, KnownNat p)
    => Double
    -> Expression '( m, n, p) R
constant3D val = Expression h (fromList [(h, node)])
  where
    size1 = valueFromNat @m
    size2 = valueFromNat @n
    size3 = valueFromNat @p
    node = ([size1, size3], Const val)
    h = hash node
