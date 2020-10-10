module HashedExpression.Modeling.Simple where

import HashedExpression.Internal.Base
import HashedExpression.Internal.Builder
import HashedExpression.Internal.MonadExpression
import HashedExpression.Internal.Node

-- | Expr is an alias of Expr builder
type Expr = ExprBuilder

instance IsExpression Expr where
  asRawExpr = buildExpr

instance IsScalarReal Expr where
  asScalarRealRawExpr = buildExpr

variable :: Shape -> String -> Expr
variable shape name = introduceNode (shape, R, Var name)

variableScalar :: String -> Expr
variableScalar = variable []

variable1D :: Int -> String -> Expr
variable1D n = variable [n]

variable2D :: Int -> Int -> String -> Expr
variable2D m n = variable [m, n]

variable3D :: Int -> Int -> Int -> String -> Expr
variable3D m n p = variable [m, n, p]

param :: Shape -> String -> Expr
param shape name = introduceNode (shape, R, Param name)

paramScalar :: String -> Expr
paramScalar = param []

param1D :: Int -> String -> Expr
param1D n = param [n]

param2D :: Int -> Int -> String -> Expr
param2D m n = param [m, n]

param3D :: Int -> Int -> Int -> String -> Expr
param3D m n p = param [m, n, p]

constant :: Shape -> Double -> Expr
constant shape val = introduceNode (shape, R, Const val)

constantScalar :: Double -> Expr
constantScalar = constant []

constant1D :: Int -> Double -> Expr
constant1D n = constant [n]

constant2D :: Int -> Int -> Double -> Expr
constant2D m n = constant [m, n]

constant3D :: Int -> Int -> Int -> Double -> Expr
constant3D m n p = constant [m, n, p]

-- | Huber loss: https://en.wikipedia.org/wiki/Huber_loss.
-- Piecewise loss function where the loss algorithm chosen depends on delta
huber :: Double -> Expr -> Expr
huber delta e = piecewise [- delta, delta] e [outerLeft, inner, outerRight]
  where
    shape = getShape e
    inner = constantScalar 0.5 *. (e * e)
    outerLeft = constantScalar (- delta) *. e - constant shape (delta * delta / 2)
    outerRight = constantScalar delta *. e - constant shape (delta * delta / 2)

sumElements :: Expr -> Expr
sumElements expr = expr <.> (constant (getShape expr) 1)

huberNorm :: Double -> Expr -> Expr
huberNorm alpha = sumElements . huber alpha

norm2squareR :: Expr -> Expr
norm2squareR x = x <.> x
