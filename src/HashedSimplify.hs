module HashedSimplify where

import HashedExpression
import HashedOperation
import HashedUtils
import Prelude hiding
    ( (*)
    , (+)
    , (-)
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
    , log
    , sin
    , sinh
    , sqrt
    , tan
    , tanh
    )
import WithHoles

-- |
--
simplify ::
       (DimensionType d, ElementType et) => Expression d et -> Expression d et
simplify e@(Expression n mp) =
    uncurry (flip Expression) . sub 1000 n . simplifyRewrite $ (mp, n)
  where
    sub :: Int -> Int -> (ExpressionMap, Int) -> (ExpressionMap, Int)
    sub 0 oldN _ =
        error "simpRewrite' ran out of iterations without reaching fixed point"
    sub iter oldN (newMap, newN)
        | newN == oldN = (newMap, newN)
        | otherwise = sub iter newN $ simplifyRewrite (newMap, newN)

-- |
--
simplifyRewrite :: (ExpressionMap, Int) -> (ExpressionMap, Int)
simplifyRewrite (mp, n)
    | Just res <- applyOne (mp, n) rules1 = res
    | otherwise = (mp, n)

-- |
--
applyOne ::
       (ExpressionMap, Int) -- (IntMap ExpressionEdge, Int)
    -> [(GuardedPattern, WithHoles)]
    -> Maybe (ExpressionMap, Int)
applyOne (e, n) ((GP pattern condition, replacement):rules)
    | Just found <- match (e, n) pattern
    , condition e found = Just $ (e, n) -- TODO
    | otherwise = applyOne (e, n) rules
applyOne _ [] = Nothing

-- |
--
rules1 :: [(GuardedPattern, WithHoles)]
rules1 =
    [ x *. (y *. z) |.~~> (x * y) *. z
    , one * x |.~~> x
    , x * one |.~~> x
    , zero * x |.~~> zero
    , x * zero |.~~> zero
    , zero *. x |.~~> zero
    , x *. zero |.~~> zero
    , one *. x |.~~> x
    , x + zero |.~~> x
    , zero + x |.~~> x
    ]
