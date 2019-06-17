module HashedSimplify where

import HashedExpression
import HashedHash
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
applyOne (mp, n) ((GP pattern condition, replacement):rules)
    | Just captures <- match (mp, n) pattern
    , condition mp captures = Just $ replace (mp, n) captures replacement
    | otherwise = applyOne (mp, n) rules
applyOne _ [] = Nothing

replace ::
       (ExpressionMap, Int)
    -> [(Capture, Int)]
    -> WithHoles
    -> (ExpressionMap, Int)
replace (mp, n) cns (WHHole c)
    | Just nId <- lookup c cns = (mp, nId)
replace (mp, n) cns (WHConst d) = addEdge mp (retrieveShape n mp, Const d)
replace (mp, n) cns replacement = case (retrieveNode n mp, replacement) of
    (Sum _ args, WHSum whs) -> undefined
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
    , x + zero |.~~> x -- added these two TB 01/06/2015
    , zero + x |.~~> x
    ]
