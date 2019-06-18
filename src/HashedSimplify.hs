module HashedSimplify where

import HashedExpression
import HashedHash
import HashedOperation
import HashedPattern
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
    , const
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
import qualified Prelude as Prelude

-- |
--
simplify ::
       (DimensionType d, ElementType et) => Expression d et -> Expression d et
simplify e@(Expression n mp) = wrap . sub 1000 n . simplifyRewrite $ (mp, n)
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

--
-- |
--
applyOne ::
       (ExpressionMap, Int)
    -> [(GuardedPattern, Pattern)]
    -> Maybe (ExpressionMap, Int)
applyOne (originalMp, originalN) ((GP pattern condition, replacement):rules)
    | Just capturesMap <- match (originalMp, originalN) pattern
    , condition originalMp capturesMap =
        let buildFromPattern :: Pattern -> (ExpressionMap, Int)
            buildFromPattern pattern =
                case pattern of
                    (PHole capture)
                        | Just nId <- lookupCapture capture capturesMap ->
                            (originalMp, nId)
                    (PConst pc) ->
                        case retrieveShape originalN originalMp of
                            [] -> unwrap $ const pc
                            [size] -> unwrap $ const1d size pc
                            [size1, size2] -> unwrap $ const2d (size1, size2) pc
                            [size1, size2, size3] ->
                                unwrap $ const3d (size1, size2, size3) pc
                    PMul sps -> mul' . map buildFromPattern $ sps
                    PSum sps -> sum' . map buildFromPattern $ sps
         in Just $ buildFromPattern replacement
    | otherwise = applyOne (originalMp, originalN) rules
applyOne _ [] = Nothing

---- | Simplification rules
----
rules1 :: [(GuardedPattern, Pattern)]
rules1 =
    [ x *. (y *. z) |.~~> (x * y) *. z
    , one *. x |.~~> x
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
