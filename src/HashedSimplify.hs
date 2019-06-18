module HashedSimplify where

import Data.Function.HT (nest)
import HashedExpression
import HashedHash
import HashedInner
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

simplify ::
       (DimensionType d, ElementType et) => Expression d et -> Expression d et
simplify e =
    let (mp, n) = unwrap e
     in wrap $ (mp, n) |> zeroOneRules |> otherRules |> productRule |> sumRule

-- | Type for Simplification, we can combine them, chain them, apply them n times using nest, ...
--
type Simplification = (ExpressionMap, Int) -> (ExpressionMap, Int)

(|>) :: (ExpressionMap, Int) -> Simplification -> (ExpressionMap, Int)
(|>) exp smp = smp exp

infixl 1 |>

-- | Chain n simplifications together to a simplification
--
chain :: [Simplification] -> Simplification
chain ss init = foldl (|>) init ss

nTimes :: Int -> Simplification -> Simplification
nTimes = nest

-- | Turn HashedPattern to a simplification
--
fromPattern :: (GuardedPattern, Pattern) -> Simplification
fromPattern (GP pattern condition, replacementPattern) (originalMp, originalN)
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
         in buildFromPattern replacementPattern
    | otherwise = (originalMp, originalN)

-- | Simplifications below
--
zeroOneRules :: Simplification
zeroOneRules =
    nTimes 1000 . chain . map fromPattern $
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
    ] otherRules

otherRules = id

productRule :: Simplification
productRule = id

sumRule :: Simplification
sumRule = id

dotProductRules :: Simplification
dotProductRules = id
