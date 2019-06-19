-------------------------------------------------------------------------------
-- | For simplifying expressions
--
-------------------------------------------------------------------------------
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
    , negate
    , sin
    , sinh
    , sqrt
    , tan
    , tanh
    )
import qualified Prelude as Prelude

-- | Simplify an expression
--
simplify ::
       (DimensionType d, ElementType et) => Expression d et -> Expression d et
simplify e
    -- Ok this is not Haskell idiomatic, but it makes sense in the context of simplification to use (|>)
 =
    let applyRules e = e |> zeroOneRules |> productRule |> sumRule |> otherRules
     in wrap . applyRules . unwrap $ e

-- | Simplification type, we can combine them, chain them, apply them n times using nest, ...
--
type Simplification = (ExpressionMap, Int) -> (ExpressionMap, Int)

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

infixl 1 |>

-- | Chain n simplifications together to a simplification
--
chain :: [Simplification] -> Simplification
chain = flip $ foldl (|>)

multipleTimes :: Int -> Simplification -> Simplification
multipleTimes = nest

-- | Turn HashedPattern to a simplification
--
fromPattern :: (GuardedPattern, Pattern) -> Simplification
fromPattern pt@(GP pattern condition, replacementPattern) ex@(originalMp, originalN)
    | Just capturesMap <- match ex pattern
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
                    PMul sps -> mulMany . map buildFromPattern $ sps
                    PSum sps -> sumMany . map buildFromPattern $ sps
                    PNeg sp ->
                        apply
                            (unaryET Neg ElementDefault)
                            [buildFromPattern sp]
                    PDiv sp1 sp2 ->
                        apply (binary Div) $ map buildFromPattern [sp1, sp2]
                    PSqrt sp -> apply (unary Sqrt) [buildFromPattern sp]
                    PSin sp -> apply (unary Sin) [buildFromPattern sp]
                    PCos sp -> apply (unary Cos) [buildFromPattern sp]
                    PTan sp -> apply (unary Tan) [buildFromPattern sp]
                    PExp sp -> apply (unary Exp) [buildFromPattern sp]
                    PLog sp -> apply (unary Log) [buildFromPattern sp]
                    PSinh sp -> apply (unary Sinh) [buildFromPattern sp]
                    PCosh sp -> apply (unary Cosh) [buildFromPattern sp]
                    PTanh sp -> apply (unary Tanh) [buildFromPattern sp]
                    PAsin sp -> apply (unary Asin) [buildFromPattern sp]
                    PAcos sp -> apply (unary Acos) [buildFromPattern sp]
                    PAtan sp -> apply (unary Atan) [buildFromPattern sp]
                    PAsinh sp -> apply (unary Asinh) [buildFromPattern sp]
                    PAcosh sp -> apply (unary Acosh) [buildFromPattern sp]
                    PAtanh sp -> apply (unary Atanh) [buildFromPattern sp]
                    PRealImag sp1 sp2 ->
                        apply (binary RealImag) $
                        map buildFromPattern [sp1, sp2]
                    PRealPart sp ->
                        apply (unary RealPart) [buildFromPattern sp]
                    PImagPart sp ->
                        apply (unary ImagPart) [buildFromPattern sp]
         in buildFromPattern replacementPattern
    | otherwise = (originalMp, originalN)

-- | Simplifications below
--
zeroOneRules :: Simplification
zeroOneRules =
    chain . map fromPattern $
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
    , xRe (x +: y) |.~~> x
    , xIm (x +: y) |.~~> y
    ]

complexNumRules :: Simplification
complexNumRules =
    chain . map fromPattern $
    [ xRe (x +: y) |.~~> x
    , xIm (x +: y) |.~~> y
    , (x +: y) + (u +: v) |.~~> (x + u) +: (y + v)
    , s *. (x +: y) |.~~> (s *. x) +: (s *. y) -- does not work for ScalarC, only vectorC; it's also in HashedComplexInstances
    , (x +: y) * (z +: w) |.~~> (x * z - y * w) +: (x * w + y * z)
    ]

productRule :: Simplification
productRule = id

sumRule :: Simplification
sumRule = id

dotProductRules :: Simplification
dotProductRules = id

otherRules :: Simplification
otherRules = id
