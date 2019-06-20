-------------------------------------------------------------------------------
-- | For simplifying expressions
--
-------------------------------------------------------------------------------
{-# LANGUAGE TupleSections #-}

module HashedSimplify where

import Data.Function.HT (nest)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import HashedExpression
import HashedHash
import HashedInner
import HashedNode
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

-- | Simplification type, we can combine them, chain them, apply them n times using nest, ...
--
type Simplification = (ExpressionMap, Int) -> (ExpressionMap, Int)

-- | Chain n simplifications together to a simplification
--
chain :: [Simplification] -> Simplification
chain = flip $ foldl (|>)

multipleTimes :: Int -> Simplification -> Simplification
multipleTimes = nest

-- | Simplify an expression
--
simplify ::
       (DimensionType d, ElementType et) => Expression d et -> Expression d et
simplify e
    -- Ok this is not Haskell idiomatic, but it makes sense in the context of simplification to use (|>)
 =
    let applyRules e =
            e |> zeroOneRules |> dotProductRules |> exponentRules |> sumRule |>
            otherRules |>
            complexNumRules |>
            removeUnreachable
     in wrap . applyRules . unwrap $ e

-- | Simplifications below
--
zeroOneRules :: Simplification
zeroOneRules =
    multipleTimes 100 . makeRecursive . chain . map fromPattern $
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

complexNumRules :: Simplification
complexNumRules =
    multipleTimes 100 . makeRecursive . chain . map fromPattern $
    [ xRe (x +: y) |.~~> x
    , xIm (x +: y) |.~~> y
    , (x +: y) + (u +: v) |.~~> (x + u) +: (y + v)
    , s *. (x +: y) |.~~> (s *. x) +: (s *. y) -- does not work for ScalarC, only vectorC; it's also in HashedComplexInstances
    , (x +: y) * (z +: w) |.~~> (x * z - y * w) +: (x * w + y * z)
    ]

dotProductRules :: Simplification
dotProductRules =
    multipleTimes 100 . makeRecursive . chain . map fromPattern $
    [
    x <.> zero |.~~> zero
    , zero <.> x |.~~> zero
    , (s *. x) <.> y |.~~> s * (x <.> y) -- TB,CD,RF: *. --> * (FIX) 27/05/2015.
    , x <.> (s *. y) |.~~> s * (x <.> y) -- TB,CD,RF: *. --> * (FIX) 27/05/2015.
    , x * (y + z) |.~~> (x * y + x * z)
    , (y + z) * x |.~~> (x * y + x * z)
    , x *. (y + z) |.~~> (x *. y + x *. z)
    , (x <.> (y + z)) |.~~> ((x <.> y) + (x <.> z))
    , ((y + z) <.> x) |.~~> ((x <.> y) + (x <.> z))
    ]

exponentRules :: Simplification
exponentRules =
    multipleTimes 100 . makeRecursive . chain . map fromPattern $
    [exp (log (x)) |.~~> x, log (exp (x)) |.~~> x, exp (zero) |.~~> one]

sumRule :: Simplification
sumRule = id

otherRules :: Simplification
otherRules = id

-- | Remove unreachable nodes
--
removeUnreachable :: Simplification
removeUnreachable (mp, n) =
    let collectNode n =
            IS.insert n . IS.unions . map collectNode . nodeArgs $
            retrieveNode n mp
        reachableNodes = collectNode n -- Set Int
        reducedMap =
            IM.filterWithKey (\nId _ -> IS.member nId reachableNodes) mp -- Only keep those in reachable nodes
     in (reducedMap, n)

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
                        | otherwise ->
                            error
                                "Capture not in the [(Capture, Int)] which never happens"
                    (PConst pc) ->
                        case retrieveShape originalN originalMp of
                            [] -> unwrap $ const pc
                            [size] -> unwrap $ const1d size pc
                            [size1, size2] -> unwrap $ const2d (size1, size2) pc
                            [size1, size2, size3] ->
                                unwrap $ const3d (size1, size2, size3) pc
                            _ -> error "Dimension > 3"
                    PMul sps -> mulMany . map buildFromPattern $ sps
                    PSum sps -> sumMany . map buildFromPattern $ sps
                    PNeg sp ->
                        apply (unaryET Neg ElementDefault) [buildFromPattern sp]
                    PScale sp1 sp2 ->
                        apply (binaryET Scale ElementDefault) $
                        map buildFromPattern [sp1, sp2]
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
                    PRealPart sp -> apply (unary RealPart) [buildFromPattern sp]
                    PImagPart sp -> apply (unary ImagPart) [buildFromPattern sp]
                    PInnerProd sp1 sp2 ->
                        apply (binaryET InnerProd ElementDefault `hasShape` []) $
                        map buildFromPattern [sp1, sp2]
         in buildFromPattern replacementPattern
    | otherwise = (originalMp, originalN)

-- | Turn a simplification to a recursive one, that is if the rule can't apply to the root node, then apply to it's children
--
makeRecursive :: Simplification -> Simplification
makeRecursive smp exp@(mp, n)
    | newExp@(_, newN) <- smp exp
    , n /= newN = newExp
    | otherwise =
        let shape = retrieveShape n mp
            simplifiedChildren =
                map smp . map (mp, ) . nodeArgs $ retrieveNode n mp
         in reconstruct exp simplifiedChildren
