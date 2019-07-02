{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

-------------------------------------------------------------------------------
-- |
-- (c) 2014 Christopher Kumar Anand
-- Helper functions/instances to make pattern gaurds involving Expressions
-- easier to read.
--
-------------------------------------------------------------------------------
module HashedPattern where

import Data.Map (Map, union)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace (trace)
import HashedExpression
import HashedInner
import HashedNode
import HashedOperation
import HashedUtils
import qualified Prelude
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
    , negate
    , sin
    , sinh
    , tan
    , tanh
    )

-- | Pattern for simplification
--
type Capture = Int

type ListCapture = Int

-- | List holes to captures many elements
--
data PatternList =
    PListHole [Pattern -> Pattern] ListCapture
    deriving (Show)

instance Show (Pattern -> Pattern) where
    show p = "Pattern List here"

-- GOAL:
--    piecewise condition branches |. allTheSame branches ~~> headOf branches
-- |
--
data Pattern
    = PHole Capture
    -- Ref to a node in the expression
    | PRef Int
    -- Ref to head of the list captures e.g: ListCapture(2) -> [2132, 34512, 4542],
    -- then PHead (tranformations) 2 ---> transformation 2132
    | PHead [Pattern -> Pattern] ListCapture
    -- MARK: For list capture
    | PSumList PatternList
    -- MARK: Reflex Node in HashedExpression
    | PConst Double
    | PSum [Pattern]
    | PMul [Pattern]
    | PNeg Pattern
    | PScale Pattern Pattern
    | PDiv Pattern Pattern
    | PSqrt Pattern
    | PSin Pattern
    | PCos Pattern
    | PTan Pattern
    | PExp Pattern
    | PLog Pattern
    | PSinh Pattern
    | PCosh Pattern
    | PTanh Pattern
    | PAsin Pattern
    | PAcos Pattern
    | PAtan Pattern
    | PAsinh Pattern
    | PAcosh Pattern
    | PAtanh Pattern
    | PRealImag Pattern Pattern
    | PRealPart Pattern
    | PImagPart Pattern
    | PInnerProd Pattern Pattern
    | PPiecewise Pattern PatternList
    deriving (Show)

instance AddableOp (Pattern) where
    (+) wh1 wh2 = PSum [wh1, wh2]
    negate = PNeg

instance MultiplyOp (Pattern) (Pattern) (Pattern) where
    (*) wh1 wh2 = PMul [wh1, wh2]

instance MultiplyOp (Pattern) (PatternList) (PatternList) where
    (*) wh1 (PListHole f listCapture) = PListHole ((wh1 *) : f) listCapture

instance MultiplyOp (PatternList) (Pattern) (PatternList) where
    (*) (PListHole f listCapture) wh2 = PListHole ((* wh2) : f) listCapture

instance VectorSpaceOp (Pattern) (Pattern) where
    scale = PScale

instance VectorSpaceOp (Pattern) (PatternList) where
    scale wh1 (PListHole f listCapture) =
        PListHole ((wh1 `scale`) : f) listCapture

instance NumOp (Pattern) where
    sqrt = PSqrt
    exp = PExp
    log = PLog
    sin = PSin
    cos = PCos
    tan = PTan
    asin = PAsin
    acos = PAcos
    atan = PAtan
    sinh = PSinh
    cosh = PCosh
    tanh = PTanh
    asinh = PAsinh
    acosh = PAcosh
    atanh = PAtanh
    (/) = PDiv

instance ComplexRealOp (Pattern) (Pattern) where
    (+:) = PRealImag
    xRe = PRealPart
    xIm = PImagPart

instance InnerProductSpaceOp (Pattern) (Pattern) (Pattern) where
    (<.>) = PInnerProd

instance InnerProductSpaceOp (Pattern) (PatternList) (PatternList) where
    (<.>) wh1 (PListHole f listCapture) = PListHole ((wh1 <.>) : f) listCapture

instance InnerProductSpaceOp (PatternList) (Pattern) (PatternList) where
    (<.>) (PListHole f listCapture) wh2 = PListHole ((<.> wh2) : f) listCapture

-- | Guarded patterns for simplification
--
data GuardedPattern =
    GP Pattern Condition

-- | Helper to make pattern and replacement without condition
--
(|.~~>) :: Pattern -> Pattern -> (GuardedPattern, Pattern)
(|.~~>) pattern replacement =
    (GP pattern $ Prelude.const (Prelude.const True), replacement)

infix 0 |.~~>, ~~>

(~~>) :: GuardedPattern -> Pattern -> (GuardedPattern, Pattern)
(~~>) gPattern replacement = (gPattern, replacement)

(|.) :: Pattern -> Condition -> GuardedPattern
(|.) pattern condition = GP pattern condition

infixl 1 |.

type Condition = (ExpressionMap, Int) -> Match -> Bool

-- |
--
isReal :: Pattern -> Condition
isReal p exp match = retrieveElementType n mp == R
  where
    (mp, n) = buildFromPattern exp match p

-- |
--
isComplex :: Pattern -> Condition
isComplex p exp match = retrieveElementType n mp == C
  where
    (mp, n) = buildFromPattern exp match p

-- |
--
isCovector :: Pattern -> Condition
isCovector p exp match = retrieveElementType n mp == Covector
  where
    (mp, n) = buildFromPattern exp match p

-- |
--
sameElementType :: [Pattern] -> Condition
sameElementType ps exp match = allEqual . map getET $ ps
  where
    getET = uncurry (flip retrieveElementType) . buildFromPattern exp match

-- |
--
allTheSame :: PatternList -> Condition
allTheSame pl@(PListHole _ listCapture) exp match
    | Just nIds <- Map.lookup listCapture . listCapturesMap $ match =
        allEqual nIds
    | otherwise = False

-- |
--
[p, q, r, s, t, u, v, w, x, y, z, condition] = map PHole [1 .. 12]

one :: Pattern
one = PConst 1

zero :: Pattern
zero = PConst 0

each :: PatternList
each = PListHole [] 1

sum :: PatternList -> Pattern
sum = PSumList

-- |
--
piecewise :: Pattern -> PatternList -> Pattern
piecewise = PPiecewise

branches :: PatternList
branches = PListHole [] 2

headOf :: PatternList -> Pattern
headOf (PListHole trans listCapture) = PHead trans listCapture

-- | Matches all nodes in the expression to see if they all match the PatternList, if they match, return
-- the inner actual nodes
-- e.g: matchList [a + (b * x), a + (b * y), a + (b * z)] (PatternList: (a + (b * _)) ---> Just [x, y, z]
--      matchList [x, y, z, t] (PatternList: (_)) = Just [x, y, z, t]
--      matchList [1 + x, 2 + x, y + x] (PatternList: (a + _)) = Nothing (not the same for all)
--
matchList :: ExpressionMap -> [Int] -> PatternList -> Maybe [Int]
matchList mp ns (PListHole fs listCapture)
    | all isJust maybeSubMatches
    , let subMatches = catMaybes maybeSubMatches
    , allEqual $ map otherCaptures subMatches = Just $ nIds subMatches
    | otherwise = Nothing
  where
    uniqueCapture = minBound :: Int
    eachHole = PHole uniqueCapture
    pt = foldr ($) eachHole fs
    matchOne nId = match (mp, nId) pt
    maybeSubMatches = map matchOne ns
    otherCaptures = Map.delete uniqueCapture . capturesMap
    nIds subMatches =
        catMaybes . map (Map.lookup uniqueCapture . capturesMap) $ subMatches

data Match =
    Match
        { capturesMap :: Map Capture Int
        , listCapturesMap :: Map ListCapture [Int]
        }

unionMatch :: Match -> Match -> Match
unionMatch match1 match2 =
    Match
        (capturesMap match1 `union` capturesMap match2)
        (listCapturesMap match1 `union` listCapturesMap match2)

-- | Match an expression with a pattern, return the map between capture hole to the actual node
-- e.g: match (Expression: (a(3243) + b(32521)) (PatternNormal:(x(1) + y(2)) --> ({1 -> 3243, 2 -> 32521}, {})
--      match (Expression sum(a(3243), b(32521), c(21321)) (PatternNormal:(sum(each(1))) --> ({}, {1 -> [3243, 32521, 21321]})
match :: (ExpressionMap, Int) -> Pattern -> Maybe Match
match (mp, n) outerWH =
    let unionBoth (x1, y1) (x2, y2) = (x1 `union` x2, y1 `union` y2)
        catMatch = foldl unionMatch (Match Map.empty Map.empty)
        recursiveAndCombine :: [Arg] -> [Pattern] -> Maybe Match
        recursiveAndCombine args whs
            | length args == length whs
            , let subMatches = zipWith match (map (mp, ) args) whs
            , all isJust subMatches = Just . catMatch . catMaybes $ subMatches
            | otherwise = Nothing
     in case (retrieveNode n mp, outerWH) of
            (_, PHole capture) ->
                Just $ Match (Map.fromList [(capture, n)]) Map.empty
            (Const c, PConst whc)
                | c == whc -> Just $ Match Map.empty Map.empty
            (Sum _ args, PSumList pl@(PListHole _ listCapture))
                | Just innerArgs <- matchList mp args pl ->
                    Just $
                    Match Map.empty (Map.fromList [(listCapture, innerArgs)])
                | otherwise -> Nothing
            (Sum _ args, PSum whs) -> recursiveAndCombine args whs
            (Mul _ args, PMul whs) -> recursiveAndCombine args whs
            (Neg _ arg, PNeg wh) -> recursiveAndCombine [arg] [wh]
            (Scale _ arg1 arg2, PScale wh1 wh2) ->
                recursiveAndCombine [arg1, arg2] [wh1, wh2]
            (Div arg1 arg2, PDiv wh1 wh2) ->
                recursiveAndCombine [arg1, arg2] [wh1, wh2]
            (Sqrt arg, PSqrt wh) -> recursiveAndCombine [arg] [wh]
            (Sin arg, PSin wh) -> recursiveAndCombine [arg] [wh]
            (Cos arg, PCos wh) -> recursiveAndCombine [arg] [wh]
            (Tan arg, PTan wh) -> recursiveAndCombine [arg] [wh]
            (Exp arg, PExp wh) -> recursiveAndCombine [arg] [wh]
            (Log arg, PLog wh) -> recursiveAndCombine [arg] [wh]
            (Sinh arg, PSinh wh) -> recursiveAndCombine [arg] [wh]
            (Cosh arg, PCosh wh) -> recursiveAndCombine [arg] [wh]
            (Tanh arg, PTanh wh) -> recursiveAndCombine [arg] [wh]
            (Asin arg, PAsin wh) -> recursiveAndCombine [arg] [wh]
            (Acos arg, PAcos wh) -> recursiveAndCombine [arg] [wh]
            (Atan arg, PAtan wh) -> recursiveAndCombine [arg] [wh]
            (Asinh arg, PAsinh wh) -> recursiveAndCombine [arg] [wh]
            (Acosh arg, PAcosh wh) -> recursiveAndCombine [arg] [wh]
            (Atanh arg, PAtanh wh) -> recursiveAndCombine [arg] [wh]
            (RealImag arg1 arg2, PRealImag wh1 wh2) ->
                recursiveAndCombine [arg1, arg2] [wh1, wh2]
            (RealPart arg, PRealPart wh) -> recursiveAndCombine [arg] [wh]
            (ImagPart arg, PImagPart wh) -> recursiveAndCombine [arg] [wh]
            (InnerProd _ arg1 arg2, PInnerProd wh1 wh2) ->
                recursiveAndCombine [arg1, arg2] [wh1, wh2]
            (Piecewise _ conditionArg branchArgs, PPiecewise wh pl@(PListHole _ listCapture))
                | Just innerArgs <- matchList mp branchArgs pl
                , Just (Match cMap lcMap) <-
                     recursiveAndCombine [conditionArg] [wh] ->
                    Just $
                    Match
                        cMap
                        (lcMap `union` (Map.fromList [(listCapture, innerArgs)]))
                | otherwise -> Nothing
            _ -> Nothing

turnToPattern :: [Pattern -> Pattern] -> Int -> Pattern
turnToPattern fs nId = foldr ($) (PRef nId) fs

buildFromPatternList ::
       (ExpressionMap, Int) -> Match -> PatternList -> [(ExpressionMap, Int)]
buildFromPatternList exp match (PListHole fs listCapture)
    | Just ns <- Map.lookup listCapture (listCapturesMap match) =
        map (buildFromPattern exp match . turnToPattern fs) ns
    | otherwise =
        error
            "ListCapture not in the Map ListCapture [Int] which should never happens"

buildFromPattern ::
       (ExpressionMap, Int) -> Match -> Pattern -> (ExpressionMap, Int)
buildFromPattern exp@(originalMp, originalN) match = buildFromPattern'
  where
    buildFromPattern' :: Pattern -> (ExpressionMap, Int)
    buildFromPattern' pattern =
        case pattern of
            PRef nId -> (originalMp, nId)
            PHole capture
                | Just nId <- Map.lookup capture (capturesMap match) ->
                    (originalMp, nId)
                | otherwise ->
                    error
                        "Capture not in the Map Capture Int which should never happens"
            PHead fs listCapture
                | Just ns <- Map.lookup listCapture (listCapturesMap match)
                , let pt = turnToPattern fs . head $ ns -> buildFromPattern' pt
                | otherwise -> error "ListCapture not in the Map ListCapture [Int] which should never happens"
            PConst pc ->
                case retrieveShape originalN originalMp of
                    [] -> unwrap $ const pc
                    [size] -> unwrap $ const1d size pc
                    [size1, size2] -> unwrap $ const2d (size1, size2) pc
                    [size1, size2, size3] ->
                        unwrap $ const3d (size1, size2, size3) pc
                    _ -> error "Dimension > 3"
            PSumList ptl -> sumMany . buildFromPatternList exp match $ ptl
            PSum sps -> sumMany . map buildFromPattern' $ sps
            PMul sps -> mulMany . map buildFromPattern' $ sps
            PNeg sp -> apply (unaryET Neg ElementDefault) [buildFromPattern' sp]
            PScale sp1 sp2 ->
                apply (binaryET Scale ElementDefault) $
                map buildFromPattern' [sp1, sp2]
            PDiv sp1 sp2 ->
                apply (binary Div) $ map buildFromPattern' [sp1, sp2]
            PSqrt sp -> apply (unary Sqrt) [buildFromPattern' sp]
            PSin sp -> apply (unary Sin) [buildFromPattern' sp]
            PCos sp -> apply (unary Cos) [buildFromPattern' sp]
            PTan sp -> apply (unary Tan) [buildFromPattern' sp]
            PExp sp -> apply (unary Exp) [buildFromPattern' sp]
            PLog sp -> apply (unary Log) [buildFromPattern' sp]
            PSinh sp -> apply (unary Sinh) [buildFromPattern' sp]
            PCosh sp -> apply (unary Cosh) [buildFromPattern' sp]
            PTanh sp -> apply (unary Tanh) [buildFromPattern' sp]
            PAsin sp -> apply (unary Asin) [buildFromPattern' sp]
            PAcos sp -> apply (unary Acos) [buildFromPattern' sp]
            PAtan sp -> apply (unary Atan) [buildFromPattern' sp]
            PAsinh sp -> apply (unary Asinh) [buildFromPattern' sp]
            PAcosh sp -> apply (unary Acosh) [buildFromPattern' sp]
            PAtanh sp -> apply (unary Atanh) [buildFromPattern' sp]
            PRealImag sp1 sp2 ->
                apply (binary RealImag) $ map buildFromPattern' [sp1, sp2]
            PRealPart sp -> apply (unary RealPart) [buildFromPattern' sp]
            PImagPart sp -> apply (unary ImagPart) [buildFromPattern' sp]
            PInnerProd sp1 sp2 ->
                apply (binaryET InnerProd ElementDefault `hasShape` []) $
                map buildFromPattern' [sp1, sp2]
            PPiecewise _ _ ->
                error
                    "Pattern piecewise appear on the right side of simplification rules which we haven't had yet"
