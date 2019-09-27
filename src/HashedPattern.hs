{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

-------------------------------------------------------------------------------
-- |
-- (c) 2014 Christopher Kumar Anand
-- Helper functions/instances to make pattern guards involving Expressions
-- easier to read.
--
-------------------------------------------------------------------------------
module HashedPattern where

import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.List.HT (splitLast, viewR)
import Data.Map (Map, union)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace (trace, traceShowId)
import HashedExpression
import HashedInner
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
    , sum
    , tan
    , tanh
    )
import Prelude (Bool)
import Prelude (Bool)

-- | Pattern for normalizier
--
type Capture = Int

type ListCapture = Int

type PowerCapture = Int

type RotateAmountCapture = Int

-- | List holes to captures many elements
--
data PatternList =
    PListHole (Pattern -> Pattern) ListCapture
    deriving (Show)

instance Show (Pattern -> Pattern) where
    show p = "(Pattern -> Pattern)"

-- | Pattern power to capture the alpha in x ^ alpha
--
data PatternPower
    = PPowerHole PowerCapture
    | PPowerConst Int
    | PPowerMul PatternPower PatternPower
    | PPowerSum PatternPower PatternPower
    deriving (Show)

-- | PatternRotateAmount to capture the amount in (rotate amount x)
--
data PatternRotateAmount
    = PRotateAmountHole RotateAmountCapture
    | PRotateAmountSum PatternRotateAmount PatternRotateAmount
    | PRotateAmountNegate PatternRotateAmount
    deriving (Show)

-- |
--
data Pattern
    = PHole Capture
    -- Ref to a node in the expression
    | PRef Int
    -- Ref to head of the list captures:
    | PHead PatternList
    | PSumList PatternList
    | PMulList PatternList
    | PConst Double
    | PScalarConst Double -- Pattern for scalar const used in RHS
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
    | PMulRest ListCapture [Pattern]
    | PSumRest ListCapture [Pattern]
    | PPower Pattern PatternPower
    | PRotate PatternRotateAmount Pattern
    | PReFT Pattern
    | PImFT Pattern
    | PTwiceReFT Pattern
    | PTwiceImFT Pattern
    deriving (Show)

-- |
--
infixl 7 ~*

infixl 6 ~+

class MulRestOp a b c | a b -> c where
    (~*) :: a -> b -> c

instance MulRestOp Pattern Pattern Pattern where
    (~*) (PMulRest listCapture ps) p = PMulRest listCapture (ps ++ [p])

class SumRestOp a b c | a b -> c where
    (~+) :: a -> b -> c

instance SumRestOp Pattern Pattern Pattern where
    (~+) (PSumRest listCapture ps) p = PSumRest listCapture (ps ++ [p])

instance SumRestOp Pattern PatternList PatternList where
    (~+) rest (PListHole fs listHole) = PListHole ((rest ~+) . fs) listHole

-- | Pattern
--
instance AddableOp Pattern where
    (+) wh1 wh2 = PSum [wh1, wh2]

instance NegateOp Pattern where
    negate = PNeg

instance MultiplyOp Pattern where
    (*) wh1 wh2 = PMul [wh1, wh2]

instance VectorSpaceOp Pattern Pattern where
    scale = PScale

instance NumOp Pattern where
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

instance ComplexRealOp Pattern Pattern where
    (+:) = PRealImag
    xRe = PRealPart
    xIm = PImagPart

instance InnerProductSpaceOp Pattern Pattern Pattern where
    (<.>) = PInnerProd

instance PowerOp Pattern PatternPower where
    (^) = PPower

instance RotateOp PatternRotateAmount Pattern where
    rotate = PRotate

-- | Pattern List
--
mapL :: (Pattern -> Pattern) -> PatternList -> PatternList
mapL f (PListHole fs listCapture) = PListHole (f . fs) listCapture

-- | Pattern Power
--
instance AddableOp PatternPower where
    (+) = PPowerMul

instance MultiplyOp PatternPower where
    (*) = PPowerMul

-- | Pattern Rotate Amount
--
instance AddableOp PatternRotateAmount where
    (+) = PRotateAmountSum

instance NegateOp PatternRotateAmount where
    negate = PRotateAmountNegate

-- | Discrete fourier transform
--
reFT :: Pattern -> Pattern
reFT = PReFT

imFT :: Pattern -> Pattern
imFT = PImFT

twiceReFT :: Pattern -> Pattern
twiceReFT = PTwiceReFT

twiceImFT :: Pattern -> Pattern
twiceImFT = PTwiceImFT

-- | Guarded patterns for normalizier
--
data GuardedPattern =
    GP Pattern Condition

type Substitution = (GuardedPattern, Pattern)

-- | Turn HashedPattern to a normalizier
--
fromSubstitution :: Substitution -> Modification
fromSubstitution pt@(GP pattern condition, replacementPattern) exp@(mp, n)
    | Just match <- match exp pattern
    , condition exp match = buildFromPattern exp match replacementPattern
    | otherwise = noChange n

-- | Helper to make pattern and replacement without condition
--
(|.~~~~~~>) :: Pattern -> Pattern -> Substitution
(|.~~~~~~>) pattern replacement =
    (GP pattern $ Prelude.const (Prelude.const True), replacement)

infix 0 |.~~~~~~>, ~~~~~~>

(~~~~~~>) :: GuardedPattern -> Pattern -> Substitution
(~~~~~~>) gPattern replacement = (gPattern, replacement)

(|.) :: Pattern -> Condition -> GuardedPattern
(|.) pattern condition = GP pattern condition

infixl 1 |.

type Condition = (ExpressionMap, Int) -> Match -> Bool

-- |
--
(&&.) :: Condition -> Condition -> Condition
(&&.) condition1 condition2 expr match =
    condition1 expr match && condition2 expr match

-- |
--
isNot :: Condition -> Condition
isNot condition expr match = not $ condition expr match

infixl 8 &&.

-- |
--
(||.) :: Condition -> Condition -> Condition
(||.) condition1 condition2 expr match =
    condition1 expr match || condition2 expr match

-- |
--
isScalar :: Pattern -> Condition
isScalar p exp match =
    let ExpressionDiff extraEntries newRootId = buildFromPattern exp match p
        originMp = fst exp
     in retrieveShape newRootId (IM.union extraEntries originMp) == []

-- |
--
isConst :: Pattern -> Condition
isConst p exp match =
    let ExpressionDiff extraEntries newRootId = buildFromPattern exp match p
        originMp = fst exp
     in case retrieveNode newRootId (IM.union extraEntries originMp) of
            Const _ -> True
            _ -> False

-- |
--
isNotConst :: Pattern -> Condition
isNotConst p exp match = not $ isConst p exp match

-- |
--
isReal :: Pattern -> Condition
isReal p exp match =
    let ExpressionDiff extraEntries newRootId = buildFromPattern exp match p
        originMp = fst exp
     in retrieveElementType newRootId (IM.union extraEntries originMp) == R

-- |
--
isComplex :: Pattern -> Condition
isComplex p exp match =
    let ExpressionDiff extraEntries newRootId = buildFromPattern exp match p
        originMp = fst exp
     in retrieveElementType newRootId (IM.union extraEntries originMp) == C

-- |
--
isCovector :: Pattern -> Condition
isCovector p exp match =
    let ExpressionDiff extraEntries newRootId = buildFromPattern exp match p
        originMp = fst exp
     in retrieveElementType newRootId (IM.union extraEntries originMp) ==
        Covector

-- |
--
sameElementType :: [Pattern] -> Condition
sameElementType ps exp match = allEqual . map getET $ ps
  where
    getET p =
        let ExpressionDiff extraEntries newRootId = buildFromPattern exp match p
            originMp = fst exp
         in retrieveElementType newRootId (IM.union extraEntries originMp)

-- |
--
allTheSame :: PatternList -> Condition
allTheSame pl@(PListHole _ listCapture) exp match
    | Just nIds <- Map.lookup listCapture . listCapturesMap $ match =
        allEqual nIds
    | otherwise = False

-- |
--
isDVar :: Pattern -> Condition
isDVar p exp match =
    let ExpressionDiff extraEntries newRootId = buildFromPattern exp match p
        originMp = fst exp
     in case retrieveNode newRootId (IM.union extraEntries originMp) of
            DVar _ -> True
            _ -> False

-- |
--
zeroAmount :: PatternRotateAmount -> Condition
zeroAmount pra exp match =
    let rotateAmount = buildFromPatternRotateAmount match pra
     in all (== 0) rotateAmount

-- |
--
sameAmount :: PatternRotateAmount -> PatternRotateAmount -> Condition
sameAmount pra1 pra2 exp match =
    let rotateAmount1 = buildFromPatternRotateAmount match pra1
        rotateAmount2 = buildFromPatternRotateAmount match pra2
     in rotateAmount1 == rotateAmount2

-- |
--
[p, q, r, s, t, u, v, w, x, y, z, condition] = map PHole [1 .. 12]

one :: Pattern
one = PConst 1

zero :: Pattern
zero = PConst 0

powerOne :: PatternPower
powerOne = PPowerConst 1

powerZero :: PatternPower
powerZero = PPowerConst 0

[alpha, beta, gamma] = map PPowerHole [1 .. 3]

scalarOne :: Pattern
scalarOne = PScalarConst 1

scalarZero :: Pattern
scalarZero = PScalarConst 0

scalar :: Double -> Pattern
scalar = PScalarConst

num :: Double -> Pattern
num = PConst

-- |
--
ys :: PatternList
ys = PListHole id 1

xs :: PatternList
xs = PListHole id 2

sum :: PatternList -> Pattern
sum = PSumList

product :: PatternList -> Pattern
product = PMulList

-- |
--
piecewise :: Pattern -> PatternList -> Pattern
piecewise = PPiecewise

branches :: PatternList
branches = PListHole id 2

headL :: PatternList -> Pattern
headL = PHead

-- |
--
restOfProduct :: Pattern
restOfProduct = PMulRest 239 []

restOfSum :: Pattern
restOfSum = PSumRest 2391 []

-- |
--
[amount, amount1, amount2, amount3] = map PRotateAmountHole [1 .. 4]

-- | Matches all nodes in the expression to see if they all match the PatternList, if they match, return
-- the inner actual nodes
-- e.g: matchList [a + (b * x), a + (b * y), a + (b * z)] (PatternList: (a + (b * _)) ---> Just [x, y, z]
--      matchList [x, y, z, t] (PatternList: (_)) = Just [x, y, z, t]
--      matchList [1 + x, 2 + x, y + x] (PatternList: (a + _)) = Nothing (not the same for all)
--
matchList :: ExpressionMap -> [Int] -> PatternList -> Maybe Match
matchList mp ns (PListHole fs listCapture)
    | all isJust maybeSubMatches
    , let subMatches = catMaybes maybeSubMatches
    , let unionSubMatches = foldl1 unionMatch subMatches
    , allEqual $ map otherCaptures subMatches =
        Just $
        unionMatch unionSubMatches $
        emptyMatch
            {listCapturesMap = Map.fromList [(listCapture, nIds subMatches)]}
    | otherwise = Nothing
  where
    uniqueCapture = minBound :: Int
    eachHole = PHole uniqueCapture
    pt = fs eachHole
    matchOne nId = match (mp, nId) pt
    maybeSubMatches = map matchOne ns
    otherCaptures = Map.delete uniqueCapture . capturesMap
    nIds subMatches =
        catMaybes . map (Map.lookup uniqueCapture . capturesMap) $ subMatches

-- | Match is the result we get when match an Expression against a Pattern
--
type NodeId = Int

type PowerValue = Int

data Match =
    Match
        { capturesMap :: Map Capture NodeId -- A given capture corresponds to a node id
        , listCapturesMap :: Map ListCapture [NodeId] -- A given list capture corresponds to a list of node ids
        , powerCapturesMap :: Map PowerCapture PowerValue -- A given power capture corresponds to a power value
        , rotateAmountCapturesMap :: Map RotateAmountCapture RotateAmount -- A given rotate amount capture corresponds to a rotate amount
        }
    deriving (Show)

emptyMatch :: Match
emptyMatch = Match Map.empty Map.empty Map.empty Map.empty

unionMatch :: Match -> Match -> Match
unionMatch match1 match2 =
    Match
        (capturesMap match1 `union` capturesMap match2)
        (listCapturesMap match1 `union` listCapturesMap match2)
        (powerCapturesMap match1 `union` powerCapturesMap match2)
        (rotateAmountCapturesMap match1 `union` rotateAmountCapturesMap match2)

-- | Match an expression with a pattern, return the map between capture hole to the actual node
-- e.g: match (Expression: (a(3243) + b(32521)) (PatternNormal:(x(1) + y(2)) --> ({1 -> 3243, 2 -> 32521}, {})
--      match (Expression sum(a(3243), b(32521), c(21321)) (PatternNormal:(sum(each(1))) --> ({}, {1 -> [3243, 32521, 21321]})
match :: (ExpressionMap, Int) -> Pattern -> Maybe Match
match (mp, n) outerWH =
    let unionBoth (x1, y1) (x2, y2) = (x1 `union` x2, y1 `union` y2)
        catMatch = foldl unionMatch emptyMatch
        recursiveAndCombine :: [Arg] -> [Pattern] -> Maybe Match
        recursiveAndCombine args whs
            | length args == length whs
            , let subMatches = zipWith match (map (mp, ) args) whs
            , all isJust subMatches = Just . catMatch . catMaybes $ subMatches
            | otherwise = Nothing
     in case (retrieveNode n mp, outerWH) of
            (_, PHole capture) ->
                Just $ emptyMatch {capturesMap = Map.fromList [(capture, n)]}
            (Const c, PConst whc)
                | c == whc -> Just emptyMatch
            (Sum _ args, PSum whs) -> recursiveAndCombine args whs
            (Sum _ args, PSumList pl@(PListHole _ listCapture)) ->
                matchList mp args pl
            (Sum _ args, PSumRest listCapture ps)
                | length args > length ps
                , let (rest, normalParts) =
                          splitAt (length args - length ps) args
                , length normalParts == length ps
                , Just matchNormalParts <- recursiveAndCombine normalParts ps
                , let matchListPart =
                          emptyMatch
                              { listCapturesMap =
                                    Map.fromList [(listCapture, rest)]
                              } ->
                    Just $ unionMatch matchNormalParts matchListPart
            (Mul _ args, PMul whs) -> recursiveAndCombine args whs
            (Mul _ args, PMulList pl@(PListHole _ listCapture)) ->
                matchList mp args pl
            (Mul _ args, PMulRest listCapture ps)
                | length args > (length ps)
                , let (rest, normalParts) =
                          splitAt (length args - length ps) args
                , length normalParts == length ps
                , Just matchNormalParts <- recursiveAndCombine normalParts ps
                , let matchListPart =
                          emptyMatch
                              { listCapturesMap =
                                    Map.fromList [(listCapture, rest)]
                              } ->
                    Just $ unionMatch matchNormalParts matchListPart
            (Neg _ arg, PNeg sp) -> recursiveAndCombine [arg] [sp]
            (Scale _ arg1 arg2, PScale sp1 sp2) ->
                recursiveAndCombine [arg1, arg2] [sp1, sp2]
            (Div arg1 arg2, PDiv sp1 sp2) ->
                recursiveAndCombine [arg1, arg2] [sp1, sp2]
            (Sqrt arg, PSqrt sp) -> recursiveAndCombine [arg] [sp]
            (Sin arg, PSin sp) -> recursiveAndCombine [arg] [sp]
            (Cos arg, PCos sp) -> recursiveAndCombine [arg] [sp]
            (Tan arg, PTan sp) -> recursiveAndCombine [arg] [sp]
            (Exp arg, PExp sp) -> recursiveAndCombine [arg] [sp]
            (Log arg, PLog sp) -> recursiveAndCombine [arg] [sp]
            (Sinh arg, PSinh sp) -> recursiveAndCombine [arg] [sp]
            (Cosh arg, PCosh sp) -> recursiveAndCombine [arg] [sp]
            (Tanh arg, PTanh sp) -> recursiveAndCombine [arg] [sp]
            (Asin arg, PAsin sp) -> recursiveAndCombine [arg] [sp]
            (Acos arg, PAcos sp) -> recursiveAndCombine [arg] [sp]
            (Atan arg, PAtan sp) -> recursiveAndCombine [arg] [sp]
            (Asinh arg, PAsinh sp) -> recursiveAndCombine [arg] [sp]
            (Acosh arg, PAcosh sp) -> recursiveAndCombine [arg] [sp]
            (Atanh arg, PAtanh sp) -> recursiveAndCombine [arg] [sp]
            (RealImag arg1 arg2, PRealImag sp1 sp2) ->
                recursiveAndCombine [arg1, arg2] [sp1, sp2]
            (RealPart arg, PRealPart sp) -> recursiveAndCombine [arg] [sp]
            (ImagPart arg, PImagPart sp) -> recursiveAndCombine [arg] [sp]
            (InnerProd _ arg1 arg2, PInnerProd sp1 sp2) ->
                recursiveAndCombine [arg1, arg2] [sp1, sp2]
            (Piecewise _ conditionArg branchArgs, PPiecewise sp pl@(PListHole _ listCapture))
                | Just matchBranches <- matchList mp branchArgs pl
                , Just matchCondition <- recursiveAndCombine [conditionArg] [sp] ->
                    Just $ unionMatch matchCondition matchBranches
            (Power x arg, PPower sp (PPowerConst val))
                | fromIntegral x == val -> recursiveAndCombine [arg] [sp]
            (Power x arg, PPower sp (PPowerHole powerCapture))
                | Just matchInner <- recursiveAndCombine [arg] [sp]
                , let matchPower =
                          emptyMatch
                              { powerCapturesMap =
                                    Map.fromList [(powerCapture, x)]
                              } -> Just $ unionMatch matchInner matchPower
            (Rotate ra arg, PRotate (PRotateAmountHole rotateAmountCapture) sp)
                | Just matchInner <- recursiveAndCombine [arg] [sp]
                , let matchRotateAmount =
                          emptyMatch
                              { rotateAmountCapturesMap =
                                    Map.fromList [(rotateAmountCapture, ra)]
                              } ->
                    Just $ unionMatch matchInner matchRotateAmount
            (ReFT arg, PReFT sp) -> recursiveAndCombine [arg] [sp]
            (ImFT arg, PImFT sp) -> recursiveAndCombine [arg] [sp]
            (TwiceReFT arg, PTwiceReFT sp) -> recursiveAndCombine [arg] [sp]
            (TwiceImFT arg, PTwiceImFT sp) -> recursiveAndCombine [arg] [sp]
            _ -> Nothing

-- |
--
turnToPattern :: (Pattern -> Pattern) -> Int -> Pattern
turnToPattern fs nId = fs $ PRef nId

-- |
--
buildFromPatternPower :: Match -> PatternPower -> PowerValue
buildFromPatternPower match pp =
    case pp of
        PPowerHole powerCapture
            | Just val <- Map.lookup powerCapture (powerCapturesMap match) ->
                val
        PPowerConst val -> val
        PPowerMul pp1 pp2 ->
            (buildFromPatternPower match pp1) *
            (buildFromPatternPower match pp2)
        PPowerSum pp1 pp2 ->
            (buildFromPatternPower match pp1) +
            (buildFromPatternPower match pp2)

-- |
--
buildFromPatternRotateAmount :: Match -> PatternRotateAmount -> RotateAmount
buildFromPatternRotateAmount match pra =
    case pra of
        PRotateAmountHole rotateAmountCapture
            | Just am <-
                 Map.lookup rotateAmountCapture (rotateAmountCapturesMap match) ->
                am
        PRotateAmountSum pra1 pra2 ->
            zipWith
                (+)
                (buildFromPatternRotateAmount match pra1)
                (buildFromPatternRotateAmount match pra2)
        PRotateAmountNegate pra ->
            map negate (buildFromPatternRotateAmount match pra)

-- |
--
buildFromPatternList ::
       (ExpressionMap, Int) -> Match -> PatternList -> [ExpressionDiff]
buildFromPatternList exp match (PListHole fs listCapture)
    | Just ns <- Map.lookup listCapture (listCapturesMap match) =
        map (buildFromPattern exp match . turnToPattern fs) ns
    | otherwise =
        error
            "ListCapture not in the Map ListCapture [Int] which should never happens"

-- |
--
buildFromPattern :: (ExpressionMap, Int) -> Match -> Pattern -> ExpressionDiff
buildFromPattern exp@(originalMp, originalN) match = buildFromPattern'
  where
    applyDiff' = applyDiff originalMp
    buildFromPattern' :: Pattern -> ExpressionDiff
    buildFromPattern' pattern =
        case pattern of
            PRef nId -> noChange nId
            PHole capture
                | Just nId <- Map.lookup capture (capturesMap match) ->
                    noChange nId
                | otherwise ->
                    error
                        "Capture not in the Map Capture Int which should never happens"
            PHead pl -> head $ buildFromPatternList exp match pl
            PConst val -> diffConst (retrieveShape originalN originalMp) val
            PScalarConst val -> diffConst [] val
            PSumList ptl ->
                sumManyDiff originalMp . buildFromPatternList exp match $ ptl
            PMulList ptl ->
                mulManyDiff originalMp . buildFromPatternList exp match $ ptl
            PSum sps -> sumManyDiff originalMp . map buildFromPattern' $ sps
            PMul sps -> mulManyDiff originalMp . map buildFromPattern' $ sps
            PNeg sp ->
                applyDiff' (unaryET Neg ElementDefault) [buildFromPattern' sp]
            PScale sp1 sp2 ->
                applyDiff' (binaryET Scale ElementDefault) $
                map buildFromPattern' [sp1, sp2]
            PDiv sp1 sp2 ->
                applyDiff' (binary Div) $ map buildFromPattern' [sp1, sp2]
            PSqrt sp -> applyDiff' (unary Sqrt) [buildFromPattern' sp]
            PSin sp -> applyDiff' (unary Sin) [buildFromPattern' sp]
            PCos sp -> applyDiff' (unary Cos) [buildFromPattern' sp]
            PTan sp -> applyDiff' (unary Tan) [buildFromPattern' sp]
            PExp sp -> applyDiff' (unary Exp) [buildFromPattern' sp]
            PLog sp -> applyDiff' (unary Log) [buildFromPattern' sp]
            PSinh sp -> applyDiff' (unary Sinh) [buildFromPattern' sp]
            PCosh sp -> applyDiff' (unary Cosh) [buildFromPattern' sp]
            PTanh sp -> applyDiff' (unary Tanh) [buildFromPattern' sp]
            PAsin sp -> applyDiff' (unary Asin) [buildFromPattern' sp]
            PAcos sp -> applyDiff' (unary Acos) [buildFromPattern' sp]
            PAtan sp -> applyDiff' (unary Atan) [buildFromPattern' sp]
            PAsinh sp -> applyDiff' (unary Asinh) [buildFromPattern' sp]
            PAcosh sp -> applyDiff' (unary Acosh) [buildFromPattern' sp]
            PAtanh sp -> applyDiff' (unary Atanh) [buildFromPattern' sp]
            PRealImag sp1 sp2 ->
                applyDiff' (binary RealImag) $ map buildFromPattern' [sp1, sp2]
            PRealPart sp -> applyDiff' (unary RealPart) [buildFromPattern' sp]
            PImagPart sp -> applyDiff' (unary ImagPart) [buildFromPattern' sp]
            PInnerProd sp1 sp2 ->
                applyDiff' (binaryET InnerProd ElementDefault `hasShape` []) $
                map buildFromPattern' [sp1, sp2]
            PPiecewise _ _ ->
                error
                    "Pattern piecewise appear on the right side of normalizier rules which we haven't had yet"
            PMulRest restCapture sps
                | Just ns <- Map.lookup restCapture (listCapturesMap match) ->
                    mulManyDiff originalMp $
                    (map noChange ns) ++ map (buildFromPattern') sps
            PSumRest restCapture sps
                | Just ns <- Map.lookup restCapture (listCapturesMap match) ->
                    sumManyDiff originalMp $
                    (map noChange $ ns) ++ map (buildFromPattern') sps
            PPower sp pp ->
                let val = buildFromPatternPower match pp
                 in applyDiff' (unary (Power val)) [buildFromPattern' sp]
            PRotate pra sp ->
                let rotateAmount = buildFromPatternRotateAmount match pra
                 in applyDiff'
                        (unary (Rotate rotateAmount))
                        [buildFromPattern' sp]
            PReFT sp -> applyDiff' (unary ReFT) [buildFromPattern' sp]
            PImFT sp -> applyDiff' (unary ImFT) [buildFromPattern' sp]
            PTwiceReFT sp -> applyDiff' (unary TwiceReFT) [buildFromPattern' sp]
            PTwiceImFT sp -> applyDiff' (unary TwiceImFT) [buildFromPattern' sp]
            _ ->
                error
                    "The right hand-side of substitution has something that we don't support yet"
