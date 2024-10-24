{-# OPTIONS_GHC -Wno-missing-methods #-}

-- |
-- Module      :  HashedExpression.Internal.Pattern
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- Helper functions/instances to make pattern guards involving Expressions easier to read.
-- Traditional pattern matching on a 'TypedExpr' is difficult because 'Node' arguments are in hashed form, use this
-- data type to perform a special kind of pattern matching that you can use to build a 'Substitution' and subsequently a
-- 'Transformation', for example
--  @
module HashedExpression.Internal.Pattern
  ( -- * Substitution
    Substitution,
    fromSubstitution,
    (~~>),
    (|.~~>),
    (|.),

    -- * Match
    Match (..),
    match,
    matchList,
    matchAll,

    -- * Patterns
    Pattern (..),
    GuardedPattern (..),
    PatternPower (..),
    PatternRotateAmount (..),
    Capture,
    MulRestOp (..),
    SumRestOp (..),
    restOfProduct,
    restOfSum,
    pvariable,
    phole,
    pconstant,

    -- * PatternList
    PatternList (..),
    mapL,
    headL,
    sumP,
    productP,
    branches,
    piecewise_,

    -- * Conditions
    Condition,
    isNot,
    (&&.),
    (||.),
    allTheSame,
    isScalar,
    isReal,
    isNotConst,
    isComplex,
    sameElementType,
    zeroAmount,
    sameAmount,
  )
where

import Control.Monad.State.Strict
import Data.Map (Map, union)
import qualified Data.Map.Strict as Map
import Data.Maybe
import HashedExpression.Internal
import HashedExpression.Internal.Base
import HashedExpression.Internal.MonadExpression
import HashedExpression.Internal.Node
import HashedExpression.Internal.Rewrite
import HashedExpression.Utils
import Prelude hiding ((^))

-- --------------------------------------------------------------------------------------------------------------------

-- * Substitution

-- --------------------------------------------------------------------------------------------------------------------

-- | A Substitution matches a pattern that fulfills a condition (provided via a 'GuardedPattern'), and replaces it with a different pattern
--   (provided via 'Pattern')
type Substitution = (GuardedPattern, Pattern)

fromSubstitution :: Substitution -> Modification
fromSubstitution pt@(GP pattern condition, replacementPattern) n = withExpressionMap $ \mp ->
  let exp = (mp, n)
   in case match exp pattern of
        Just match | condition exp match -> buildFromPattern (Just $ getShape exp) match replacementPattern
        _ -> return n

-- | Create a 'Substitution' that matches a 'Pattern' (automatically converting into a 'GuardedPattern' that's always true) and
--   replaces it with another 'Pattern'
(|.~~>) ::
  -- | find match
  Pattern ->
  -- | replacement
  Pattern ->
  -- | combined result
  Substitution
(|.~~>) pattern replacement =
  (GP pattern $ const (const True), replacement)

-- | Create a 'Substitution' that matches a 'GuardedPattern' (a 'Pattern' that only matches upon fulfilling a condition) and
--   replaces it with another 'Pattern'
(~~>) ::
  -- | find a match
  GuardedPattern ->
  -- | replacement
  Pattern ->
  -- | combined result
  Substitution
(~~>) gPattern replacement = (gPattern, replacement)

infix 0 |.~~>, ~~>

-- | Turn a 'Pattern' into a 'GuardedPattern' (so it only matches upon fulfilling a condition)
(|.) ::
  -- | original pattern
  Pattern ->
  -- | condition to be fulfilled
  Condition ->
  -- | combined result
  GuardedPattern
(|.) pattern condition = GP pattern condition

infixl 1 |.

-- --------------------------------------------------------------------------------------------------------------------

-- * Patterns

-- --------------------------------------------------------------------------------------------------------------------

-- | This data type contains constructors for representing different patterns a 'Node' in a 'TypedExpr' may contain.
--   Match any 'Node' to a hole using 'PHole', to distinguish between holes each must be given a unique identifier (i.e 'Capture')
--   TODO haddock: why do we have some ops wrapping PatternList and the same ops with [Pattern]
data Pattern
  = -- | hole with a identifier (i.e 'Capture')
    PHole Capture
  | -- | direct reference to a node in the expression
    PRef NodeID
  | -- | reference to the head of a 'PatternList'
    PHead PatternList
  | -- | sum via 'PatternList'
    PSumList PatternList
  | -- | multiply via 'PatternList'
    PMulList PatternList
  | -- | variable
    PVar String
  | -- | constant
    PConst Double
  | -- | summation operator
    PSum [Pattern]
  | -- | multiplication operator
    PMul [Pattern]
  | -- | negation operator
    PNeg Pattern
  | -- | scale a Pattern by another Pattern
    PScale Pattern Pattern
  | -- | division
    PDiv Pattern Pattern
  | -- | square Root operator
    PSqrt Pattern
  | -- | sin operator
    PSin Pattern
  | -- | cos operator
    PCos Pattern
  | -- | tan operator
    PTan Pattern
  | -- | exp operator
    PExp Pattern
  | -- | log operator
    PLog Pattern
  | -- | sinh operator
    PSinh Pattern
  | -- | cosh operator
    PCosh Pattern
  | -- | tanh operator
    PTanh Pattern
  | -- | asin operator
    PAsin Pattern
  | -- | acos operator
    PAcos Pattern
  | -- | atan operator
    PAtan Pattern
  | -- | asinh operator
    PAsinh Pattern
  | -- | acosh operator
    PAcosh Pattern
  | -- | atanh operator
    PAtanh Pattern
  | -- | pattern inside real and imaginary parts of a complex number
    PRealImag Pattern Pattern
  | -- | pattern that has a real part extraction operator applied to it
    PRealPart Pattern
  | -- | pattern that has a imaginary part extraction operator applied to it
    PImagPart Pattern
  | -- |
    PConjugate Pattern
  | -- | pattern that has a inner product operator applied to it
    PInnerProd Pattern Pattern
  | -- | pattern that has a piecewise
    PPiecewise Pattern PatternList
  | -- | a hole that is the rest of a multiplication
    PMulRest Capture [Pattern]
  | -- | a hole taht is the rest of a summation
    PSumRest Capture [Pattern]
  | -- | pattern that has a power operator with a 'PatternPower' applied to it
    PPower Pattern PatternPower
  | -- | pattern that has a rotate operator with a 'PatternRotateAmount' applied to it
    PRotate PatternRotateAmount Pattern
  deriving (Show)

instance Show (Pattern -> Pattern) where
  show p = "(Pattern -> Pattern)"

-- | Auxiliary functions for create pattern holes
phole :: Capture -> Pattern
phole c = PHole c

-- | Auxiliary functions for create variable patterns
pvariable :: String -> Pattern
pvariable lbl = PVar lbl

-- | Auxiliary functions for create variable patterns
pconstant :: Double -> Pattern
pconstant c = PConst c

-- | A 'Pattern' that only matches when it fullfills a 'Condition'
data GuardedPattern
  = GP Pattern Condition

-- | Pattern power to capture the alpha in x ^ alpha
data PatternPower
  = PPowerHole Capture
  | PPowerConst Int
  | PPowerMul PatternPower PatternPower
  | PPowerSum PatternPower PatternPower
  deriving (Show)

-- | PatternRotateAmount to capture the amount x in (rotate amount x)
data PatternRotateAmount
  = PRotateAmountHole Capture
  | PRotateAmountSum PatternRotateAmount PatternRotateAmount
  | PRotateAmountNegate PatternRotateAmount
  deriving (Show)

-- | 'Pattern' holes are identified uniquely by a Capture id
type Capture = Int

-- | Predefined hole for capturing the tail of a product (with hardcoded 'Capture' id 239)
restOfProduct :: Pattern
restOfProduct = PMulRest 239 []

-- | Predefined hole for capturing the tail of a summation (with hardcoded 'Capture' id 2391)
restOfSum :: Pattern
restOfSum = PSumRest 2391 []

infixl 7 ~*

-- | Create holes that are the tail of a series of multiplications
class MulRestOp a b c | a b -> c where
  (~*) :: a -> b -> c

instance MulRestOp Pattern Pattern Pattern where
  (~*) (PMulRest listCapture ps) p = PMulRest listCapture (ps ++ [p])

infixl 6 ~+

-- | Create holes that are the tail of a series of summation
class SumRestOp a b c | a b -> c where
  (~+) :: a -> b -> c

instance SumRestOp Pattern Pattern Pattern where
  (~+) (PSumRest listCapture ps) p = PSumRest listCapture (ps ++ [p])

instance SumRestOp Pattern PatternList PatternList where
  (~+) rest (PListHole fs listHole) = PListHole ((rest ~+) . fs) listHole

instance Num Pattern where
  (+) wh1 wh2 = PSum [wh1, wh2]
  negate = PNeg
  (*) wh1 wh2 = PMul [wh1, wh2]
  fromInteger = PConst . fromInteger

instance Fractional Pattern where
  (/) = PDiv
  fromRational = PConst . fromRational

instance ScaleOp Pattern Pattern where
  scale = PScale

instance Floating Pattern where
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

instance ComplexRealOp Pattern Pattern where
  (+:) = PRealImag
  xRe = PRealPart
  xIm = PImagPart
  conjugate = PConjugate

instance InnerProductSpaceOp Pattern Pattern where
  (<.>) = PInnerProd

instance PowerOp Pattern PatternPower where
  (^) = PPower

instance RotateOp PatternRotateAmount Pattern where
  rotate = PRotate

instance Num PatternPower where
  fromInteger = PPowerConst . fromInteger
  (+) = PPowerMul
  (*) = PPowerMul

instance Num PatternRotateAmount where
  (+) = PRotateAmountSum
  negate = PRotateAmountNegate

-- --------------------------------------------------------------------------------------------------------------------

-- * PatternList

-- --------------------------------------------------------------------------------------------------------------------

-- | Capture a list of associated 'Pattern'
--   NT: the functional parameters is for capturing transformation happens to all the element in the captured list
--   E.g:  `sumP (mapL (* y) xs)` match with (f * g + h * g) gives us {xs -> [f , h], y -> g}
data PatternList
  = PListHole (Pattern -> Pattern) Capture
  deriving (Show)

-- | Map a function over a 'PatternList'
mapL :: (Pattern -> Pattern) -> PatternList -> PatternList
mapL f (PListHole fs listCapture) = PListHole (f . fs) listCapture

-- | Take the head of a 'PatternList'
headL :: PatternList -> Pattern
headL = PHead

-- | Product operation over 'PatternList'
productP :: PatternList -> Pattern
productP = PMulList

-- | Summation operation over 'PatternList'
sumP :: PatternList -> Pattern
sumP = PSumList

-- | Piecewise function over a 'PatternList'
piecewise_ :: Pattern -> PatternList -> Pattern
piecewise_ = PPiecewise

-- | Branch over a 'PatternList'
branches :: PatternList
branches = PListHole id 2

-- --------------------------------------------------------------------------------------------------------------------

-- * Conditions

-- --------------------------------------------------------------------------------------------------------------------

-- | Check if the match satisfy some properties so that rewrite can happen
type Condition =
  -- | The matched expression
  RawExpr ->
  -- | The match
  Match ->
  -- | Whether this match satisfy the condition
  Bool

infixl 8 &&.

infixl 8 ||.

-- | 'Condition' combinator, Logical Or
(||.) :: Condition -> Condition -> Condition
(||.) condition1 condition2 expr match =
  condition1 expr match || condition2 expr match

-- | 'Condition' combinator, Logical And
(&&.) :: Condition -> Condition -> Condition
(&&.) condition1 condition2 expr match =
  condition1 expr match && condition2 expr match

-- | 'Condition' combinator, Logical Negation
isNot :: Condition -> Condition
isNot condition expr match = not $ condition expr match

-- | Returns True iff all captures in a 'PatternList' are the same 'NodeID'
allTheSame :: PatternList -> Condition
allTheSame pl@(PListHole _ listCapture) exp match
  | Just nIds <- Map.lookup listCapture . listCapturesMap $ match =
    allEqual nIds
  | otherwise = False

-- | Returns True iff the 'Pattern' capture is a 'Scalar'
isScalar :: Pattern -> Condition
isScalar p exp match =
  let (mp, pNodeID) = runRewrite (buildFromPattern Nothing match p) $ fst exp
   in retrieveShape pNodeID mp == []

-- | Returns True iff the 'Pattern' capture is a 'Scalar'
isConst :: Pattern -> Condition
isConst p exp match =
  let (mp, pNodeID) = runRewrite (buildFromPattern Nothing match p) $ fst exp
   in case retrieveOp pNodeID mp of
        Const _ -> True
        _ -> False

-- | Returns True iff the 'Pattern' captured is NOT a 'Const'
isNotConst :: Pattern -> Condition
isNotConst p exp match = not $ isConst p exp match

-- | Returns True iff the 'Pattern' captured has a Real (i.e 'R') 'ElementType'
isReal :: Pattern -> Condition
isReal p exp match =
  let (mp, pNodeID) = runRewrite (buildFromPattern Nothing match p) $ fst exp
   in retrieveElementType pNodeID mp == R

-- | Returns True iff the 'Pattern' captured has a Complex (i.e 'C') 'ElementType'
isComplex :: Pattern -> Condition
isComplex p exp match =
  let (mp, pNodeID) = runRewrite (buildFromPattern Nothing match p) $ fst exp
   in retrieveElementType pNodeID mp == C

-- | Returns True iff all the 'Pattern' captures have a the same 'ElementType'
sameElementType :: [Pattern] -> Condition
sameElementType ps exp match = allEqual . map getET $ ps
  where
    getET p =
      let (mp, pNodeID) = runRewrite (buildFromPattern Nothing match p) $ fst exp
       in retrieveElementType pNodeID mp

-- | Returns True iff the 'PatternRotateAmount' captured has a value of 0
zeroAmount :: PatternRotateAmount -> Condition
zeroAmount pra exp match =
  let rotateAmount = buildFromPatternRotateAmount match pra
   in all (== 0) rotateAmount

-- | Returns True iff both 'PatternRotateAmount' captured have the same value
sameAmount :: PatternRotateAmount -> PatternRotateAmount -> Condition
sameAmount pra1 pra2 exp match =
  let rotateAmount1 = buildFromPatternRotateAmount match pra1
      rotateAmount2 = buildFromPatternRotateAmount match pra2
   in rotateAmount1 == rotateAmount2

-- --------------------------------------------------------------------------------------------------------------------

-- * Matching Internals

-- --------------------------------------------------------------------------------------------------------------------

-- | Matches all nodes in the expression to see if they all match the PatternList, if they match, return
--   the inner actual nodes, e.g
--
-- @
--   matchList [a + (b * x), a + (b * y), a + (b * z)] (PatternList: (a + (b * _)) ---> Just [x, y, z]
--   matchList [x, y, z, t] (PatternList: (_)) = Just [x, y, z, t]
--   matchList [1 + x, 2 + x, y + x] (PatternList: (a + _)) = Nothing (not the same for all)
-- @
-- TODO haddock: the capture is always minBound??? this has to be an issue
-- TODO: the minBound trick is to avoid other capture, NT: probably should introduce a capture for PatternList as well
matchList ::
  -- | from base 'TypedExpr'
  ExpressionMap ->
  -- | List of expressions
  [NodeID] ->
  -- | patterns to match to
  PatternList ->
  -- | potentially a 'Match' with a filled 'listCapturesMap' attribute
  Maybe Match
matchList mp ns (PListHole fs listCapture)
  | all isJust maybeSubMatches,
    let subMatches = catMaybes maybeSubMatches,
    let unionSubMatches = foldl1 unionMatch subMatches,
    allEqual $ map otherCaptures subMatches =
    Just $
      unionMatch unionSubMatches $
        emptyMatch
          { listCapturesMap = Map.fromList [(listCapture, nIds subMatches)]
          }
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

-- | the 'Int' value n in any expression x^n
type PowerValue = Int

-- | A wrapper for different 'Map' that associate a capture (i.e hole identifier) to matched 'NodeID'. The function 'match' will
--   return this type if it succesfully matches a 'TypedExpr' to a 'Pattern' by locating 'NodeID' that correspond to the holes in
--   a pattern. For example, 'PHole' wraps a 'Int' identifier, so we need a 'Map' that associates those identifiers to 'NodeID'
data Match = Match
  { -- | Associates 'PHole' identifiers to corresponding matched 'NodeID'
    capturesMap :: Map Capture NodeID,
    -- | Associates 'PListHole' identifiers to corresponding matched 'NodeID'
    listCapturesMap :: Map Capture [NodeID],
    -- | Associates 'PPowerHole' identifiers to corresponding matched 'NodeID'
    powerCapturesMap :: Map Capture PowerValue,
    -- | Associates 'PPowerHole' identifiers to corresponding matched 'NodeID'
    rotateAmountCapturesMap :: Map Capture RotateAmount
  }
  deriving (Show)

-- | A 'Match' data constructor with all empty 'Map' for entries
emptyMatch :: Match
emptyMatch = Match Map.empty Map.empty Map.empty Map.empty

-- | Take the union of each of the 'Map' wrapped by a 'Match' data constructor
unionMatch :: Match -> Match -> Match
unionMatch match1 match2 =
  Match
    (capturesMap match1 `union` capturesMap match2)
    (listCapturesMap match1 `union` listCapturesMap match2)
    (powerCapturesMap match1 `union` powerCapturesMap match2)
    (rotateAmountCapturesMap match1 `union` rotateAmountCapturesMap match2)

-- | Match an expression with a pattern, return the map between capture hole to the actual node
-- e.g: match (TypedExpr: (a(3243) + b(32521)) (PatternNormal:(x(1) + y(2)) --> ({1 -> 3243, 2 -> 32521}, {})
--      match (TypedExpr sum(a(3243), b(32521), c(21321)) (PatternNormal:(sum(each(1))) --> ({}, {1 -> [3243, 32521, 21321]})
match :: RawExpr -> Pattern -> Maybe Match
match (mp, n) outerWH =
  let catMatch = foldl unionMatch emptyMatch
      recursiveAndCombine :: [Arg] -> [Pattern] -> Maybe Match
      recursiveAndCombine args whs
        | length args == length whs,
          let subMatches = zipWith match (map (mp,) args) whs,
          all isJust subMatches =
          Just . catMatch . catMaybes $ subMatches
        | otherwise = Nothing
   in case (retrieveOp n mp, outerWH) of
        (_,PRef nID)
          | n == nID -> Just emptyMatch
        (_, PHole capture) ->
          Just $ emptyMatch {capturesMap = Map.fromList [(capture, n)]}
        (Var s, PVar whs)
          | s == whs -> Just emptyMatch
        (Const c, PConst whc)
          | c == whc -> Just emptyMatch
        (Sum args, PSum whs) -> recursiveAndCombine args whs
        (Sum args, PSumList pl) ->
          matchList mp args pl
        (Sum args, PSumRest listCapture ps)
          | length args > length ps,
            let (rest, normalParts) = splitAt (length args - length ps) args,
            length normalParts == length ps,
            Just matchNormalParts <- recursiveAndCombine normalParts ps,
            let matchListPart =
                  emptyMatch
                    { listCapturesMap =
                        Map.fromList [(listCapture, rest)]
                    } ->
            Just $ unionMatch matchNormalParts matchListPart
        (Mul args, PMul whs) -> recursiveAndCombine args whs
        (Mul args, PMulList pl) ->
          matchList mp args pl
        (Mul args, PMulRest listCapture ps)
          | length args > (length ps),
            let (rest, normalParts) =
                  splitAt (length args - length ps) args,
            length normalParts == length ps,
            Just matchNormalParts <- recursiveAndCombine normalParts ps,
            let matchListPart =
                  emptyMatch
                    { listCapturesMap =
                        Map.fromList [(listCapture, rest)]
                    } ->
            Just $ unionMatch matchNormalParts matchListPart
        (Neg arg, PNeg sp) -> recursiveAndCombine [arg] [sp]
        (Scale arg1 arg2, PScale sp1 sp2) ->
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
        (InnerProd arg1 arg2, PInnerProd sp1 sp2) ->
          recursiveAndCombine [arg1, arg2] [sp1, sp2]
        (Piecewise _ conditionArg branchArgs, PPiecewise sp pl@(PListHole _ listCapture))
          | Just matchBranches <- matchList mp branchArgs pl,
            Just matchCondition <- recursiveAndCombine [conditionArg] [sp] ->
            Just $ unionMatch matchCondition matchBranches
        (Power x arg, PPower sp (PPowerConst val))
          | fromIntegral x == val -> recursiveAndCombine [arg] [sp]
        (Power x arg, PPower sp (PPowerHole powerCapture))
          | Just matchInner <- recursiveAndCombine [arg] [sp],
            let matchPower =
                  emptyMatch
                    { powerCapturesMap =
                        Map.fromList [(powerCapture, x)]
                    } ->
            Just $ unionMatch matchInner matchPower
        (Rotate ra arg, PRotate (PRotateAmountHole rotateAmountCapture) sp)
          | Just matchInner <- recursiveAndCombine [arg] [sp],
            let matchRotateAmount =
                  emptyMatch
                    { rotateAmountCapturesMap =
                        Map.fromList [(rotateAmountCapture, ra)]
                    } ->
            Just $ unionMatch matchInner matchRotateAmount
        _ -> Nothing

-- | Attempt to match an expression with a pattern, at all nodes of the
-- expression. Returns a list of all matches
-- NOTE costly on large expressions
matchAll :: RawExpr -> Pattern -> [Match]
matchAll (mp,n) outerWH =
  let
    nIDs = nodeIDs mp
  in catMaybes $ map (\n' -> match (mp,n') outerWH) nIDs

-- | Turn a 'Pattern' transformation into a 'Pattern' reference
turnToPattern :: (Pattern -> Pattern) -> NodeID -> Pattern
turnToPattern fs nId = fs $ PRef nId

-- | Find a 'PowerValue' corresponding to a 'PatternPower' that was already
--   found in a 'Match'
buildFromPatternPower :: Match -> PatternPower -> PowerValue
buildFromPatternPower match pp =
  case pp of
    PPowerHole powerCapture
      | Just val <- Map.lookup powerCapture (powerCapturesMap match) ->
        val
    PPowerConst val -> val
    PPowerMul pp1 pp2 ->
      (buildFromPatternPower match pp1)
        * (buildFromPatternPower match pp2)
    PPowerSum pp1 pp2 ->
      (buildFromPatternPower match pp1)
        + (buildFromPatternPower match pp2)

-- | Find a 'RotateAmount' corresponding to a 'PatternRotateAmount' that was already
--   found in a 'Match'
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

buildFromPatternList ::
  Maybe Shape -> Match -> PatternList -> [Rewrite NodeID]
buildFromPatternList inferredShape match (PListHole fs listCapture)
  | Just ns <- Map.lookup listCapture (listCapturesMap match) =
    map (buildFromPattern inferredShape match . turnToPattern fs) ns
  | otherwise =
    error "Capture not in the Map Capture [Int] which should never happens"

buildFromPattern :: Maybe Shape -> Match -> Pattern -> Rewrite NodeID
buildFromPattern inferredShape match = build inferredShape
  where
    build :: Maybe Shape -> Pattern -> Rewrite NodeID
    build inferredShape pattern =
      case pattern of
        PRef nId -> just nId
        PHole capture
          | Just nId <- Map.lookup capture (capturesMap match) ->
            just nId
          | otherwise ->
            error "Capture not in the Map Capture Int which should never happen"
        PHead pl -> head $ buildFromPatternList inferredShape match pl
        PConst val -> case inferredShape of
          Just shape -> const_ shape val
          _ -> error "Can't infer shape of the constant"
        PSumList ptl ->
          sum_ . buildFromPatternList inferredShape match $ ptl
        PMulList ptl ->
          product_ . buildFromPatternList inferredShape match $ ptl
        PSum sps -> sum_ . map (build inferredShape) $ sps
        PMul sps -> product_ . map (build inferredShape) $ sps
        PNeg sp -> - build inferredShape sp
        PScale sp1 sp2 -> build (Just []) sp1 *. build inferredShape sp2
        PDiv sp1 sp2 -> build inferredShape sp1 / build inferredShape sp2
        PSqrt sp -> sqrt (build inferredShape sp)
        PSin sp -> sin (build inferredShape sp)
        PCos sp -> cos (build inferredShape sp)
        PTan sp -> tan (build inferredShape sp)
        PExp sp -> exp (build inferredShape sp)
        PLog sp -> log (build inferredShape sp)
        PSinh sp -> sinh (build inferredShape sp)
        PCosh sp -> cosh (build inferredShape sp)
        PTanh sp -> tanh (build inferredShape sp)
        PAsin sp -> asin (build inferredShape sp)
        PAcos sp -> acos (build inferredShape sp)
        PAtan sp -> atan (build inferredShape sp)
        PAsinh sp -> asinh (build inferredShape sp)
        PAcosh sp -> acosh (build inferredShape sp)
        PAtanh sp -> atanh (build inferredShape sp)
        PRealImag sp1 sp2 -> build inferredShape sp1 +: build inferredShape sp2
        PRealPart sp -> xRe (build inferredShape sp)
        PImagPart sp -> xIm (build inferredShape sp)
        PInnerProd sp1 sp2 -> build Nothing sp1 <.> build Nothing sp2
        PMulRest restCapture sps
          | Just ns <- Map.lookup restCapture (listCapturesMap match) ->
            product_ $ (map just ns) ++ map (build inferredShape) sps
        PSumRest restCapture sps
          | Just ns <- Map.lookup restCapture (listCapturesMap match) ->
            sum_ $ (map just $ ns) ++ map (build inferredShape) sps
        PPower sp pp ->
          let val = buildFromPatternPower match pp
           in build inferredShape sp ^ val
        PRotate pra sp ->
          let rotateAmount = buildFromPatternRotateAmount match pra
           in rotate rotateAmount (build inferredShape sp)
        _ -> error "The right hand-side of substitution has something that we don't support yet"
