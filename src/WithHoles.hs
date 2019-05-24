{-
(c) 2014 Christopher Kumar Anand

Helper functions/instances to make pattern gaurds involving Expressions easier to read.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module WithHoles where

import qualified Data.IntMap as I
import qualified Data.List as L
import Data.Maybe
import HashedExpression
    ( Complex
    , Dims(..)
    , Dir(..)
    , ExpressionEdge(..)
    , Internal
    , Node
    , OpId(..)
    , RealVectorSpace
    , Rectangular
    , addEdge
    , getDimE
    )
import qualified HashedExpression
import HashedInstances ()

data WithHoles
    = WHOp OpId [WithHoles]
    | WHConst Double
    | WHHole Int -- like a variable in a regex
    | WHList Int -- list variable
    deriving (Show, Eq, Ord)

{-
FIXME:  figure these out later:
  - to support Sum and Prod types, we need WHList and then we will need predicates
    or we need to recognize Sum as (+) similarly to List and (:)
    but this would be slower
      -- - s :*: Sum(x,y,z) -> Sum(s :*: x, s :*: y, s :*: z)

    CONFLUENCE:  start by normalizing sums and prods
                 - 0 in a sum always disappears, reduces number of ops
                 - 0 in prod always produces zero with fewer ops
                 - 1 in prod always reduces the number of ops
                 - we are looking for minimum number of ops for a normal form
  - if we want to substitute values for variables, we need to add support
  - DVar support will be needed for differentiation
  - RelElem help!

                | Var Dims ByteString               -- variable X
                | DVar Dims ByteString              -- differential variable dX
                | RelElem  {reArray::Int            -- array number
                           ,reBoundary::Boundary    -- how to treat boundary elements
                           ,reIdx::[Int]            -- offsets in array indices
                           }                        --   length reIdx == length outDims

Return a list of all wholes in a pattern-matching expression.
-}
definedHoles (WHHole i) = [i]
definedHoles (WHOp _ subs) = concatMap definedHoles subs
definedHoles _ = []

isHoleInPattern i pattern = i `elem` (definedHoles pattern)

{-

Instances which help us build expressions.
-}
instance Num WithHoles where
    negate x = WHOp Prod [WHConst (-1), x]
    fromInteger i = WHConst $ fromInteger i
    x + y = WHOp Sum [x, y]
    x * y = WHOp Prod [x, y]
    abs x = WHOp Abs [x]
    signum x = WHOp Signum [x]

{-

Instance to convert back and forth between real and complex nodes.
-}
instance Complex WithHoles WithHoles where
    x +: y = WHOp RealImag [x, y]
    iRe x = WHOp RealImag [x, WHConst 0]
    iIm y = WHOp RealImag [WHConst 0, y]
    xRe z = WHOp RealPart [z]
    xIm z = WHOp ImagPart [z]

{-

Float-valued expressions for now:
-}
instance Fractional WithHoles where
    x / y = WHOp Div [x, y]
    recip x = WHOp Div [WHConst 1, x]
    fromRational x = WHConst $ fromRational x

{-

-}
instance Floating WithHoles where
    sqrt x = WHOp Sqrt [x]
    pi = WHConst pi
    exp x = WHOp Exp [x]
    cos x = WHOp Cos [x]
    sin x = WHOp Sin [x]
    tan x = WHOp Tan [x]
    log x = WHOp Log [x]
    asin x = WHOp Asin [x]
    acos x = WHOp Acos [x]
    atan x = WHOp Atan [x]
    sinh x = WHOp Sinh [x]
    cosh x = WHOp Cosh [x]
    tanh x = WHOp Tanh [x]
    atanh x = WHOp Atanh [x]
    acosh x = WHOp Acosh [x]
    asinh x = WHOp Asinh [x]

{-

-}
instance RealVectorSpace WithHoles WithHoles where
    scale n1 n2 = WHOp ScaleV [n1, n2]
    dot n1 n2 = WHOp Dot [n1, n2]
    subMask n1 n2 = WHOp SubMask [n1, n2]
    negMask n1 n2 = WHOp NegMask [n1, n2]
    projSS ss n = WHOp (Project ss) [n]
    injectSS ss n = WHOp (Inject ss) [n]
    mapR _fun _n =
        error $ "WithHoles.cannot match maps  FIXME:  switch over to sczs"

{-

FIXME support instance SubsampledSpase, ComplexVectorSpace

-}
instance Rectangular WithHoles where
    ft n = WHOp (FT True) [n]
    invFt n = WHOp (FT False) [n] -- FIXME think about where the normalizing factor should go
    rowPFT n = WHOp (PFT True Row) [n]
    columnPFT _e = error "partial ft not defined for WithHoles"
    colPFT _ _e = error "partial ft not defined for WithHoles"
    slicePFT _e = error "partial ft not defined for WithHoles"
    invRowPFT n = WHOp (PFT False Row) [n]
    invColumnPFT _e = error "partial ft not defined for WithHoles"
    invSlicePFT _e = error "partial ft not defined for WithHoles"
    transp swap =
        error $ "transpose " ++ show swap ++ " not defined for WithHoles"

{-

Check for a match, and collect a list of identified holes.
--FIXME  this allows for finding the same hole twice and doesn't check that the matchings agree
-}
isMatch :: Internal -> WithHoles -> Node -> Maybe [(Int, Node)]
isMatch exprs (WHOp opW xs) n
    | Just (Op _ opE ns) <- I.lookup n exprs
    , opE == opW
    , length xs == length ns
    , let subMatches = zipWith (isMatch exprs) xs ns
    , not $ any (== Nothing) subMatches = Just $ concat $ catMaybes subMatches
isMatch exprs (WHConst c) n
    | Just (Const _ d) <- I.lookup n exprs
    , c == d = Just []
isMatch _exprs (WHHole idx) n = Just [(idx, n)]
isMatch _ _ _ = Nothing

{-

Find dims
-}
dimWH :: [(Int, Node)] -> Internal -> WithHoles -> Maybe Dims
dimWH found e0 (WHOp op xs@(_x1:_)) =
    case op of
        Dot -> Just Dim0
        ScaleV ->
            case xs of
                (_:x2:[]) -> dimWH found e0 x2
                _ -> error "WH.dimWH ScaleV needs 2 args"
        Transpose _ -> error "WH.dimWH transpose"
        _ ->
            case catMaybes $ map (dimWH found e0) xs of
                (newDim:_) -> Just newDim
                _ -> Nothing
dimWH _found _e0 (WHOp _op []) = Nothing -- FIXME should we check for errors
dimWH _found _e0 (WHConst _c) = Nothing
dimWH found e0 (WHHole idx) =
    case lookup idx found of
        Just n -> Just $ getDimE e0 n
        Nothing -> error $ "WH.dimWH hole not found " ++ show (idx, found, e0)
dimWH _found _e0 (WHList _hs) = error "WH.dimWH List unfinished"

{-

Apply transformation
-}
apply :: Dims -> [(Int, Node)] -> Internal -> WithHoles -> (Internal, Node)
apply dims found e0 (WHOp op xs) = addEdge e1 (Op dims op ns)
  where
    (e1, ns) =
        case op of
            ScaleV ->
                case xs of
                    [xS, xV] ->
                        let (e1, nS) = apply Dim0 found e0 xS
                            (e2, nV) = apply dims found e1 xV
                         in (e2, [nS, nV])
                    _ ->
                        error $
                        "WH.apply Scale " ++ show (dims, found, e0, op, xs)
            Dot ->
                case catMaybes $ map (dimWH found e0) xs of
                    (newDim:_) -> L.mapAccumL (apply newDim found) e0 xs
                    _ ->
                        error $
                        "WH.apply unknown <.> dim " ++
                        show (dims, found, e0, op, xs)
            Transpose _ -> error "WithHoles.apply confused by Transpose"
            _ -> L.mapAccumL (apply dims found) e0 xs
apply dims _found e0 (WHConst c) = addEdge e0 (Const dims c)
apply _dims found e0 (WHHole idx) =
    case lookup idx found of
        Just n -> (e0, n)
        Nothing -> error $ "WH.apply hole not found " ++ show (idx, found, e0)
apply _dims _found _e0 (WHList _hs) = error "WH.apply List unfinished"

{-

Apply the first rule from a list which applies to the head node.
-- FIXME  this is recursive, so we better know it is confluent or at least that it terminates.
-}
applyOne ::
       (Internal, Node)
    -> [(GuardedPattern, WithHoles)]
    -> Maybe (Internal, Node)
applyOne (e, n) ((GP pattern condition, replacement):rules)
    | Just found <- isMatch e pattern n
    , condition e found = Just $ apply (getDimE e n) found e replacement
    | otherwise = applyOne (e, n) rules
applyOne _ [] = Nothing

{-

Guards for pattern matching
-}
containsDifferential ::
       WithHoles -> WithHoles -> Internal -> [(Int, Node)] -> Bool
containsDifferential (WHHole i) pattern =
    if i `isHoleInPattern` pattern
        then \exprs holes ->
                 case lookup i holes of
                     Just n -> HashedExpression.containsDifferential exprs n
                     _ ->
                         error $
                         "WH.containsDifferential " ++ show (i, pattern, exprs)
        else error $
             "WH.containsDifferential applied to hole " ++
             show i ++ " not defined in " ++ show pattern
containsDifferential x _ =
    error $ "WH.containsDifferential can only be given a hole not a " ++ show x

{-

-}
always :: WithHoles -> Internal -> [(Int, Node)] -> Bool
always _ = \_ _ -> True

{-

-}
infixl 2 ||.

infixl 3 &&.

type Condition = WithHoles -> Internal -> [(Int, Node)] -> Bool

(||.) :: Condition -> Condition -> Condition
(||.) = mergeWith (||)

(&&.) :: Condition -> Condition -> Condition
(&&.) = mergeWith (&&)

mergeWith op c1 c2 =
    \pattern ->
        let c1Ok = c1 pattern
            c2Ok = c2 pattern
         in \exprs filled -> (c1Ok exprs filled) `op` (c2Ok exprs filled)

{-

Add condition to pattern.
-}
infixl 1 |.

data GuardedPattern =
    GP WithHoles (Internal -> [(Int, Node)] -> Bool)

-- with required property that the holes used by the condition are defined in the pattern
(|.) :: WithHoles -> Condition -> GuardedPattern
(|.) pattern condition = GP pattern $ condition pattern

{-

Syntactic sugar for defining rules.
-}
infix 0 ~~>

infix 0 |.~~>

(|.~~>) :: WithHoles -> WithHoles -> (GuardedPattern, WithHoles)
(|.~~>) pattern replacement = (GP pattern $ const (const True), replacement)

(~~>) :: GuardedPattern -> WithHoles -> (GuardedPattern, WithHoles)
(~~>) gPattern replacement = (gPattern, replacement)

{-

-}
[p, q, r, s, t, u, v, w, x, y, z] = map WHHole [1 .. 11] {-


-}
