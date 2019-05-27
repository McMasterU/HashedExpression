{-
(c) 2010 Christopher Kumar Anand

Experiment in common subexpressions without monads and better expression simplification.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module HashedExpression where

import qualified Data.Array.Unboxed as U
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import qualified Data.List as L
import qualified Numeric

--import Data.Map (Map)
import Control.DeepSeq
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace

{-


Here we can turn debug on and off with one variable.  mt is a function to apply a trace, but only if debug is on.
-}
skipDebug = True

mt x y =
    if skipDebug
        then y
        else trace x y

{-

Type wrappers for expressions which we will use to build expressions.
The |Show| instances of these pretty-print a single expression (duplicating common subexpressions).
-}
data Scalar =
    Scalar
        { unScalar :: Expression
        }
    deriving (Eq)

data ScalarC =
    ScalarC
        { unScalarC :: Expression
        }
    deriving (Eq)

data OneD =
    OneD
        { unOneD :: Expression
        }
    deriving (Eq)

oneD e n = OneD $ Expression n e

data OneDC =
    OneDC
        { unOneDC :: Expression
        }
    deriving (Eq)

oneDC e n = OneDC $ Expression n e

data TwoD =
    TwoD
        { unTwoD :: Expression
        }
    deriving (Eq)

twoD e n = TwoD $ Expression n e

data TwoDSparse =
    TwoDSparse !SparseList2D Expression
    deriving (Eq)

twoDSparse sampleList e n = TwoDSparse sampleList $ Expression n e

data TwoDC =
    TwoDC
        { unTwoDC :: Expression
        }
    deriving (Eq)

twoDC e n = TwoDC $ Expression n e

data TwoDCSparse =
    TwoDCSparse !SparseList2D Expression
    deriving (Eq)

data ThreeD =
    ThreeD
        { unThreeD :: Expression
        }
    deriving (Eq)

data ThreeDSparse =
    ThreeDSparse !SparseList3D Expression
    deriving (Eq)

threeD e n = ThreeD $ Expression n e

data ThreeDC =
    ThreeDC
        { unThreeDC :: Expression
        }
    deriving (Eq)

data ThreeDCSparse =
    ThreeDCSparse !SparseList3D Expression
    deriving (Eq)

threeDC e n = ThreeDC $ Expression n e

{-
The generic case of some samples not on any grid is
-}
data Vector =
    Vector Int Expression
    deriving (Eq, Show)

{-
which only encodes the number of samples.

If we subsample affine subspaces of a regular k-space,
we have a choice of putting each affine subspace in a separate ND variable,
or we could put the union in a Vector, but |Vector| doesn't implement |Rectangular|.

The wrapped types all implement |Num| although only |+| works for non-scalars, and |Scalar| implements |Floating| and |Fractional|.

Users will construct expressions using these classes and the following classes which provide vector space operations:

All the non-scalar types are rectangular arrays, for which some other operations makes sense,
but in the future, we may have to make the classes more fine-grained.
Dimensions must match and will be checked by the instances.
-}
class Rectangular a where
    ft :: a -> a -- complex Fourier Transform on (real,imaginary) pair of arrays
    invFt :: a -> a -- inverse transform
    rowPFT :: a -> a
    colPFT :: Bool -> a -> a
    columnPFT :: a -> a
    slicePFT :: a -> a
    invRowPFT :: a -> a
    invColumnPFT :: a -> a
    invSlicePFT :: a -> a
    transp :: Swap -> a -> a -- precondition:  Swap type has to match dimensions

transp2d = transp SCR

-- crop each dimension by keeping |[(low,high)]| for each dimension
crop (keep, fullDims) = projSS (SSCrop keep fullDims)

uncrop (keep, fullDims) = injectSS (SSCrop keep fullDims)
                                --   skipping first $b$ and last $e$

-- Nyquist subsampling |[(p,(b,e))]| keeping every $p$th hyperplane
comb list = projSS (SSNyquist list)

uncomb list = injectSS (SSNyquist list)
  -- *** this doesn't make sense in general, only for cropping and Nyquist

{-

All variables with real vector-space interpretations will implement the following class:
-}
infixl 8 *.

infixl 8 `scale`

class HasScale v s where
    (*.) :: s -> v -> v

instance (RealVectorSpace v s) => HasScale v s where
    (*.) = scale -- deprecate scale

-- RF: Changed to 8 to give higher prec. than |*| |+| etc.
-- May cause problems with something like x1*.1 <.> y1*.1
-- To solve: use parentheses: (x1*.1) <.> (y1*.1)
-- but: Is there a simplification rule x*.1 <.> y*.1 ~~> x <.> y *.1  ?
-- If so, this may not cause it to happen (unsure, must investigate).
-- 2015/06/04.
infixl 8 <.>

infixl 8 `dot`

(<.>) = dot

class RealVectorSpace v s | v -> s where
    scale :: s -> v -> v
    dot :: v -> v -> s
    subMask :: v -> v -> v
    negMask :: v -> v -> v
    mapR :: (s -> s) -> v -> v
  --zipR :: (s -> s -> s) -> v -> v -> v
  --zipR3 :: (s -> s -> s -> s) -> v -> v -> v -> v
    norm2 :: v -> s
    norm :: (Floating s) => v -> s
    norm2 v = dot v v
    norm = sqrt . norm2
  -- *** currently, we don't have enough information in the nodes to determine which subspaces
  -- *** make sense, so we have to walk the graph and infer types
  -- FIXME don't use for SparseList, should make different proj/inject for each type of sparsity
    projSS :: Subspace -> v -> v -- project onto a subspace as defined below (e.g. by cropping)
    injectSS :: Subspace -> v -> v -- inject (adjoint to project) from a subspace

{-

Regularization, the dimension of the discretized space determines the type of the domain kernel specifier.
The model only contains the objective function, not the gradient.
-}
class Regularizable v dk where
    huber :: dk -> v -> Scalar
    tukey :: dk -> Double -> v -> Scalar
    l2Bi :: dk -> v -> Scalar
    l1Bi :: dk -> v -> Scalar
    l1l2Bi :: dk -> v -> Scalar
    gmBi :: dk -> v -> Scalar

{-

Sparse sampling
-}
data Dim12
    = Dii1
    | Dii2
    deriving (Show, Ord, Eq)

data SparseList2D =
    SparseList2D
        Int -- number of samples
        Dim12 -- dense dimension
        !(U.UArray Int Int) -- array of samples to keep
        !(Int, Int) -- minimum and maximum elements
    deriving (Eq, Show, Ord)

data Dim13
    = Diii1
    | Diii2
    | Diii3
    deriving (Show, Ord, Eq)

data SparseList3D =
    SparseList3D
        Int -- number of samples
        Dim13 -- dense dimension
        !(U.UArray Int Int) -- array of y samples to keep
        !(Int, Int) -- minimum and maximum elements
        !(U.UArray Int Int) -- array of z samples to keep
        !(Int, Int) -- minimum and maximum elements
    deriving (Eq, Show, Ord)

data Dim14
    = Diiii1
    | Diiii2
    | Diiii3
    | Dimm4
    deriving (Show, Ord, Eq)

data SparseList4D =
    SparseList4D
        Int -- number of samples
        Dim14 -- dense dimension
        !(U.UArray Int Int) -- array of samples to keep
        !(Int, Int) -- minimum and maximum elements
        !(U.UArray Int Int) -- array of samples to keep
        !(Int, Int) -- minimum and maximum elements
        !(U.UArray Int Int) -- array of samples to keep
        !(Int, Int) -- minimum and maximum elements
    deriving (Eq, Show, Ord)

-- CKA: this is only implemented for Dii1, what about Dii2??
mkSparseList2D list =
    let len = length list
     in SparseList2D len Dii1 (U.listArray (0, len - 1) list) (minMax id list)

mkSparseList3D list =
    let len = length list
     in SparseList3D
            len
            Diii1
            (U.listArray (0, len - 1) $ map fst list)
            (minMax fst list)
            (U.listArray (0, len - 1) $ map snd list)
            (minMax snd list)

mkSparseList4D list =
    let len = length list
     in SparseList4D
            len
            Diiii1
            (U.listArray (0, len - 1) $ map fst3 list)
            (minMax fst3 list)
            (U.listArray (0, len - 1) $ map snd3 list)
            (minMax snd3 list)
            (U.listArray (0, len - 1) $ map thrd3 list)
            (minMax thrd3 list)

minMax f = minMax' (1073741824, -1073741824)
  where
    minMax' (min, max) [] = (min, max)
    minMax' (min, max) (x:xs) =
        let x' = f x
         in minMax'
                ( if min < x'
                      then min
                      else x'
                , if max > x'
                      then max
                      else x')
                xs

class SubsampledSpace vFull vSparse sL
    | vSparse -> vFull
    , vSparse -> sL
    , vFull -> vSparse
    where
    projSparse :: sL -> vFull -> vSparse
    injectSparse :: vSparse -> vFull

{-

Define complex type with operations to insert/extract real components:
-}
infix 6 +:

class Complex c r | r -> c, c -> r where
    (+:) :: r -> r -> c
    iRe :: r -> c
    iIm :: r -> c
    xRe :: c -> r
    xIm :: c -> r

{-

All variables with real vector-space interpretations will implement the following class:
-}
infix 7 .*:

class ComplexVectorSpace v vr s sr | v -> s, v -> vr, vr -> sr where
    (.*:) :: s -> v -> v
    scaleC :: s -> v -> v
    scaleR :: sr -> v -> v
  --dot :: v -> v -> s    -- JLMP: still thinking about this
    mapC :: (s -> s) -> v -> v
    realPartV :: v -> vr -- JLMP:  change to xReV and xImV
    imagPartV :: v -> vr
    magV :: v -> vr
    (.*:) = scaleC

{-

It would be nice to have a class like this, but we would need to do type inferencing to figure out its dimensions.
CKA:  when we wrap Mary's types, we can use this
-}
class HasZero v where
    zero :: v

{-

-}
class Pretty a where
    pretty :: a -> String

{-

Inputs to expressions are just free variables, which can be created by these helper functions:
-}
var = Scalar . (varHidden Dim0)

varc = ScalarC . (varHidden Dim0)

var1d shape = OneD . (varHidden (Dim1 shape))

dvar1d shape = OneD . (dvarHidden (Dim1 shape))

var1dc shape = OneDC . (varHidden (Dim1 shape)) -- shouldn't we create separate real and imaginary parts and wrap them

var2d shape = TwoD . (varHidden (Dim2 shape))

var2ds shape sL@(SparseList2D len _ _ _) =
    (TwoDSparse sL) . (varHidden (Dim2SL1 shape len))

var2dc shape = TwoDC . (varHidden (Dim2 shape))

var3d shape = ThreeD . (varHidden (Dim3 shape))

var3ds shape sL@(SparseList3D len _ _ _ _ _) =
    (ThreeDSparse sL) . (varHidden (Dim3SL1 shape len))

var3dc shape = ThreeDC . (varHidden (Dim3 shape))

{-

FIXME:  change isConst to isJustConst and other function to isConst
Is the |ExpressionEdge| lookup up into a Maybe a constant zero.
-}
isZero (Just (Const _ 0)) = True
isZero _ = False

isJustVar (Just (Var _ _)) = True
isJustVar _ = False

isConst (Just (Const _ _)) = True
isConst _ = False

isScalarConst (Just (Const Dim0 _)) = True
isScalarConst _ = False

extractScalarConsts =
    concatMap
        (\x ->
             case x of
                 Just (Const Dim0 d) -> [d]
                 _ -> [])

isJustSCZ = isSCZ

isSCZ (Just (Op _ (SCZ _) _)) = True
isSCZ _ = False

isJustDot (Just (Op _ Dot _)) = True
isJustDot _ = False

isJustProj (Just (Op _ (Project _) _)) = True
isJustProj _ = False

isJustInject (Just (Op _ (Inject _) _)) = True
isJustInject _ = False

getConst (Just (Const _ d)) = [d]
getConst _ = []

{-
Find SCZs in an expression:
-}
getSCZs (e, n) = filter isSCZ $ map (flip I.lookup e) $ topSort e n

{-
-}
isSCZNode e n =
    case I.lookup n e of
        Just (Op _ (SCZ (Expression sczN sczE)) args) ->
            Just ((sczE, sczN), args)
        _ -> Nothing

justArgs (Just (_, args)) = args
justArgs Nothing = []

{-

-}
containsDifferential exprs node =
    case I.lookup node exprs of
        Just (Const _ _) -> False
        Just (RelElem _ _ _) ->
            error $
            "containsDifferential found RelElem " ++ (take 200 $ show exprs)
        Just (Op _dims (SCZ _sczExpr) inputs)
     -- for now, we will assume that the differential is in the last slot,
     -- because that is where we are putting it initially, and that this slot is
     -- included linearly in every term of the expression (but we don't check it)
         ->
            containsDifferential exprs $
            case (reverse inputs) of
                (a:_) -> a
                _ ->
                    error $
                    "containsDifferential SCZ no inputs" ++
                    (pretty $ Expression node exprs)
        Just (Op _dims _op inputs) -> any (containsDifferential exprs) inputs
    {- case any (containsDifferential exprs) inputs of
                                 [True] -> True
                                 [False] -> False
                                 _ -> error $ "containsDifferential Op["++show op++
                                   "] inhomogeneous inputs " ++
                                   (concatMap ((\ x->pretty $ Expression x exprs) . snd) inputs) ++ show (inputs,exprs) -}
        Just (Var _ _) -> False
        Just (DVar _ _) -> True
        Nothing ->
            error $ "containsDifferential lookup failure " ++ show (node, exprs)

{-

Is the |ExpressionEdge| lookup up into a Maybe a constant zero.
-}
isNegArg exprs node =
    case I.lookup node exprs of
        Just (Op _ Neg [arg]) -> (exprs, (True, arg))
        Just (Const dims a) ->
            if a < 0
                then let (e, n) = addEdge exprs (Const dims (-a))
                      in (e, (True, n))
                else (exprs, (False, node))
        _ -> (exprs, (False, node))

{-

Is the |ExpressionEdge| lookup up into a Maybe a constant one.
-}
isOne (Just (Const Dim0 1)) = True
isOne _ = False

{-

Filter out nodes from a list of |Node|s in an |Internal| expression representation, using a predicate |toExclude|.
-}
filterOut toExclude exprs = filter (not . toExclude . (flip I.lookup exprs))

{-

Definition of subspaces:
Required properties:  the lists in |SSCoord|, |SSDiag|, |SSList| and |SSCrop| must
have the same length as the number of dimensions.
|SSCrop| and |SSNyquist| the only one which will preserve rectangularity and the number of dimensions.
Index numbering starts at 0.
-}
data Subspace
    = SSCoord [Maybe Int] -- project out coords which are |Just x| along coord = x
    | SSNyquist [(Int, (Int, Int))] -- $(p,(b,e))$ keep every $p$th hyperplane
                                                 --   skipping first $b$ and last $e$
    | SSAffine [Affine Int] -- this is currently too dangerous, don't use
    | SSUnion [Subspace] -- keep union of these subspaces; this is currently too dangerous, don't use
    | SSInter [Subspace] -- keep only intersection of these subspaces
    | SSComplement Subspace -- complement of a |Subspace|
    | SSList2D !SparseList2D -- just keep these PEs
    | SSList3D !SparseList3D
    | SSList4D !SparseList4D
                          -- eg [(0,0),(0,1),(-2,1),...] in a 2d discretization
    | SSCrop [(Int, Int)] [Int] -- keep only a box (low,high) including both endpoints
    deriving (Show, Eq, Ord)

{-

Descriptor for affine subspaces (cut out by linear equations).
We make slopes and intercepts a type parameter so we can specify diophantine equations.
Note that we can also use this to specify Nyquist sampling.
It would also be possible to
-}
data Affine num
    = AAll -- cross-section in this dimension is complete
    | ASlopeIntercept
          num --   slope of line with respect to unit change
          num --   position at unit=0
    deriving (Show, Ord, Eq)

{-

Possibility for typing subspaces more...not worth it for now. TB
data Subspace1 = SSCoord1 Maybe Int
               | SSNyquist1 (Int,(Int,Int))
               | SSAffine1 Affine Int

-}
projDim :: Dims -> Subspace -> Dims
projDim (Dim1 dimIn) (SSCrop [bound] ds) =
    if [dimIn] == ds
        then Dim1 (cropDim dimIn bound)
        else error $ "p1Dim" ++ (show ((Dim1 dimIn), (SSCrop [bound] ds)))
projDim (Dim2 (d1, d2)) (SSCrop bounds ds) =
    if length bounds == 2 && ds == [d1, d2]
        then let [d1', d2'] = zipWith cropDim [d1, d2] bounds
              in Dim2 (d1', d2')
        else error $
             "projDim Dim2 crop" ++ (show ((Dim2 (d1, d2)), (SSCrop bounds ds)))
projDim (Dim3 (d1, d2, d3)) (SSCrop bounds ds) =
    if length bounds == 3 && ds == [d1, d2, d3]
        then let [d1', d2', d3'] = zipWith cropDim [d1, d2, d3] bounds
              in Dim3 (d1', d2', d3')
        else error $
             "projDim Dim3 crop" ++
             (show ((Dim3 (d1, d2, d3)), (SSCrop bounds ds)))
projDim (Dim4 (d1, d2, d3, d4)) (SSCrop bounds ds) =
    if length bounds == 4 && ds == [d1, d2, d3, d4]
        then let [d1', d2', d3', d4'] = zipWith cropDim [d1, d2, d3, d4] bounds
              in Dim4 (d1', d2', d3', d4')
        else error $
             "projDim Dim4 crop" ++
             (show ((Dim4 (d1, d2, d3, d4)), (SSCrop bounds ds)))
projDim (Dim1 dimIn) (SSNyquist [reduction]) =
    mt ("new dims for Project Dim1 " ++
        show dimIn ++ " SSNyquist is Dim1 " ++ show dimOut) $
    Dim1 dimOut
  where
    dimOut = reduce dimIn reduction
projDim (Dim2 (d1, d2)) (SSNyquist reductions) =
    if length reductions == 2
        then let [d1', d2'] = zipWith reduce [d1, d2] reductions
              in Dim2 (d1', d2')
        else error "projDim Dim2 Nyquist"
projDim (Dim3 (d1, d2, d3)) (SSNyquist reductions) =
    if length reductions == 3
        then let [d1', d2', d3'] = zipWith reduce [d1, d2, d3] reductions
              in Dim3 (d1', d2', d3')
        else error "projDim Dim3 Nyquist"
projDim (Dim4 (d1, d2, d3, d4)) (SSNyquist reductions) =
    if length reductions == 4
        then let [d1', d2', d3', d4'] =
                     zipWith reduce [d1, d2, d3, d4] reductions
              in Dim4 (d1', d2', d3', d4')
        else error "projDim Dim4 Nyquist"
{-

TB -> my projDim SSCoord implementation; I don't know if it's quite right, but it seems to work.
-}
projDim (Dim1 dimIn) (SSCoord [Nothing]) = Dim1 dimIn
projDim (Dim1 _dimIn) (SSCoord [Just _int]) = Dim0
projDim (Dim2 (d1, d2)) (SSCoord maybeInts) =
    if length maybeInts /= 2
        then error ("HE.projDim Dim2 Coord does not match " ++ show maybeInts)
        else case length newDims of
                 0 -> Dim0
                 1 -> Dim1 (newDims !! 0)
                 2 -> Dim2 (newDims !! 0, newDims !! 1)
                 _ -> error $ "projDim2 unsupported " ++ show maybeInts
  where
    newIndices = L.elemIndices Nothing maybeInts
    newDims = map ((!!) [d1, d2]) newIndices
projDim (Dim3 (d1, d2, d3)) (SSCoord maybeInts) =
    if length maybeInts /= 3
        then error ("HE.projDim Dim3 Coord does not match " ++ show maybeInts)
        else case length newDims of
                 0 -> Dim0
                 1 -> Dim1 (newDims !! 0)
                 2 -> Dim2 (newDims !! 0, newDims !! 1)
                 3 -> Dim3 (newDims !! 0, newDims !! 1, newDims !! 2)
                 _ -> error $ "projDim3 unsupported " ++ show maybeInts
  where
    newIndices = L.elemIndices Nothing maybeInts
    newDims = map ((!!) [d1, d2, d3]) newIndices
{--
unTwoD (projSS (SSCoord [Just 3, Nothing]) x2)
returns this:
Expression 75096643 (fromList [(2029638,Var (Dim2 (3,4)) "X2"),(75096643,Op (Dim1 4) (Project (SSCoord [Just 3,Nothing])) [2029638])])
which seems to work, but it doesn't show properly show TwoD non-scalar (Dim1 4,Project (SSCoord [Just 3,Nothing]))
--}
projDim DimUnknown _ss = DimUnknown
projDim dim ss = error $ "projDim doesn't implement " ++ show (dim, ss)

reduce dim (reduction, (skipLeft, skipRight)) =
    let trimmed = dim - skipLeft - skipRight + (reduction - 1)
     in if mod trimmed reduction == 0
            then div trimmed reduction
            else error $
                 "projDim SSNyquist " ++
                 show (dim, (reduction, (skipLeft, skipRight), trimmed))

mapAnd (fun:funs) list = all fun list && mapAnd funs list
mapAnd [] _list = True

cropDim dimIn (left, right) =
    let err =
            concat
                [ if left < right
                      then ""
                      else "left >= right "
                , if 0 <= left
                      then ""
                      else "left < 0 "
                , if right <= dimIn
                      then ""
                      else "right > dimIn "
                ]
     in if null err
            then right - left + 1
            else error $ "cropDim " ++ err ++ show (dimIn, left, right)

{-

-}
injectDim dimIn (SSCrop bounds [d1]) = checkInj dimIn bounds $ Dim1 d1
injectDim dimIn (SSCrop bounds [d1, d2]) = checkInj dimIn bounds $ Dim2 (d1, d2)
injectDim dimIn (SSCrop bounds [d1, d2, d3]) =
    checkInj dimIn bounds $ Dim3 (d1, d2, d3)
injectDim dimIn (SSCrop bounds [d1, d2, d3, d4]) =
    checkInj dimIn bounds $ Dim4 (d1, d2, d3, d4)
injectDim (Dim1 dimIn) (SSNyquist [reduction]) =
    Dim1 (injReduce dimIn reduction)
injectDim (Dim2 (d1, d2)) (SSNyquist reductions) =
    if length reductions == 2
        then let [d1', d2'] = zipWith injReduce [d1, d2] reductions
              in Dim2 (d1', d2')
        else error "injectDim Dim2 Nyquist"
injectDim (Dim3 (d1, d2, d3)) (SSNyquist reductions) =
    if length reductions == 3
        then let [d1', d2', d3'] = zipWith injReduce [d1, d2, d3] reductions
              in Dim3 (d1', d2', d3')
        else error "injectDim Dim3 Nyquist"
injectDim (Dim4 (d1, d2, d3, d4)) (SSNyquist reductions) =
    if length reductions == 4
        then let [d1', d2', d3', d4'] =
                     zipWith injReduce [d1, d2, d3, d4] reductions
              in Dim4 (d1', d2', d3', d4')
        else error "projDim Dim4 Nyquist"
injectDim dim ss = error $ "injectDim doesn't implement " ++ show (dim, ss)

checkInj d b =
    if dimList d == map width b
        then id
        else trace ("checkInj " ++ show (d, b)) -- I think that this should be an error.  Treat it like an error for now, and see what happens.
  where
    width (low, high) = high - low + 1

injReduce dim (reduction, (skipLeft, skipRight)) =
    dim * reduction + skipLeft + skipRight

{-

-}
class Transformable a where
    simplify :: a -> a
    simplifyOne :: a -> a
    factor :: a -> a

{-


*** FIXME:  still needed here: functions to help create convolutions, and regularization functions

------------------------------
This is all the user should need to know about basic expressions from this module.
The actual expressions are
-}
data Expression =
    Expression
        Node -- the final product of this expression
        Internal -- all subexpressions, including vars -- (Int -> ExpressionEdge)
    deriving (Show, Ord)

{-
Equality instances.  Add one expression to the other and if they are equal the hashes will be the same.
(This instance is here so we can derive it for wrapper types.)
-}
instance Eq Expression where
    (Expression n1 e1) == (Expression n2 e2) =
        recreate (e1, n1) == recreate (e2, n2)

bigE (exprs, n) = Expression n exprs

{-

Nodes are indexed by 32-bit |Int|s because there is an optimized map for this.
-}
type Node = Int

type Internal = IntMap ExpressionEdge

{-

Notes:
*** Having reverse lookup doesn't seem very useful if hash functions are quick to calculate,
so we should definitely benchmark them and remove if not providing a benefit,
because they create an opportunity for things to get out of sync.

All the edges produce a single output, which is an |ExpressionEdge|.
Functions which seem to take pairs (like FT and complex arithmetic) are just wrappers around tuples of real expressions.
MC:  should we make a parametrized |ExpressionEdge a| type here to make it easier to test tree versions?
MC:  Property:  if we try to detect cycles (which we need to add to depthFirst) then we'd like to
                test that we really throw an error, or we could test that we never get into an infinite loop
MC:  generate random trees, and read them back and check against inputs, and check that they're the same
CKA:  should we require that each variable name has only one dimension?
      Or should we consider the dimension to be part of the name.
      Make names unique because that is consistent with mathematical convention.
      The problem will arrive if expressions with different variables are merged.
-}
data Dims
    = Dim0
    | Dim1 Int
    | Dim1p Int Int -- size and order of polynomial, interval is [-1,1]
    | Dim2 (Int, Int)
    | Dim2p (Int, Int) (Int, Int)
    | Dim2SL1 (Int, Int) Int
    | Dim2SL2 (Int, Int) Int
    | Dim3 (Int, Int, Int)
    | Dim3p (Int, Int, Int) (Int, Int, Int)
    | Dim3SL1 (Int, Int, Int) Int
    | Dim3SL2 (Int, Int, Int) Int
    | Dim3SL3 (Int, Int, Int) Int
    | Dim4 (Int, Int, Int, Int)
    | Dim4SL1 (Int, Int, Int, Int) Int
    | Dim4SL2 (Int, Int, Int, Int) Int
    | Dim4SL3 (Int, Int, Int, Int) Int
    | Dim4SL4 (Int, Int, Int, Int) Int
    | CDims [Dims] -- a compound node has a list of dimensions, which are the dimensions of the inputs.
    | DimUnknown
    deriving (Eq, Show, Ord)

data ExpressionEdge
    = Op
          Dims -- dims of output
          OpId -- the operation
          [Node] -- dims and nodes of inputs
    | Var Dims ByteString -- variable X
    | DVar Dims ByteString -- differential variable dX
    | RelElem
          { reArray :: Int -- array number
          , reBoundary :: Boundary -- how to treat boundary elements
          , reIdx :: [Int] -- offsets in array indices
          } --   length reIdx == length outDims
    | Const
          { unConstDims :: Dims
          , unConst :: Double
          }
    deriving (Eq, Show, Ord)

{-
*** what we can't do with |RelElem|s is make relative calculations needed to fix $N/R$
undersampling artifacts.
If we want to do that, we probably need to introduce size variables and expressions
-}
data ZipConvElem =
    ZCE Expression

relElem :: Int -> Boundary -> [Int] -> Scalar {-always returns RelElem-}
relElem array boundary offset = Scalar $ Expression h (I.fromList [(h, e)])
  where
    h = hash e
    e = RelElem array boundary offset

{-

-}
class ConvZip v s idx | v -> s, v -> idx where
    conv :: Boundary -> [(idx, s)] -> v -> v
    conv1Zip1 :: ((Boundary -> idx -> s) -> s) -> v -> v
    conv2Zip1 ::
           (((Boundary -> idx -> s), (Boundary -> idx -> s)) -> s)
        -> (v, v)
        -> v
    conv3Zip1 ::
           (( (Boundary -> idx -> s)
            , (Boundary -> idx -> s)
            , (Boundary -> idx -> s)) -> s)
        -> (v, v, v)
        -> v
    conv4Zip1 ::
           (( (Boundary -> idx -> s)
            , (Boundary -> idx -> s)
            , (Boundary -> idx -> s)
            , (Boundary -> idx -> s)) -> s)
        -> (v, v, v, v)
        -> v
    czMap :: (s -> s) -> v -> v
    czZip :: (s -> s -> s) -> v -> v -> v
    czZip3 :: (s -> s -> s -> s) -> v -> v -> v -> v
    czZip4 :: (s -> s -> s -> s -> s) -> v -> v -> v -> v -> v

{-

-}
cMult u v =
    let ux = xRe u
        uy = xIm u
        vx = xRe v
        vy = xIm v
     in (conv4Zip1
             (\(ur, ui, vr, vi) ->
                  ((ur ZeroMargin zeroIdx) * (vr ZeroMargin zeroIdx) -
                   (ui ZeroMargin zeroIdx) * (vi ZeroMargin zeroIdx)))
             (ux, uy, vx, vy)) +:
        (conv4Zip1
             (\(ur, ui, vr, vi) ->
                  ((ur ZeroMargin zeroIdx) * (vi ZeroMargin zeroIdx) +
                   (ui ZeroMargin zeroIdx) * (vr ZeroMargin zeroIdx)))
             (ux, uy, vx, vy))

{-



--
Operations which we recognize in our expressions.
***They don't know about airity.
***We can interpret scalar operations as maps, we will need to prove that constructors
   make this make sense.
   A side effect of this is that if we make the expression edges Num instances, then we will get multiplication and addition and we can throw an error, so it would be better to wrap graphs in different types
Since nodes are stored in a map (Node -> ExpressionEdge), we can handle multiple inputs to an edge/operation, but multiple outputs require that we create a compound node and then extract elements with |Extract|.
Given the limitations of the Haskell type system, it is probably easier for us to write wrappers to generate operations with higher output airity.
-}
data OpId
    = FT Bool -- FT of any dimension, as long as the sizes are the same
                          -- True = forward , False = Inverse
    | PFT Bool Dir -- Partial FT with direction (same as above) and Dir of row, column or slice
    | Transpose Swap
    | Sum
    | SubMask
    | NegMask
    | Reglzr DomainWeights RangeKernel
    | GradReglzr DomainWeights RangeKernel
    | Neg
    | Abs
    | Signum
    | Prod
    | Div
    | Sqrt
    | Sin
    | Cos
    | Tan
    | Exp
    | Log
    | Sinh
    | Cosh
    | Tanh
    | Asin
    | Acos
    | Atan
    | Asinh
    | Acosh
    | Atanh
    | Dot -- *** this is polytypic, requiring an existential type
                 -- Dot of complex arguments is Hermitian project (so returns a real scalar)
                 -- JLMP: check that this is consistent with all uses of Dot
           -- JLMP:  eliminate this and convert uses to SCZ
    | MapND
          { mapExpr :: Expression -- Scalar; not explict due to circular Show instances
          , mapInput :: ByteString -- CKA: why can't we use SCZ?
          }
    | SCZ Expression -- WARNING:  use for pattern matching, not construction, use |mkSCZ|
    | ScaleV -- first input should be scalar and second a vector expression
    | Project Subspace
    | Inject Subspace
    | Compound Int -- make compound node
    | Extract Int -- extract element from compound node
    | RealPart -- extract real part
    | ImagPart -- extract imaginary part
    | RealImag -- make complex node by combining real and imaginary nodes
    deriving (Eq, Show, Ord)

prodScale dims =
    if null dims
        then Prod
        else ScaleV

{-
For the Regularizers, we need domain weights, which are a discrete version of a kernel,
-}
data DomainWeights
    = DW1d [(Int, Double)]
    | DW2d [((Int, Int), Double)]
    | DW3d [((Int, Int, Int), Double)]
    | DW4d [((Int, Int, Int, Int), Double)]
    | DW5d [((Int, Int, Int, Int, Int), Double)]
    | DW6d [((Int, Int, Int, Int, Int, Int), Double)]
    | DW7d [((Int, Int, Int, Int, Int, Int, Int), Double)]
    deriving (Eq, Show, Ord)

{-
and a range kernel
-}
data RangeKernel
    = RKHuber
    | RKTukey Double
    | RKL2
    | RKL1
    | RKL1mL2
    | RKGM -- Geman-McClure
    deriving (Eq, Show, Ord)

{-
These are taken from statistics:  http://research.microsoft.com/en-us/um/people/zhang/INRIA/Publis/Tutorial-Estim/node24.html


Direction of partial ft
-}
data Dir
    = Row
    | Column
    | Slice
    deriving (Eq, Show, Ord)

{-

Use index notation
  0 = Row
  1 = Column
  2 = Slice
-}
data Swap
    = SCR -- 2d transpose
    | SCRS -- swap first 2 indices of 3
    | SCSR -- rotate forward 3 indices
    | SSRC -- rotate backward 3 indices
    | SSCR -- swap first and last of 3
    | SRSC -- swap last 2 indices
    deriving (Eq, Show, Ord)

transposeDims :: Swap -> Dims -> Dims
transposeDims SCR (Dim2 (d1, d2)) = Dim2 (d2, d1)
transposeDims SCRS (Dim3 (r, c, s)) = Dim3 (c, r, s)
transposeDims SCSR (Dim3 (r, c, s)) = Dim3 (c, s, r)
transposeDims SSRC (Dim3 (r, c, s)) = Dim3 (s, r, c)
transposeDims SSCR (Dim3 (r, c, s)) = Dim3 (s, c, r)
transposeDims SRSC (Dim3 (r, c, s)) = Dim3 (r, s, c)
transposeDims _swap DimUnknown = DimUnknown
transposeDims swap dims =
    error $ "HE.transposeDims doesn't handle " ++ show (swap, dims)

{-

A reverse transpose; given the transpose of a dimension and the swap used to get it; we want the original.
-}
revTrDims :: Swap -> Dims -> Dims
revTrDims SCR (Dim2 (d1, d2)) = Dim2 (d2, d1)
revTrDims SCRS (Dim3 (c, r, s)) = Dim3 (r, c, s)
revTrDims SCSR (Dim3 (c, s, r)) = Dim3 (r, c, s)
revTrDims SSRC (Dim3 (s, r, c)) = Dim3 (r, c, s)
revTrDims SSCR (Dim3 (s, c, r)) = Dim3 (r, c, s)
revTrDims SRSC (Dim3 (r, s, c)) = Dim3 (r, c, s)
revTrDims _swap DimUnknown = DimUnknown
revTrDims swap dims =
    error $ "HE.revTrDims doesn't handle " ++ show (swap, dims)

linearOps =
    [ Neg
    , RealPart
    , ImagPart
    , FT True
    , FT False
    , PFT True Row
    , PFT False Row
    , PFT True Column
    , PFT False Column
    , PFT True Slice
    , PFT False Slice
    ]

linearOp SubMask = True
linearOp NegMask = True
linearOp (Project _) = True
linearOp (Inject _) = True
linearOp op = op `elem` linearOps

linearOpNoProj (Inject _) = True
linearOpNoProj op = op `elem` (RealImag : linearOps)

wantOutside (Project _) = False
wantOutside (Inject _) = True
wantOutside op =
    op `elem`
    [ Neg
    , FT True
    , FT False
    , PFT True Row
    , PFT False Row
    , PFT True Column
    , PFT False Column
    , PFT True Slice
    , PFT False Slice
    ]

wantInside (Project _) = True
wantInside (Inject _) = False
wantInside op = op `elem` [RealPart, ImagPart]

{-

|MapND| contains an function defined by an expression.
If this function has free variables, we need to grab the values from the outside
context.
The easiest would be to grab compile-time constants,
but it would be useful to be able to grab run-time values after verifying
that they have the right type.

Sparse-Convolution-Zip |SCZ| covers zips and sparse convolutions, and the convex hull of such operations.
It takes in a set of arrays, and produces a set of arrays with different dimensions.
\textit{Warning:  Use |HashedSimplify.mkSCZ| to construct a new node, which will put it in normal form,
       keeping the Expression map smaller.}
FIXME:  put in a place-holder
Note |mkSCZ| returns an |ExpressionEdge| but it is always |Op dims (SCZ expr) nodes|.
The expression with be normalized, and the node order will be normalized, with commuting inputs sorted by hash.
Type for ordering inner expressions.
-}
data InnerExpr
    = IERE Int Boundary [Int]
    | IEConst Double
    | IESum [InnerExpr]
    | IEProd [InnerExpr]
    | IENeg InnerExpr
    | IEAbs InnerExpr
    | IESignum InnerExpr
    | IEDiv InnerExpr InnerExpr
    | IESqrt InnerExpr
    | IESin InnerExpr
    | IECos InnerExpr
    | IETan InnerExpr
    | IEExp InnerExpr
    | IELog InnerExpr
    | IESinh InnerExpr
    | IECosh InnerExpr
    | IETanh InnerExpr
    | IEAsin InnerExpr
    | IEAcos InnerExpr
    | IEAtan InnerExpr
    | IEAsinh InnerExpr
    | IEAcosh InnerExpr
    | IEAtanh InnerExpr
    deriving (Show, Eq, Ord)

{-
Conversion to |InnerExpr|, incorporating the following simplifications:
\begin{itemize}
    \item multiply out products outside sums
    \item combine products with child products
    \item combine products of constants
    \item combine sums with child sums
    \item combine sums of constants
    \item sort children
\end{itemize}
-}
toInner :: Expression -> InnerExpr
toInner (Expression n exprs) = toI n
  where
    toI n =
        case I.lookup n exprs of
            Just (RelElem idx bdy off) -> IERE idx bdy off
            Just (Const Dim0 d) -> IEConst d
            Just (Op Dim0 Sum args) -> simpSum $ map toI args
            Just (Op Dim0 Prod args) -> simpProd $ map toI args
            Just (Op Dim0 Neg [arg]) -> IENeg (toI arg)
            Just (Op Dim0 Abs [arg]) -> IEAbs (toI arg)
            Just (Op Dim0 Signum [arg]) -> IESignum (toI arg)
            Just (Op Dim0 Div [num, denom]) -> IEDiv (toI num) (toI denom)
            Just (Op Dim0 Sqrt [arg]) -> IESqrt (toI arg)
            Just (Op Dim0 Sin [arg]) -> IESin (toI arg)
            Just (Op Dim0 Cos [arg]) -> IECos (toI arg)
            Just (Op Dim0 Tan [arg]) -> IETan (toI arg)
            Just (Op Dim0 Exp [arg]) -> IEExp (toI arg)
            Just (Op Dim0 Log [arg]) -> IELog (toI arg)
            Just (Op Dim0 Sinh [arg]) -> IESinh (toI arg)
            Just (Op Dim0 Cosh [arg]) -> IECosh (toI arg)
            Just (Op Dim0 Tanh [arg]) -> IETanh (toI arg)
            Just (Op Dim0 Asin [arg]) -> IEAsin (toI arg)
            Just (Op Dim0 Acos [arg]) -> IEAcos (toI arg)
            Just (Op Dim0 Atan [arg]) -> IEAtan (toI arg)
            Just (Op Dim0 Asinh [arg]) -> IEAsinh (toI arg)
            Just (Op Dim0 Acosh [arg]) -> IEAcosh (toI arg)
            Just (Op Dim0 Atanh [arg]) -> IEAtanh (toI arg)
            x ->
                error $
                "HE.toInner " ++ show x ++ " ~ " ++ pretty (Expression n exprs)

{-
Simplify IESum
-}
simpSum [] = IEConst 0
simpSum children =
    let allArgs =
            concatMap
                (\n ->
                     case n of
                         IESum subArgs -> subArgs
                         _ -> [n])
                children
        c =
            sum $
            concatMap
                (\n ->
                     case n of
                         IEConst d -> [d]
                         _ -> [])
                allArgs
        nonConst =
            filter
                (\n ->
                     case n of
                         IEConst _ -> False
                         _ -> True)
                allArgs
     in IESum $
        L.sort $
        (if c == 0
             then id
             else ((IEConst c) :))
            nonConst {-trace ("simpSum "++show (c,children,nonConst)) $-}

{-
-}
simpProd [] = IEConst 1
simpProd children =
    let allArgs =
            concatMap
                (\n ->
                     case n of
                         IEProd subArgs -> subArgs
                         _ -> [n])
                children
        c =
            product $
            concatMap
                (\n ->
                     case n of
                         IEConst d -> [d]
                         _ -> [])
                allArgs
        nonConst =
            filter
                (\n ->
                     case n of
                         IEConst _ -> False
                         _ -> True)
                allArgs
        (sums, nonSums) =
            L.partition
                (\n ->
                     case n of
                         IESum _ -> True
                         _ -> False)
                nonConst
        terms =
            if c == 1
                then nonSums
                else (IEConst c) : nonSums
        prodSums ((IESum args1):(IESum args2):rest) =
            prodSums
                ((IESum [simpProd [arg1, arg2] | arg1 <- args1, arg2 <- args2]) :
                 rest)
        prodSums [IESum args1] = args1
        prodSums [] = []
        prodSums _x = error $ "HE.simpProd impossible " ++ show children
             {-trace ("simpProd "++show (children,terms,"simpSum",simpSum [simpProd $ arg : terms | arg <- prodSums sums])) $-}
     in if null sums
            then IEProd $ L.sort terms
            else simpSum [simpProd $ arg : terms | arg <- prodSums sums]

{-
-}
fromInner :: InnerExpr -> Expression
fromInner ie = Expression n exprs
  where
    (exprs, n) = fromI I.empty ie
    fromI :: Internal -> InnerExpr -> (Internal, Node)
    fromI exprs (IERE idx bdy off) = addEdge exprs $ RelElem idx bdy off
    fromI exprs (IEConst c) = addEdge exprs $ Const Dim0 c
    fromI exprs (IESum ies) =
        let (exprs', args) = L.mapAccumL fromI exprs ies
         in addEdge exprs' $ Op Dim0 Sum args
    fromI exprs (IEProd ies) =
        let (exprs', args) = L.mapAccumL fromI exprs ies
         in addEdge exprs' $ Op Dim0 Prod args
    fromI exprs (IENeg ie) =
        let (exprs', i) = fromI exprs ie
         in addEdge exprs' $ Op Dim0 Neg [i]
    fromI exprs (IEAbs ie) =
        let (exprs', i) = fromI exprs ie
         in addEdge exprs' $ Op Dim0 Abs [i]
    fromI exprs (IESignum ie) =
        let (exprs', i) = fromI exprs ie
         in addEdge exprs' $ Op Dim0 Signum [i]
    fromI exprs (IEDiv inum idenom) =
        let (exprs', num) = fromI exprs inum
            (exprs'', denom) = fromI exprs' idenom
         in addEdge exprs'' $ Op Dim0 Div [num, denom]
    fromI exprs (IESqrt ie) =
        let (exprs', i) = fromI exprs ie
         in addEdge exprs' $ Op Dim0 Sqrt [i]
    fromI exprs (IESin ie) =
        let (exprs', i) = fromI exprs ie
         in addEdge exprs' $ Op Dim0 Sin [i]
    fromI exprs (IECos ie) =
        let (exprs', i) = fromI exprs ie
         in addEdge exprs' $ Op Dim0 Cos [i]
    fromI exprs (IETan ie) =
        let (exprs', i) = fromI exprs ie
         in addEdge exprs' $ Op Dim0 Tan [i]
    fromI exprs (IEExp ie) =
        let (exprs', i) = fromI exprs ie
         in addEdge exprs' $ Op Dim0 Exp [i]
    fromI exprs (IELog ie) =
        let (exprs', i) = fromI exprs ie
         in addEdge exprs' $ Op Dim0 Log [i]
    fromI exprs (IESinh ie) =
        let (exprs', i) = fromI exprs ie
         in addEdge exprs' $ Op Dim0 Sinh [i]
    fromI exprs (IECosh ie) =
        let (exprs', i) = fromI exprs ie
         in addEdge exprs' $ Op Dim0 Cosh [i]
    fromI exprs (IETanh ie) =
        let (exprs', i) = fromI exprs ie
         in addEdge exprs' $ Op Dim0 Tanh [i]
    fromI exprs (IEAsin ie) =
        let (exprs', i) = fromI exprs ie
         in addEdge exprs' $ Op Dim0 Asin [i]
    fromI exprs (IEAcos ie) =
        let (exprs', i) = fromI exprs ie
         in addEdge exprs' $ Op Dim0 Acos [i]
    fromI exprs (IEAtan ie) =
        let (exprs', i) = fromI exprs ie
         in addEdge exprs' $ Op Dim0 Atan [i]
    fromI exprs (IEAsinh ie) =
        let (exprs', i) = fromI exprs ie
         in addEdge exprs' $ Op Dim0 Asinh [i]
    fromI exprs (IEAcosh ie) =
        let (exprs', i) = fromI exprs ie
         in addEdge exprs' $ Op Dim0 Acosh [i]
    fromI exprs (IEAtanh ie) =
        let (exprs', i) = fromI exprs ie
         in addEdge exprs' $ Op Dim0 Atanh [i]

{-
Relabel |RelElems| in |InnerExpr| so that the first one you see on the left is 0, ...
This makes these expressions unique.
-}
canonicalIE :: InnerExpr -> ((Int, I.IntMap Int), InnerExpr)
canonicalIE = cie (0, I.empty)

cie (next, remap) (IERE idx bdy off) =
    let (newIdx, (newNext, newRemap)) =
            case I.lookup idx remap of
                Nothing -> (next, (next + 1, I.insert idx next remap))
                Just newIdx -> (newIdx, (next, remap))
     in ((newNext, newRemap), IERE newIdx bdy off)
cie (next, remap) ie@(IEConst _) = ((next, remap), ie)
cie (next, remap) (IESum ies) =
    let ((newNext, newRemap), newIes) = L.mapAccumL cie (next, remap) ies
     in ((newNext, newRemap), IESum newIes)
cie (next, remap) (IEProd ies) =
    let ((newNext, newRemap), newIes) = L.mapAccumL cie (next, remap) ies
     in ((newNext, newRemap), IEProd newIes)
cie nr (IENeg ie) =
    let (nr', i) = cie nr ie
     in (nr', IENeg i)
cie nr (IEAbs ie) =
    let (nr', i) = cie nr ie
     in (nr', IEAbs i)
cie nr (IESignum ie) =
    let (nr', i) = cie nr ie
     in (nr', IESignum i)
cie (next, remap) (IEDiv inum idenom) =
    let ((newNext, newRemap), [newNum, newDenom]) =
            L.mapAccumL cie (next, remap) [inum, idenom]
     in ((newNext, newRemap), IEDiv newNum newDenom)
cie nr (IESqrt ie) =
    let (nr', i) = cie nr ie
     in (nr', IESqrt i)
cie nr (IESin ie) =
    let (nr', i) = cie nr ie
     in (nr', IESin i)
cie nr (IECos ie) =
    let (nr', i) = cie nr ie
     in (nr', IECos i)
cie nr (IETan ie) =
    let (nr', i) = cie nr ie
     in (nr', IETan i)
cie nr (IEExp ie) =
    let (nr', i) = cie nr ie
     in (nr', IEExp i)
cie nr (IELog ie) =
    let (nr', i) = cie nr ie
     in (nr', IELog i)
cie nr (IESinh ie) =
    let (nr', i) = cie nr ie
     in (nr', IESinh i)
cie nr (IECosh ie) =
    let (nr', i) = cie nr ie
     in (nr', IECosh i)
cie nr (IETanh ie) =
    let (nr', i) = cie nr ie
     in (nr', IETanh i)
cie nr (IEAsin ie) =
    let (nr', i) = cie nr ie
     in (nr', IEAsin i)
cie nr (IEAcos ie) =
    let (nr', i) = cie nr ie
     in (nr', IEAcos i)
cie nr (IEAtan ie) =
    let (nr', i) = cie nr ie
     in (nr', IEAtan i)
cie nr (IEAsinh ie) =
    let (nr', i) = cie nr ie
     in (nr', IEAsinh i)
cie nr (IEAcosh ie) =
    let (nr', i) = cie nr ie
     in (nr', IEAcosh i)
cie nr (IEAtanh ie) =
    let (nr', i) = cie nr ie
     in (nr', IEAtanh i)

{-
Normalize an SCZ before committing it to the graph.
Make replace an empty expression with a zero constant, or a constant expression with that constant.
-}
mkSCZ' :: Dims -> Expression -> [Node] -> ExpressionEdge
mkSCZ' dims expr args =
    if null newArgs
        then case newInner of
                 IEConst c -> Const dims c
                 _ ->
                     error $
                     "HE.mkSCZ no args but not a constant! Try simplifying. " ++
                     show (dims, expr, args, newInner)
                        {-trace (pretty expr ++ show newInner ++ pretty (fromInner newInner))
                        $ -}
        else Op dims (SCZ $ fromInner newInner) newArgs
  where
    ((_numREs, remap), newInner) = canonicalIE $ toInner expr
    newArgs =
        map snd $
        L.sort $
        concatMap
            (\(arg, idx) ->
                 case I.lookup idx remap of
                     Just newIdx -> [(newIdx, arg)]
                     _ -> []) $
        zip args [0 ..]

{-
Allowable permutations come from permutations of arguments of |Sum| and |Prod|,
so we only need to look at those nodes to restrict the permutations.
Permutations can be represented as 0/1 matrices, and information from nodes
puts restrictions on off-diagonal 1s.
At each |Sum| or |Prod| node, the set of |RelElem|s with a given offset and boundary
have no communication with other nodes, so we can eliminate an off-diagonal rectangle.
Children of such nodes which follow the same path produce similar restrictions,
however, the restrictions, however, the permutation at the higher level may operate on a block,
ie |Sum [Sin [Sum [0,1]],Sin [Sum [2,3]]]| admits permutations
0010
0001
1000
0100
,
0100
1000
0010
0001
and products
, which is isometric to Z/2 x Z/2 x Z/2
So if |RelElem|s are at the top level, they box out communication, but lower down, they box out communication but have to be taken in the context of block permutations at the higher level.
Because of simplification, we only have a Sum of Prods at the top, if there is no sum at top, you need to go into a non-linear operation to get one.
NOTE:  We don't need to get all permutations, we can still make IntMaps smaller!
FIXME:  For now we will not consider permutations, and see what impact that has.

The |Expression| is a compound node with as many components as the size of |convOutDims|,
which are each arrays with the given dimensions.
CKA:  Are multiple outputs really supported?
The inputs are variables
which should either be in the surrounding scope,
\textbf{CKA:  this is danger for garbage collection}
or be |RelElem| variables.
For |RelElem|, |Boundary| specifies what to do if the relative index goes outside
the boundary of the array.
-}
data Boundary
    = Reflective -- values are reflected in the boundaries 1/2 an element outside the array
    | Torus -- positions are calulated using modulo arithmetic in each index
    | ZeroMargin -- values at positions outside array are treated as zero
    | ConstMargin Double -- values at postions outside the array are treated as the given constant
    deriving (Eq, Show, Ord, Read)

{-

-}
class ZeroIdx a where
    zeroIdx :: a

instance ZeroIdx Int where
    zeroIdx = 0

instance ZeroIdx (Int, Int) where
    zeroIdx = (0, 0)

instance ZeroIdx (Int, Int, Int) where
    zeroIdx = (0, 0, 0)

instance ZeroIdx (Int, Int, Int, Int) where
    zeroIdx = (0, 0, 0, 0)

{-

-}
data Airity
    = Natural
    | Fixed Int
    | IsMap (Airity, Airity)
    deriving (Eq, Read, Show, Ord)

class HasHash a where
    hash :: a -> Node

instance HasHash Scalar where
    hash (Scalar e) = hash e

instance HasHash Expression where
    hash (Expression n _) = n

--  JLMP : make hash depend on some arguments to reduce clashes
--         and benchmark to make sure this really helps
instance HasHash OpId where
    hash Sum = 2131
    hash Neg = 2293
    hash SubMask = 2749
    hash Prod = 2437
    hash Div = 2621
    hash NegMask = 2909
    hash (Reglzr dw rk) = 281 * (C.foldr' fun 0 $ C.pack $ show (dw, rk))
      where
        fun :: Char -> Int -> Int
        fun c hash = hash * 40591 + (fromEnum c)
    hash (GradReglzr dw rk) = 283 * (C.foldr' fun 0 $ C.pack $ show (dw, rk))
      where
        fun :: Char -> Int -> Int
        fun c hash = hash * 40591 + (fromEnum c)
    hash Sqrt = 3083
    hash Sin = 1009
    hash Cos = 1013
    hash Tan = 1019
    hash Exp = 1031
    hash Log = 1033
    hash Sinh = 1039
    hash Cosh = 1049
    hash Tanh = 1051
    hash Asin = 1061
    hash Acos = 1063
    hash Atan = 1069
    hash Asinh = 1087
    hash Acosh = 1091
    hash Atanh = 1093
    hash Dot = 3187
    hash (MapND _ _) = 3259
    hash ScaleV = 3343
    hash RealPart = 223
    hash ImagPart = 227
    hash RealImag = 229
    hash (FT True) = 5099
    hash (FT False) = 1021
    hash (PFT True Row) = 257
    hash (PFT True Column) = 761
    hash (PFT True Slice) = 769
    hash (PFT False Row) = 263
    hash (PFT False Column) = 773
    hash (PFT False Slice) = 787
    hash (Transpose SCR) = 269
    hash (Transpose SCSR) = 751
    hash (Transpose SCRS) = 757
    hash (Transpose SSRC) = 797
    hash (Transpose SSCR) = 809
    hash (Transpose SRSC) = 743
    hash (Compound _) = 593
    hash (Extract _) = 719
    hash (Project _) = 37
    hash (Inject _) = 41
    hash (SCZ e) = 239 + 393919 * (hash e)
    hash x = error $ "opHash unsupported op " ++ show x

{-

Checks if a node is complex
-}
nodeIsComplex :: Internal -> Node -> Bool
nodeIsComplex edges top =
    case I.lookup top edges of
        Nothing -> error $ "nodeIsComplex not top " ++ show (top, edges)
        Just (Op _dim op args) ->
            case op of
                Sqrt ->
                    if (any (nodeIsComplex edges) args)
                        then error "Sqrt on complex number node"
                        else False
                Asin ->
                    if (any (nodeIsComplex edges) args)
                        then error "Asin on complex number node"
                        else False
                Acos ->
                    if (any (nodeIsComplex edges) args)
                        then error "Acos on complex number node"
                        else False
                Atan ->
                    if (any (nodeIsComplex edges) args)
                        then error "Atan on complex number node"
                        else False
                Asinh ->
                    if (any (nodeIsComplex edges) args)
                        then error "Asinh on complex number node"
                        else False
                Acosh ->
                    if (any (nodeIsComplex edges) args)
                        then error "Acosh on complex number node"
                        else False
                Atanh ->
                    if (any (nodeIsComplex edges) args)
                        then error "Atanh on complex number node"
                        else False
                Dot -> any (nodeIsComplex edges) args
                FT _ -> True
                RealPart -> False
                ImagPart -> False
                Reglzr _ _ -> False
                GradReglzr _ _ -> False -- FIXME it could be complex if there all the inputs are complex
                RealImag -> True
                _ -> any (nodeIsComplex edges) args
        Just _ -> False

nodeIsComplex' (Expression n e) = nodeIsComplex e n

{-

Compare nodes according to content, with scaling effecting ordering, but less strongly
than the nodes being scaled.
-}
compareOp :: Internal -> Node -> Node -> Ordering
compareOp exprs' left right =
    let (exprs, one) = addEdge exprs' $ Const Dim0 1
     in case (I.lookup left exprs, I.lookup right exprs) of
            (Just (Op _ ScaleV [sL, nL]), Just (Op _ ScaleV [sR, nR])) ->
                case compareOp exprs nL nR of
                    EQ -> compareOp exprs sL sR
                    x -> x
            (Just (Op _ ScaleV [sL, nL]), _) ->
                case compareOp exprs nL right of
                    EQ -> compareOp exprs sL one
                    x -> x
            (_, Just (Op _ ScaleV [sR, nR])) ->
                case compareOp exprs left nR of
                    EQ -> compareOp exprs one sR
                    x -> x
            (Just (Op _ opL nL), Just (Op _ opR nR)) ->
                case compare opL opR of
                    EQ ->
                        case dropWhile (== EQ) $ zipWith (compareOp exprs) nL nR of
                            [] -> EQ
                            (c:_) -> c
                    c -> c
            (_, Just (Op _ _opR _)) -> GT
            (Just (Op _ _opL _), _) -> LT
            (x, y) -> compare x y

{-

Checks if a node is complex scalar
-}
nodeIsScalarC :: Internal -> Node -> Bool
nodeIsScalarC edges top = (nodeIsZeroDim edges top) && (nodeIsComplex edges top)

{-

Checks if a node is scalar
-}
nodeIsScalar :: Internal -> Node -> Bool
nodeIsScalar edges top =
    (nodeIsZeroDim edges top) && (not $ nodeIsComplex edges top)

{-

Checks if a node is zero
-}
nodeIsZeroDim :: Internal -> Node -> Bool
nodeIsZeroDim edges top =
    case I.lookup top edges of
        Nothing -> error $ "nodeIsScalar not top " ++ show (top, edges)
        Just (Op _dim op args) ->
            case op of
                Dot -> True
                FT _ -> False
                _ -> L.and $ map (nodeIsScalar edges) args
        Just _ -> False

{-

Checks if a node is a differential
-}
nodeIsDifferential :: Internal -> Node -> Bool
nodeIsDifferential edges top =
    case I.lookup top edges of
        Nothing -> error $ "nodeIsDifferential not top " ++ show (top, edges)
        Just (DVar _ _) -> True
        Just _ -> False

{-

Checks if a node is a |RelElem|
-}
nodeIsRelElem :: Internal -> Node -> Bool
nodeIsRelElem edges node =
    case I.lookup node edges of
        Nothing -> error $ "nodeIsRelElem not top " ++ show (node, edges)
        Just (RelElem _ _ _) -> True
        Just _ -> False

{-

-}
airity :: OpId -> (Airity, Airity)
airity Sum = (Natural, Fixed 1)
airity SubMask = (Fixed 2, Fixed 1)
airity NegMask = (Fixed 2, Fixed 1)
airity (Reglzr _ _) = (Natural, Fixed 1)
airity (GradReglzr _ _) = (Natural, Fixed 1)
airity Prod = (Natural, Fixed 1)
airity Div = (Fixed 2, Fixed 1)
airity Neg = (Fixed 1, Fixed 1)
airity Abs = (Fixed 1, Fixed 1)
airity Signum = (Fixed 1, Fixed 1)
airity Sin = (Fixed 1, Fixed 1)
airity Cos = (Fixed 1, Fixed 1)
airity Tan = (Fixed 1, Fixed 1)
airity Exp = (Fixed 1, Fixed 1)
airity Log = (Fixed 1, Fixed 1)
airity Sinh = (Fixed 1, Fixed 1)
airity Cosh = (Fixed 1, Fixed 1)
airity Tanh = (Fixed 1, Fixed 1)
airity Asin = (Fixed 1, Fixed 1)
airity Acos = (Fixed 1, Fixed 1)
airity Atan = (Fixed 1, Fixed 1)
airity Asinh = (Fixed 1, Fixed 1)
airity Acosh = (Fixed 1, Fixed 1)
airity Atanh = (Fixed 1, Fixed 1)
airity Sqrt = (Fixed 1, Fixed 1)
airity Dot = (Fixed 2, Fixed 1)
airity (MapND _ _) = (Fixed 1, Fixed 1)
airity ScaleV = (Fixed 2, Fixed 1)
airity (FT _) = (Fixed 1, Fixed 1) -- takes complex input node and produces complex (compound) node
airity (PFT _ _) = (Fixed 1, Fixed 1)
airity (SCZ _) = (Natural, Fixed 1)
airity (Compound _) = (Natural, Fixed 1)
airity (Extract _) = (Fixed 1, Fixed 1)
airity (Project _) = (Fixed 1, Fixed 1)
airity (Inject _) = (Fixed 1, Fixed 1)
airity RealPart = (Fixed 1, Fixed 1)
airity ImagPart = (Fixed 1, Fixed 1)
airity RealImag = (Fixed 2, Fixed 1)
airity (Transpose _) = (Fixed 1, Fixed 1)

--airity x = error $ "airity not implemented for " ++ show x
{-

*** later, we want expressions to have tuple outputs, but this probably needs a new type class.

++++++++++++++++++++++++++++++++++++++++++++  MurmurHash
switching to MurmurHash causes, we need better tests before implementing this.
*** Exception: HP2.buffer propogation conflict at (-318771984,2104020358) with ((RelOffset 3968 + 2228224 * num_slices,57344),(RelOffset 61312 + 2228224 * num_slices,57344))
also need
  hash (SCZ e) = hash32AddWord32 239 (hash e)


FIXME:  not finished implementing new Hash
FIXME:  benchmark new versus old Hash
FIXME:  try switching to ByteString from ByteString.Char
%\begin{code}
instance HasHash ExpressionEdge where
  hash ee = case ee of
      (Var _dim name) -> C.foldr' fun 1 name
      (DVar _dim name) -> C.foldr' fun 1123 name
      (RelElem a _ idxs) -> foldr hash32AddWord32 479001599 (map fromIntegral (a:idxs))
-- CKA many clashes, use for testing confluence        + (sum $ zipWith (\ i off -> 181 * (i + 191 * (off )) ) [1..] idxs)
   --     + (sum $ zipWith (\ i off -> 181 * (191^i * (off )) ) [1..] idxs)
--      (Const dim d) -> 919393 + (C.foldr' fun 0 $ C.pack $ show (d,dim))
      (Const dim d) -> foldr hash32AddWord32 (hashDouble d) (map fromIntegral $ dimList dim)
      (Op _ op args) -> let
          (airIn, airOut) = case airity op of
            (Natural, Fixed 1) -> (length args,1)
            (Fixed n, Fixed 1) -> if length args == n then (n,1)
                                  else error $ "eHash airity mismatch "++show (op, airity op, length args)
            _ -> error $ "eHash unsupported airity "++show (op, airity op, length args)
        in if airIn*airOut==0  then error $ "hash bad airities "++show (airIn,airOut,ee)
                               else argHash op args
    where
      fun :: Char -> Int -> Int
      fun c hash = hash32AddWord32 (fromIntegral $ fromEnum c) hash
      argHash op (arg:args) = hash32AddWord32 (fromIntegral arg) (argHash op args)
      argHash op [] = hash op
%\end{code}

implementation of MurmurHash2 (http://murmurhash.googlepages.com)
based on Data.Digest.Murmur32 by Thomas Schilling © 2010, BSD license
FIXME -- this is assuming Int == Int32, but this won't always be the case, probably need CPP to switch between Hash32 and Hash64
%\begin{code}
hash32AddWord32 :: Word32 -> Int -> Int
hash32AddWord32 k h =
  let h' = fromIntegral h
      k1 = k * murmur_m
      k2 = k1 `xor` (k1 `shiftR` murmur_r)
      k3 = k2 * murmur_m
      h1 = h' * murmur_m
      h2 = h1 `xor` k3
  in fromIntegral h2

hashInteger k = foldr hash32AddWord32 (if k < 0 then 271 else 277) (word32s $ abs k)
  where
    word32s 0 = []
    word32s x = (fromInteger x) : word32s (x `shiftR` 32)

hashDouble d = let dRat = toRational d in hash32AddWord32 (fromIntegral $ hashInteger $ numerator dRat) (hashInteger $ denominator dRat)

murmur_m :: Word32
murmur_m = 0x5bd1e995

murmur_r :: Int
murmur_r = 24
%\end{code}
++++++++++++++++++++++++++++++++++++++++++++  MurmurHash

-}
instance HasHash ExpressionEdge where
    hash ee =
        case ee of
            (Var _dim name) -> C.foldr' fun 0 name
            (DVar _dim name) -> C.foldr' fun 1123 name
            (RelElem a _ idxs) ->
                479001599 + a * 179 +
                (sum $ zipWith (\i off -> 181 * (191 ^ i * (off))) [1 ..] idxs)
            (Const dim d) -> 919393 + (C.foldr' fun 0 $ C.pack $ show (d, dim))
            (Op _ op args) ->
                let (airIn, airOut) =
                        case airity op of
                            (Natural, Fixed 1) -> (length args, 1)
                            (Fixed n, Fixed 1) ->
                                if length args == n
                                    then (n, 1)
                                    else error $
                                         "eHash airity mismatch " ++
                                         show (op, airity op, length args)
                            _ ->
                                error $
                                "eHash unsupported airity " ++
                                show (op, airity op, length args)
                 in if airIn * airOut == 0
                        then error $
                             "hash bad airities " ++ show (airIn, airOut, ee)
                        else (hash op) * (1 + (argHash args))
      where
        fun :: Char -> Int -> Int
        fun c hash = hash * 40591 + (fromEnum c)
        argHash (arg:args) = arg + 31 * (argHash args)
        argHash [] = 0

-- CKA many clashes, use for testing confluence        + (sum $ zipWith (\ i off -> 181 * (i + 191 * (off )) ) [1..] idxs)
{-


Merge two expression graphs.
\savecolumns
-}
merge :: Expression -> Expression -> (Expression, (Node, Node))
merge e1@(Expression head0 expr0) e2@(Expression head1 expr1) =
    (Expression (error "merge Expression don't use node") expr', (h0, h1))
  where
    (exprs, newExprs, newHead, (h0, h1)) =
        if I.size expr0 > I.size expr1
            then (expr0, expr1, head1, (head0, remapIfNecessary head1))
            else (expr1, expr0, head0, (remapIfNecessary head0, head1))
    (allRemaps, expr') =
        foldr remap (I.empty, exprs) $ reverse $ depthFirst newExprs newHead
    remap (node, edge) (rehashed, exprs) =
        ( if newHash == node
              then rehashed
              else I.insertWithKey
                       (\k a b ->
                            if a == b
                                then a
                                else error $ "rehash " ++ show (k, a, b, e1, e2))
                       node
                       newHash
                       rehashed
        , vces)
      where
        (vces, newHash) = addEdge exprs $ remapNodes rehashed edge
    remapIfNecessary node =
        let result = I.findWithDefault node node allRemaps
         in result

{-
Add edges from the smaller expression to the larger.
\restorecolumns
-}
{-
Normally variables map to unique hashes, but collisions are possible in one
expression but not the other, so we need to rewrite when this happens.
\restorecolumns
-}
{-

Merge list of expression graphs.
\savecolumns
-}
mergeL' ens =
    let (Expression _ e, ns) = mergeL (map (\(e', n') -> Expression n' e') ens)
     in (e, ns)

mergeL :: [Expression] -> (Expression, [Node])
mergeL eList =
    ( Expression (error "merge Expression don't use node") es
    , map fst $ L.sortBy (\(_, x) (_, y) -> compare x y) remappedNodes)
  where
    ((Expression n0 exprs, idx0):eSorted) =
        L.sortBy
            (\(Expression _ e1, _) (Expression _ e2, _) ->
                 compare (I.size e2) (I.size e1)) $
        zip eList [0 ..]
    (remappedNodes, es) = foldr remapExpression ([(n0, idx0)], exprs) eSorted
    remapExpression (Expression newHead newExprs, idx) (nodes, vces) =
        let (allRemaps, vces') =
                foldr remap (I.empty, vces) $
                reverse $ depthFirst newExprs newHead
         in ( nodes ++ [(I.findWithDefault newHead newHead allRemaps, idx)]
            , vces')
    remap (node, edge) (rehashed, exprs) =
        ( if newHash == node
              then rehashed
              else I.insertWithKey
                       (\k a b ->
                            if a == b
                                then a
                                else error $
                                     "HE.remap " ++ show (k, a, b, eList))
                       node
                       newHash
                       rehashed
        , vces)
      where
        (vces, newHash) = addEdge exprs $ remapNodes rehashed edge

{-
Sort expressions descending by the size of the hash tables.
\restorecolumns
-}
{-
Add edges from the smaller expressions to the larger.
\restorecolumns
-}
{-
Normally variables map to unique hashes, but collisions are possible in one
expression but not the other, so we need to rewrite when this happens.
\restorecolumns
-}
{-

Change names of nodes which were renamed because of collisions with previous hashes.
-}
remapNodes theMap edge =
    case edge {- trace (if I.size theMap  > 0 then "remapNodes " ++ show (edge,theMap) else "" ) $ -}
          of
        (Op dim op args) -> Op dim op $ map mapNode args
        e -> e
  where
    mapNode node =
        case I.lookup node theMap of
            Nothing -> node
            Just newNode -> newNode

{-

Recreate a post-simplified expression with new hashes (as if was created from scratch as simplified)
-}
recreate' (Expression n e) =
    let (e', n') = recreate (e, n)
     in Expression n' e'

recreate :: (Internal, Node) -> (Internal, Node)
recreate (eOld, headOld) =
    let (eNew, old2new) =
            foldr remap (I.empty, I.empty) (reverse $ topSort eOld headOld)
        remap :: Node -> (Internal, I.IntMap Node) -> (Internal, I.IntMap Node)
        remap nOld (exprs, old2new) =
            let (exprs2, nNew) =
                    case I.lookup nOld eOld of
                        Nothing ->
                            error $
                            "recreate " ++
                            show nOld ++ " not in old expression "
            -- if op then find new remap for the args and add the edge to the new map
                        Just (Op dims op args)
                -- get new list of args (with their new hashes)
                         ->
                            let args' =
                                    map
                                        (flip
                                             (I.findWithDefault
                                                  (error $
                                                   "HE.recreate arg remap " ++
                                                   show
                                                       ( args
                                                       , eOld
                                                       , old2new
                                                       , reverse $
                                                         topSort eOld headOld) ++
                                                   pretty (eOld, headOld)))
                                             old2new)
                                        args
                             in addEdge exprs (Op dims op args') --}
                        Just e -> addEdge exprs e
             in ( exprs2
                , I.insertWith
                      (\a b ->
                           if a == b
                               then a
                               else error $
                                    "HE.recreate mismatch " ++
                                    show
                                        ( nOld
                                        , nNew
                                        , exprs
                                        , pretty (eOld, headOld)))
                      nOld
                      nNew
                      old2new)
     in ( eNew
        , I.findWithDefault
              (error $ "HE.recreate missing head " ++ show (headOld, old2new))
              headOld
              old2new)

{-


Prime numbers from \texttt{http://en.wikipedia.org/wiki/List_of_prime_numbers}.
-}
somePrimes =
    [ 293
    , 307
    , 311
    , 313
    , 317
    , 331
    , 337
    , 347
    , 349
    , 353
    , 359
    , 367
    , 373
    , 379
    , 383
    , 389
    , 397
    , 401
    , 409
    , 419
    , 421
    , 431
    , 433
    , 439
    , 443
    , 449
    , 457
    , 461
    , 463
    , 467
    , 479
    , 487
    , 491
    , 499
    , 503
    , 509
    , 521
    , 523
    , 541
    , 547
    , 557
    , 563
    , 569
    , 571
    , 577
    , 587
    , 599
    , 601
    , 607
    , 613
    , 617
    , 619
    , 631
    , 641
    , 643
    , 647
    , 653
    , 659
    , 661
    , 673
    , 677
    , 683
    , 691
    , 701
    , 709
    , 727
    ]

otherPrimes = [1097, 1103, 1109, 1117, 40099, 42589, 933199, 87178291199]

{-

Add an edge (not necessarily the head)
-}
addEdge' :: Internal -> ExpressionEdge -> Expression
addEdge' e edge = (\(es, n) -> Expression n es) $ addEdge e edge

addEdge'' exprs edge =
    let (exprs', n) = addEdge exprs edge
     in (exprs', (getDimE exprs' n, n))

addEdges exprs (edge:edges) =
    let (exprs', n) = addEdge'' exprs edge
        (exprs'', nodes) = addEdges exprs' edges
     in (exprs'', n : nodes)
addEdges exprs [] = (exprs, [])

addEdge :: Internal -> ExpressionEdge -> (Internal, Node)
addEdge exprs edge'
   {- (\en -> trace ("addEdge "++pretty en) en) $ -}
 =
    let edge =
            case edge'
           -- have to remove edges which would cause us problems with hash, and we can also avoid adding edges which will be simplified out later
                  of
                (Op Dim0 Sum []) -> Const Dim0 0
                (Op dims Sum []) -> Const dims 0
                x -> x
     in case dropWhile (== IsClash) $
             map (isClash exprs edge) $ rehash edge' $ hash edge of
            ((IsDuplicate h):_) -> (exprs, h)
            ((IsNew h):_) -> (insertNewI "addEdge.Edge" exprs (h, edge), h)
            _ -> error "addEdge everything clashed!"

{-
Add an edge and figure out the right dimensions for it.
-}
addEdgeD :: Internal -> OpId -> [Node] -> (Internal, Node)
addEdgeD exprs op@(Project ss@(SSCrop _ _)) [node] =
    addEdge exprs $ Op (projDim (getDimE exprs node) ss) op [node]
addEdgeD exprs op@(Inject ss@(SSCrop _ _)) [node] =
    addEdge exprs $ Op (injectDim (getDimE exprs node) ss) op [node]
addEdgeD exprs Dot [n1, n2] = addEdge exprs $ Op Dim0 Dot [n1, n2]
addEdgeD exprs op@(Transpose swap) [node] =
    let dims =
            case (swap, getDimE exprs node) of
                (SCR, Dim2 (d1, d2)) -> Dim2 (d2, d1)
                (SCRS, Dim3 (d1, d2, d3)) -> Dim3 (d2, d1, d3) -- swap first 2 indices of 3
                (SCSR, Dim3 (d1, d2, d3)) -> Dim3 (d3, d1, d2) -- rotate forward 3 indices
                (SSRC, Dim3 (d1, d2, d3)) -> Dim3 (d2, d3, d1) -- rotate backward 3 indices
                (SSCR, Dim3 (d1, d2, d3)) -> Dim3 (d3, d2, d1) -- swap first and last of 3
                (SRSC, Dim3 (d1, d2, d3)) -> Dim3 (d1, d3, d2)
                (swap, dim) ->
                    error $
                    "addEdgeD got illegal transpose combo " ++
                    show (dim, swap, pretty (exprs, node))
     in addEdge exprs $ Op dims op [node]
addEdgeD exprs op args@(arg:_) = addEdge exprs $ Op (getDimE exprs arg) op args
addEdgeD _exprs op [] =
    error $ "addEdgeD doesn't handle nullary operations" ++ show op

{-
Add a |Prod|, a |Prod| followed by a |Scale| or a |Scale| or a scalar |Const 0|.
-}
genProd :: Internal -> [Node] -> (Internal, Node)
genProd expr nodes =
    case nodes of
        [] -> addEdge expr $ Const Dim0 0 -- genProd only works if you have a nonzero product, or a scalar
        [node] -> (expr, node)
        [node1, node2] ->
            case (getDimE expr node1, getDimE expr node2) of
                (Dim0, Dim0) -> addEdge expr $ Op Dim0 Prod [node1, node2]
                (Dim0, dims) -> addEdge expr $ Op dims ScaleV [node1, node2]
                (dims, Dim0) -> addEdge expr $ Op dims ScaleV [node2, node1]
                _ ->
                    error $
                    "prodScale dims2 " ++ show (map (getDimE expr) nodes, expr)
        _ ->
            let dimss = zip (map (getDimE expr) nodes) nodes
                (scalars, nonscalars) = L.partition ((Dim0 ==) . fst) dimss
             in case (scalars, nonscalars) of
                    (_, []) -> addEdge expr $ Op Dim0 Prod nodes
                    (_, [(dims, nonscalar)]) ->
                        let (e1, sN) =
                                addEdge expr $ Op Dim0 Prod $ map snd scalars
                         in addEdge e1 $ Op dims ScaleV [sN, nonscalar]
                    _ -> error $ "prodScale dimsN " ++ show (nodes, expr)

{-
-}
rehash :: ExpressionEdge -> Node -> [Node]
rehash e x =
    x :
    [mt ("rehash " ++ show (i, x, e)) $ x + (241 + x * 251) * i | i <- [1 ..]]

data IsClash
    = IsClash
    | IsDuplicate Node
    | IsNew Node
    deriving (Eq, Show, Ord)

{-

Figure out if this edge is in the map already.
-}
isClash :: Internal -> ExpressionEdge -> Node -> IsClash
isClash exprs newEdge newHash =
    case I.lookup newHash exprs of
        Nothing -> IsNew newHash
        Just old ->
            if old == newEdge
                then IsDuplicate newHash
                else IsClash

{-

When we know that keys don't exist in maps:
-}
insertNew err theMap (key, val) = Map.insertWith (error err) key val theMap

insertNewI err theMap (key, val) =
    I.insertWith (\_ _ -> error $ err ++ show (key, val, theMap)) key val theMap

{-

Topologically sorted list of nodes, top is last.
Note that this assumes it is a DAG, see https://en.wikipedia.org/wiki/Topological_sorting to use two sets to also test for this property.
CKA:  Somebody should see if http://hackage.haskell.org/packages/archive/containers/latest/doc/html/src/Data-Graph.html#topSort is faster.
-}
topSort :: Internal -> Node -> [Node]
topSort edges top = reverse $ snd $ addDependencies top (Set.empty, [])
  where
    addDependencies :: Node -> (Set.Set Node, [Node]) -> (Set.Set Node, [Node])
    addDependencies newNode (visited, inOrder) =
        let dependencies =
                case I.lookup newNode edges of
                    Just (Op _ _ args) -> args
                    _ -> []
         in if newNode `Set.member` visited
                then (visited, inOrder)
                else let (newVisited, newInOrder) =
                             foldr
                                 addDependencies
                                 (visited, inOrder)
                                 dependencies
                      in (Set.insert newNode newVisited, newNode : newInOrder)

topSort' (exprs, node) = topSort exprs node

{-
TODO:  use IntSet instead of Set
Compute consumers of each reachable node to make it easier to traverse expression DAG.
-}
type Consumers = I.IntMap (Set.Set Node)

allConsumers (exprs, head) = allMap
  where
    cMap = consumers (exprs, head)
    nodes = reverse $ topSort exprs head
    allMap =
        I.fromList $
        (head, Set.empty) :
        (map (\s ->
                  ( s
                  , Set.unions $
                    map (err2 . flip I.lookup allMap) $
                    Set.toList $ err1 $ I.lookup s cMap))
             nodes)
    err1 x =
        case x of
            Just x' -> x'
            _ ->
                error $
                "allConsumers 1 " ++ show (head, nodes, I.keys cMap, cMap)
    err2 x =
        case x of
            Just x' -> x'
            _ -> error "allConsumers 2"

consumers :: (Internal, Node) -> I.IntMap (Set.Set Node)
consumers (exprs, head) =
    I.unionWith
        (Set.union)
        (I.unionsWith
             (Set.union)
             (map (\n -> add_node head n (I.fromList [(head, Set.empty)]))
                  consumeds))
        (I.unionsWith (Set.union) (map (\n -> consumers (exprs, n)) consumeds))
  where
    consumeds =
        case I.lookup head exprs of
            Just (Op _ _ args) -> args
            _ -> []
    add_node ::
           Node -> Node -> I.IntMap (Set.Set Node) -> I.IntMap (Set.Set Node)
    add_node consumer consumed i =
        if I.notMember consumed i
            then I.insert consumed (Set.insert consumer Set.empty) i
            else let ccs =
                         case I.lookup consumed i of
                             Nothing ->
                                 error $
                                 "Consumers.this is impossible since already checked is member"
                             Just s -> s
                     new_ccs = Set.insert consumer ccs
                  in I.insert consumed new_ccs i

{-

Calculate depth of all edges to all the dependent variables.
A cycle will produce an infinite list.
If we built up an IntMap of them, we wouldn't have to worry about cycles.
LATER: pass in an edge weight
-}
depthMap ::
       (Internal, Node)
    -> I.IntMap (Map.Map ByteString Int, I.IntMap (Map.Map ByteString Int))
depthMap (exprs, head) =
    fst $ dM (error "depthMap top level") exprs I.empty head
  where
    dM :: (Int -> String -> Map.Map ByteString Int)
       -> Internal
       -> I.IntMap (Map.Map ByteString Int, I.IntMap (Map.Map ByteString Int))
       -> Node
       -> ( I.IntMap (Map.Map ByteString Int, I.IntMap (Map.Map ByteString Int))
          , Map.Map ByteString Int)
    dM reFun exprs dMap node =
        case I.lookup node dMap of
            Just (dMapNode, _insideSCZ) -> (dMap, dMapNode) -- we have already done this node
            Nothing ->
                case I.lookup node exprs of
                    Just (Op _ (SCZ (Expression sczN sczE)) args) ->
                        let (dMapNew, _dMapNodes) =
                                L.mapAccumR
                                    (dM (error "dM.scz") exprs)
                                    dMap
                                    args
                            idxArgs = I.fromList $ zip [0 ..] args
                            rEf idx e =
                                case I.lookup idx idxArgs of
                                    Just arg ->
                                        case I.lookup arg dMapNew of
                                            Just (deps, _) -> deps
                                            Nothing ->
                                                error $
                                                "depthMap lookup arg" ++ e
                                    Nothing ->
                                        error $ "depthMap lookup idx" ++ e
                            (sczDM, dMapUnion) = dM rEf sczE I.empty sczN
                         in ( (I.insert
                                   node
                                   (dMapUnion, I.map fst sczDM)
                                   dMapNew)
                            , dMapUnion)
                    Just (Op _ op args) ->
                        let (dMapNew, dMapNodes) =
                                L.mapAccumR (dM reFun exprs) dMap args
                            dMapUnion =
                                Map.map (+ (opDepth op)) $
                                Map.unionsWith max dMapNodes :: Map.Map ByteString Int
                         in ( I.insert node (dMapUnion, I.empty) dMapNew
                            , dMapUnion)
                    Just (Var _ name) ->
                        ( I.insert node (Map.singleton name 0, I.empty) dMap
                        , Map.singleton name 0)
                    Just (DVar _ name) ->
                        error $ "P.depthMap DVar " ++ show name
                    Just (RelElem idx _ _) ->
                        let dMapRE = reFun idx (show node)
                         in (I.insert node (dMapRE, I.empty) dMap, dMapRE)
                    Just (Const _ _) ->
                        (I.insert node (Map.empty, I.empty) dMap, Map.empty)
                    Nothing ->
                        error $
                        "P.depthMap node not found " ++ show (node, dMap, exprs)

opDepth (FT _) = 10000
opDepth (PFT _ _) = 100 * 8
opDepth (Transpose _) = 100
opDepth (Sum) = 100
opDepth (SubMask) = 100
opDepth (NegMask) = 100
opDepth (Dot) = 100
opDepth (MapND _ _) = 1000
opDepth (SCZ _) = 1000
opDepth (ScaleV) = 1
opDepth _ = 0

{-

Depth-first list of edges in a DAG.  A cycle will produce an infinite list.
If we built up an IntMap of them, we wouldn't have to worry about cycles.
-}
depthFirst :: Internal -> Node -> [(Node, ExpressionEdge)]
depthFirst edges top = (concatMap (depthFirst edges) lower) ++ [(top, topEdge)]
  where
    (topEdge, lower) =
        case I.lookup top edges of
            Nothing -> error $ "depthFirst not top " ++ show (top, edges)
            Just e@(Op _dim _op args) -> (e, args)
            Just e -> (e, [])

{-

Breadth-first list of edges in a DAG.  A cycle will produce an infinite list.
If we built up an IntMap of them, we wouldn't have to worry about cycles.
-}
breadthFirst :: Internal -> Node -> [(Node, ExpressionEdge)]
breadthFirst edges top = (top, topEdge) : (concatMap (breadthFirst edges) lower)
  where
    (topEdge, lower) =
        case I.lookup top edges of
            Nothing -> error $ "breadthFirst not top " ++ show (top, edges)
            Just e@(Op _dim _op args) -> (e, args)
            Just e -> (e, [])

{-


Predicates
-}
isVar, isOp, isRealOp, isConstEdge :: ExpressionEdge -> Bool
isVar (Var _ _) = True
isVar _ = False

maybeVarNode e n =
    case I.lookup n e of
        Nothing -> error $ "HE.isVarNode no node " ++ show (n, e)
        Just (Var _ name) -> Just name
        _ -> Nothing

isConstEdge (Const _ _) = True
isConstEdge _ = False

isRelElem (RelElem _ _ _) = True
isRelElem _ = False

isOp (Op _ _ _) = True
isOp _ = False

isRealOp (Op _ RealImag _) = False
isRealOp (Op _ (FT _) _) = False
isRealOp (Op _ _ _) = True
isRealOp _ = False

isReal (Op _ RealImag _) = False
isReal (Op _ (FT _) _) = False
isReal _ = True

{-


List of variables
-}
variables :: Internal -> Node -> [(ByteString, Node)]
variables edges top =
    map nameTuple $
    I.toList $ I.fromList $ filter (isVar . snd) $ depthFirst edges top
  where
    nameTuple (hash, Var _dims name) = (name, hash)
    nameTuple (hash, ee) = error $ "variables found " ++ show (hash, ee, edges)

{-

List of constants
-}
constants :: Internal -> Node -> [(Double, Node)]
constants edges top =
    map nameTuple $
    I.toList $ I.fromList $ filter (isConstEdge . snd) $ depthFirst edges top
  where
    nameTuple (hash, Const _dims d) = (d, hash)
    nameTuple (hash, ee) = error $ "variables found " ++ show (hash, ee, edges)

{-

List of |RelElem|s
-}
relElems :: Internal -> Node -> [(Int, Node)]
relElems edges top =
    map nameTuple $
    I.toList $ I.fromList $ filter (isRelElem . snd) $ depthFirst edges top
  where
    nameTuple (hash, RelElem inputIdx _ _) = (inputIdx, hash)
    nameTuple (hash, ee) = error $ "variables found " ++ show (hash, ee, edges)

{-

List of RelElems with all Zero offsets.
-}
relElemZeroOffsets :: Internal -> Node -> [(Int, Node)]
relElemZeroOffsets edges top =
    L.sort $
    map unique $
    map L.nub $
    filter (\grp -> null $ filter (== Nothing) $ map snd grp) $
    L.groupBy (\(i, _) (j, _) -> i == j) $
    L.sort $ map nameTuple $ filter (isRelElem . snd) $ depthFirst edges top
  where
    nameTuple (hash, RelElem inputIdx _ offsets) =
        if L.and (map (== 0) offsets)
            then (inputIdx, Just hash)
            else (inputIdx, Nothing)
    nameTuple (hash, ee) = error $ "variables found " ++ show (hash, ee, edges)
    unique [(i, Just hash)] = (i, hash)
    unique x =
        error $
        "relElemZeroOffsets found two kinds of non-zero offsets " ++
        pretty (edges, top) ++ (show (x, edges))

{-


-}
zeroOffsets dims =
    case dims of
        Dim0 -> []
        Dim1 _ -> [0]
        Dim2 _ -> [0, 0]
        Dim3 _ -> [0, 0, 0]
        Dim4 _ -> [0, 0, 0, 0]
        _ -> error $ "zeroOffsets " ++ show dims

{-


Generic function we don't plan to expose:
-}
varHidden :: Dims -> String -> Expression
varHidden dim name = Expression h (I.fromList [(h, Var dim packedName)])
  where
    packedName = C.pack name
    h = hash $ Var dim packedName

{-

Generic function we don't plan to expose:
-}
dvarHidden :: Dims -> String -> Expression
dvarHidden dim name = Expression h (I.fromList [(h, DVar dim packedName)])
  where
    packedName = C.pack name
    h = hash $ DVar dim packedName

{-

Create appropriately wrapped zero array objects.
-}
zero1d dim = OneD $ (zeroHidden (Dim1 dim))

zero2d dim = TwoD $ (zeroHidden (Dim2 dim))

zero3d dim = ThreeD $ (zeroHidden (Dim3 dim))

zeroHidden dim = Expression h (I.fromList [(h, Const dim 0)])
  where
    h = hash $ Const dim 0

{-

Create appropriately wrapped constant array objects.
-}
const1d dim k = OneD $ (constHidden (Dim1 dim) k)

const2d dim k = TwoD $ (constHidden (Dim2 dim) k)

const3d dim k = ThreeD $ (constHidden (Dim3 dim) k)

constHidden dim k = Expression h (I.fromList [(h, Const dim k)])
  where
    h = hash $ Const dim k

{-

Helper function to wrap node in |Expression| and |OneD|.
-}
mkOneD exprs n =
    case (getDimE exprs n, I.lookup n exprs >>= (Just . isReal)) of
        (Dim1 _d, Just True) -> (OneD $ Expression n exprs)
        (dims, ri) -> error $ "mkOneD dims " ++ show (dims, ri, n, exprs)

{-

Helper function to wrap node in |Expression| and |OneDC|.
-}
mkOneDC exprs n =
    case (getDimE exprs n, I.lookup n exprs >>= (Just . isReal)) of
        (Dim1 _d, Just False) -> (OneDC $ Expression n exprs)
        (dims, ri) -> error $ "mkOneDC dims " ++ show (dims, ri, n, exprs)

{-

Helper function to wrap node in |Expression| and |TwoD|.
-}
mkTwoD exprs n =
    case (getDimE exprs n, I.lookup n exprs >>= (Just . isReal)) of
        (Dim2 _, Just True) -> TwoD $ Expression n exprs
        (dims, ri) -> error $ "mkTwoD dims " ++ show (dims, ri, n, exprs)

{-

Helper function to wrap node in |Expression| and |TwoDSparse|.
-}
mkTwoDSparse exprs n =
    case (getDimE exprs n, I.lookup n exprs >>= (Just . isReal)) of
        (Dim2SL1 _ _, Just True) ->
            let (sL, _dims) = sL2Dims exprs n
             in TwoDSparse sL $ Expression n exprs
        (dims, ri) -> error $ "mkTwoDSparse dims " ++ show (dims, ri, n, exprs)

{-
Helper function to wrap node in |Expression| and |TwoDCSparse|.
-}
mkTwoDCSparse exprs n =
    case (getDimE exprs n, I.lookup n exprs >>= (Just . isReal)) of
        (Dim2SL1 _ _, Just True) ->
            let (sL, _dims) = sL2Dims exprs n
             in TwoDCSparse sL $ Expression n exprs
        (dims, ri) -> error $ "mkTwoDCSparse dims " ++ show (dims, ri, n, exprs)

{-



Helper function to wrap node in |Expression| and |TwoDC|.
-}
mkTwoDC exprs n =
    case (getDimE exprs n, I.lookup n exprs >>= (Just . isReal)) of
        (Dim2 _, Just False) -> TwoDC $ Expression n exprs
        (dims, ri) -> error $ "mkTwoDC dims " ++ show (dims, ri, n, exprs)

{-

Helper function to wrap node in |Expression| and |ThreeD|.
-}
mkThreeD exprs n =
    case (getDimE exprs n, I.lookup n exprs >>= (Just . isReal)) of
        (Dim3 _, Just True) -> ThreeD $ Expression n exprs
        (dims, ri) -> error $ "mkThreeD dims " ++ show (dims, ri, n, exprs)

{-

Helper function to wrap node in |Expression| and |ThreeDSparse|.
-}
mkThreeDSparse exprs n =
    case (getDimE exprs n, I.lookup n exprs >>= (Just . isReal)) of
        (Dim3SL1 _ _, Just True) ->
            let (sL, _dims) = sL3Dims exprs n
             in ThreeDSparse sL $ Expression n exprs
        (dims, ri) ->
            error $ "mkThreeDSparse dims " ++ show (dims, ri, n, exprs)

{-
Helper function to wrap node in |Expression| and |ThreeDCSparse|.
-}
mkThreeDCSparse exprs n =
    case (getDimE exprs n, I.lookup n exprs >>= (Just . isReal)) of
        (Dim3SL1 _ _, Just True) ->
            let (sL, _dims) = sL3Dims exprs n
             in ThreeDCSparse sL $ Expression n exprs
        (dims, ri) ->
            error $ "mkThreeDCSparse dims " ++ show (dims, ri, n, exprs)

{-

Helper function to wrap node in |Expression| and |ThreeDC|.
-}
mkThreeDC exprs n =
    case (getDimE exprs n, I.lookup n exprs >>= (Just . isReal)) of
        (Dim3 _, Just False) -> (ThreeDC $ Expression n exprs)
        (dims, ri) -> error $ "mkThreeDC dims " ++ show (dims, ri, n, exprs)

{-

Go back in DAG looking for projection to find out what the current sparse list lives in.
-}
sL2Dims exprs n =
    case I.lookup n exprs of
        Just (Op _ (Project (SSList2D sL)) [n1]) ->
            case getDimE exprs n1 of
                Dim2 d -> (sL, d)
                d -> error $ "sL2Dims expected 2d " ++ show d
        Just (Op _ _ (n1:_)) -> sL2Dims exprs n1
        x -> error $ "sl2Dims " ++ show (n, x, exprs)

sL3Dims exprs n =
    case I.lookup n exprs of
        Just (Op _ (Project (SSList3D sL)) [n1]) ->
            case getDimE exprs n1 of
                Dim3 d -> (sL, d)
                d -> error $ "sL3Dims expected 3d " ++ show d
        Just (Op _ _ (n1:_)) -> sL3Dims exprs n1
        x -> error $ "sl3Dims " ++ show (n, x, exprs)

sL4Dims exprs n =
    case I.lookup n exprs of
        Just (Op _ (Project (SSList4D sL)) [n1]) ->
            case getDimE exprs n1 of
                Dim4 d -> (sL, d)
                d -> error $ "sL4Dims expected 4d " ++ show d
        Just (Op _ _ (n1:_)) -> sL4Dims exprs n1
        x -> error $ "sl4Dims " ++ show (n, x, exprs)

{-



Display dimensions for pretty-printing.
-}
dispDims (Dim0) = ""
dispDims (Dim1 d) = "(" ++ show d ++ ")"
dispDims (Dim1p size polyOrd) = "(" ++ show size ++ "^{" ++ show polyOrd ++ "})"
dispDims (Dim2 d) = show d
dispDims (Dim2p size polyOrd) = "(" ++ show size ++ "^{" ++ show polyOrd ++ "})"
dispDims (Dim2SL1 d l) = "(" ++ show l ++ "of" ++ show d ++ ")"
dispDims (Dim2SL2 d l) = "(" ++ show l ++ "of" ++ show d ++ ")"
dispDims (Dim3 d) = show d
dispDims (Dim3p size polyOrd) = "(" ++ show size ++ "^{" ++ show polyOrd ++ "})"
dispDims (Dim3SL1 d l) = "(" ++ show l ++ "of" ++ show d ++ ")"
dispDims (Dim3SL2 d l) = "(" ++ show l ++ "of" ++ show d ++ ")"
dispDims (Dim3SL3 d l) = "(" ++ show l ++ "of" ++ show d ++ ")"
dispDims (Dim4 d) = show d
dispDims (Dim4SL1 d l) = "(" ++ show l ++ "of" ++ show d ++ ")"
dispDims (Dim4SL2 d l) = "(" ++ show l ++ "of" ++ show d ++ ")"
dispDims (Dim4SL3 d l) = "(" ++ show l ++ "of" ++ show d ++ ")"
dispDims (Dim4SL4 d l) = "(" ++ show l ++ "of" ++ show d ++ ")"
dispDims DimUnknown =
    if skipDebug
        then ""
        else "DimUnknown"
dispDims (CDims dims) = concat ["(", L.intercalate "," $ map dispDims dims, ")"]

{-

Extract dimensions from |ExpressionEdges| and a |node| in an expression.
-}
getDim (Var dims _) = dims
getDim (DVar dims _) = dims
getDim (Op dims _ _) = dims
getDim (Const dims _) = dims
getDim (RelElem _ _ _) = Dim0

getDimE' (Expression n e) = getDimE e n

getDimE :: Internal -> Node -> Dims
getDimE exprs node =
    case I.lookup node exprs of
        Just x -> getDim x
        Nothing -> error $ "getDimE missing Edge " ++ show (node, exprs)

cDims :: Dims -> [Dims]
cDims (CDims dims) = dims
cDims dim = error ("cDims applied to " ++ show dim)

trDims SCR (Dim2 (d1, d2)) = Dim2 (d2, d1) -- FIXME possible duplicate of transposeDims 626; this function is called in HashedInstances 1218;1248
trDims swap dims = error $ "trDims not implemented for " ++ show (swap, dims)

{-

Make scalar constants.  Usually we don't need this because of the
|Num| class for |Scalar|, which makes |3| an expression in |x + 3|
if |x| is a |Scalar| expression.
-}
fromDbl :: Double -> Scalar
fromDbl d = Scalar $ Expression h (I.fromList [(h, Const Dim0 d)])
  where
    h = hash $ Const Dim0 d

{-

For |case| matching expressions to constants.
-}
maybeConst :: Expression -> Maybe Double
maybeConst (Expression n exprs) =
    case I.lookup n exprs of
        Just (Const Dim0 x) -> Just x
        _ -> Nothing

maybeConstS (Scalar e) = maybeConst e

{-

-}
nodeSort :: (Internal, [Node]) -> (Internal, [Node])
nodeSort (exprs, nodes) = (exprs, nodeSort' exprs nodes)

nodeSort' exprs nodes = L.sortBy (compareOp exprs) nodes

nodeSortE = flip nodeSort'

{-

Check for cycles in DAG (which should not have cycles).
Do a depth-first traversal, checking for re-occurence of an ancestor as a descendant.
-}
hasCycles (e, n) = hC IntSet.empty n
  where
    hC seen n =
        case I.lookup n e of
            Just (Op _ _ inputs) ->
                if n `IntSet.member` seen
                    then [n]
                    else concatMap (hC $ IntSet.insert n seen) inputs
            _ -> filter (flip IntSet.member seen) [n]

hasCyclesTest =
    [0] ==
    hasCycles (I.fromList [(0, Op Dim0 Neg [1]), (1, Op Dim0 Neg [0])], 0)

{-

Need to put Pretty instance here if we want to debug with it.
-}
instance Pretty (Internal, Node) where
    pretty (e, n) = pretty $ Expression n e

instance Pretty Expression where
    pretty (Expression node exprs) = hidden (I.empty) node exprs

prettySubsRE e subsRE n = hidden subsRE n e

hidden subsLst node exprs =
    let showIn n = hidden subsLst n exprs
        prettySep _ [] = []
        prettySep _ [a] = showIn a
        prettySep sep (b:bs) = concat [showIn b, sep, prettySep sep bs]
     in case I.lookup node exprs of
            Nothing ->
                "pretty Expression found no node " ++
                (take 1000 $ show (node, exprs))
            Just (Var dims name) -> C.unpack name ++ (dispDims dims)
            Just (DVar dims name) ->
                "d(" ++ C.unpack name ++ (dispDims dims) ++ ")"
            Just (RelElem num bndry idx) ->
                (case I.lookup num subsLst of
                     Just called -> called
                     Nothing -> rectNum num bndry) ++
                "[" ++ (showSep "," idx) ++ "]"
            Just (Const dims d) ->
                if d < 0
                    then "(" ++ show d ++ ")" ++ dispDims dims
                    else show d ++ dispDims dims
            Just (Op _dims (FT True) [n]) -> "(FT(" ++ showIn n ++ "))"
            Just (Op _dims (FT False) [n]) -> "(InvFT(" ++ showIn n ++ "))"
            Just (Op _dims (PFT True dir) [n]) ->
                "(" ++ show dir ++ "PFT(" ++ showIn n ++ "))"
            Just (Op _dims (PFT False dir) [n]) ->
                "(" ++ show dir ++ "PInvFT(" ++ showIn n ++ "))"
            Just (Op _dims (Transpose swp) [n]) ->
                "(Transpose" ++ show swp ++ "(" ++ showIn n ++ "))"
            Just (Op _dims Div [a, b]) ->
                "(" ++ showIn a ++ "/" ++ showIn b ++ ")"
            Just (Op _dims Dot [a, b]) ->
                "(" ++ showIn a ++ "<.>" ++ showIn b ++ ")"
            Just (Op _dims ScaleV [a, b]) ->
                "(" ++ showIn a ++ "*." ++ showIn b ++ ")"
            Just (Op _dims Neg [a]) -> "(-" ++ showIn a ++ ")"
            Just (Op _dims RealPart [a]) -> "(Re" ++ showIn a ++ ")"
            Just (Op _dims ImagPart [a]) -> "(Im" ++ showIn a ++ ")"
            Just (Op _dims RealImag [a, b]) ->
                "(" ++ showIn a ++ "+:" ++ showIn b ++ ")"
            Just (Op dims (Project ss@(SSCrop list dims')) [a]) ->
                "(Proj_{" ++
                (showSep "x" list) ++
                flagProj dims ss (dims, dims') ++ "}(" ++ showIn a ++ "))"
            Just (Op dims (Inject ss@(SSCrop list dims')) [a]) ->
                "(Inj_{" ++
                (showSep "x" list) ++
                flagInj dims ss (dims, dims') ++ "}(" ++ showIn a ++ "))"
            Just (Op _dims (Project (SSNyquist list)) [a]) ->
                "(Proj_{" ++ (show list) ++ "}(" ++ showIn a ++ "))"
            Just (Op _dims (Inject (SSNyquist list)) [a]) ->
                "(Inj_{" ++ (show list) ++ "}(" ++ showIn a ++ "))"
            Just (Op _dims Prod inputs) -> "(" ++ prettySep "*" inputs ++ ")"
            Just (Op _dims Sum inputs) -> "(" ++ prettySep "+" inputs ++ ")"
            Just (Op _dims (SCZ e) inputs) ->
                "ConvZip[" ++ pretty e ++ "](" ++ prettySep "," inputs ++ ")"
            Just (Op _dims (Compound _) inputs) ->
                "{Compound(" ++
                unwords (map (('(' :) . (++ ")")) $ map showIn inputs) ++ ")}"
            Just (Op _dims op inputs) ->
                "(" ++ nice op ++ "(" ++ unwords (map showIn inputs) ++ "))"

-- check dimensions
flagInj (Dim1 d1) ss@(SSCrop _ [dim]) x =
    let err = "~Crop~" ++ show (ss, x)
     in if dim == d1
            then fc err ss
            else err
flagInj (Dim2 (d1, d2)) ss@(SSCrop _ dims) x =
    let err = "~Crop~" ++ show (ss, x)
     in if [d1, d2] == dims
            then fc err ss
            else err
flagInj (Dim3 (d1, d2, d3)) ss@(SSCrop _ dims) x =
    let err = "~Crop~" ++ show (ss, x)
     in if [d1, d2, d3] == dims
            then fc err ss
            else err
flagInj (Dim4 (d1, d2, d3, d4)) ss@(SSCrop _ dims) x =
    let err = "~Crop~" ++ show (ss, x)
     in if [d1, d2, d3, d4] == dims
            then fc err ss
            else err
flagInj _dims ss x = "~Crop~??" ++ show (ss, x)

flagProj (Dim1 d1) ss@(SSCrop crop _dims) x =
    let err = "~Crop~Proj~" ++ show (ss, x)
     in if widths crop == [d1]
            then fc err ss
            else err
flagProj (Dim2 (d1, d2)) ss@(SSCrop crop _dims) x =
    let err = "~Crop~Proj~" ++ show (ss, x)
     in if [d1, d2] == widths crop
            then fc err ss
            else err
flagProj (Dim3 (d1, d2, d3)) ss@(SSCrop crop _dims) x =
    let err = "~Crop~Proj~" ++ show (ss, x)
     in if [d1, d2, d3] == widths crop
            then fc err ss
            else err
flagProj (Dim4 (d1, d2, d3, d4)) ss@(SSCrop crop _dims) x =
    let err = "~Crop~Proj~" ++ show (ss, x)
     in if [d1, d2, d3, d4] == widths crop
            then fc err ss
            else err
flagProj dims ss x = "~Crop~Inj??" ++ show (dims, ss, x)

widths ::
       forall a. Num a
    => [(a, a)]
    -> [a]
widths ((low, high):rest) = (high - low + 1) : (widths rest)
widths [] = []

fc :: [Char] -> Subspace -> [Char]
fc err (SSCrop ((low, high):list) (dim:dims)) =
    if low < 0 || low > high || high >= dim
        then err
        else fc err (SSCrop list dims)
fc _ _ = ""

nice :: OpId -> [Char]
nice Sqrt = "sqrt"
nice Exp = "exp"
nice Cos = "cos"
nice Sin = "sin"
nice Tan = "tan"
nice Log = "log"
nice Asin = "asin"
nice Acos = "acos"
nice Atan = "atan"
nice Sinh = "sinh"
nice Cosh = "cosh"
nice Tanh = "tanh"
nice Atanh = "atanh"
nice Acosh = "acosh"
nice Asinh = "asinh"
nice op = show op

printComp0 :: (IntMap ExpressionEdge, Node) -> IO ()
printComp0 = printComp (\_ -> [])

printComp :: (Node -> [Char]) -> (IntMap ExpressionEdge, Node) -> IO ()
printComp xtra (e, n) = printCompInt xtra (e, n) [nodes]
  where
    nodes = topSort e n

printCompInt ::
       (Node -> [Char])
    -> (IntMap ExpressionEdge, Node)
    -> [[IntSet.Key]]
    -> IO ()
printCompInt xtra (e, n) nodeLists =
    putStrLn $
    unlines $
    concatMap
        ("===========================" :)
        [concatMap printOne $ noVars $ ns | ns <- nodeLists]
  where
    noVars =
        filter
            (\x ->
                 case I.lookup x e of
                     Just (Var _ _) -> False
                     _ -> True)
    nodes = noVars $ topSort e n
    shortNumMap = I.fromList $ zip nodes [(0 :: Int) ..]
    shortNum m =
        case I.lookup m e of
            Just (Var _ name) -> show name
            _ ->
                case I.lookup m shortNumMap of
                    Just idx -> show idx
                    _ -> "printComp couldn't find node " ++ show (m, nodes)
    pArgs args =
        concat
            ("(" : (L.intersperse "," $ map (('t' :) . shortNum) args) ++ [")"])
    printOne node =
        let idx = shortNum node :: String
         in (("----- " ++
              idx ++
              " (" ++
              show node ++
              ")   sized " ++ show (getDimE e node) ++ "  " ++ xtra node) :
             (case I.lookup node e of
                  Just (Op _ (SCZ sczE) args) ->
                      "  t" ++ idx ++ " := " ++ pretty sczE ++ " " ++ pArgs args
                  Just (Op _ op args) ->
                      "  t" ++ idx ++ " := " ++ nice op ++ " " ++ pArgs args
                  Just x -> "  t" ++ idx ++ " := " ++ show x
                  Nothing ->
                      error $ "printComp couldn't find node " ++ show (n, e)) :
             [])

class PrettyHex a where
    prettyHex :: a -> String

instance PrettyHex Expression where
    prettyHex (Expression node exprs) =
        let showIn n = prettyHex (Expression n exprs)
         in case I.lookup node exprs of
                Nothing ->
                    "prettyHex Expression found no node " ++
                    (take 1000 $ show (node, exprs))
                Just (Var dims name) -> C.unpack name ++ (dispDims dims)
                Just (DVar dims name) ->
                    "d(" ++ C.unpack name ++ (dispDims dims) ++ ")"
                Just (RelElem num bndry idx) ->
                    rectNum num bndry ++ "[" ++ (showSep "," idx) ++ "]"
                Just (Const dims d) ->
                    Numeric.showHex (round d :: Integer) $ dispDims dims
                Just (Op _dims (FT True) [n]) -> "(FT(" ++ showIn n ++ "))"
                Just (Op _dims (FT False) [n]) -> "(InvFT(" ++ showIn n ++ "))"
                Just (Op _dims (PFT True dir) [n]) ->
                    "(" ++ show dir ++ "PFT(" ++ showIn n ++ "))"
                Just (Op _dims (PFT False dir) [n]) ->
                    "(" ++ show dir ++ "PInvFT(" ++ showIn n ++ "))"
                Just (Op _dims (Transpose swp) [n]) ->
                    "(Transpose" ++ show swp ++ "(" ++ showIn n ++ "))"
                Just (Op _dims Div [a, b]) ->
                    "(" ++ showIn a ++ "/" ++ showIn b ++ ")"
                Just (Op _dims Dot [a, b]) ->
                    "(" ++ showIn a ++ "." ++ showIn b ++ ")"
                Just (Op _dims ScaleV [a, b]) ->
                    "(" ++ showIn a ++ ":*:" ++ showIn b ++ ")"
                Just (Op _dims Neg [a]) -> "(-" ++ showIn a ++ ")"
                Just (Op _dims RealPart [a]) -> "(Re" ++ showIn a ++ ")"
                Just (Op _dims ImagPart [a]) -> "(Im" ++ showIn a ++ ")"
                Just (Op _dims RealImag [a, b]) ->
                    "(" ++ showIn a ++ "+:" ++ showIn b ++ ")"
                Just (Op _dims (Project (SSCrop list _)) [a]) ->
                    "(Proj_{" ++ (showSep "x" list) ++ "}(" ++ showIn a ++ "))"
                Just (Op _dims (Inject (SSCrop list _)) [a]) ->
                    "(Inj_{" ++ (showSep "x" list) ++ "}(" ++ showIn a ++ "))"
                Just (Op _dims (Project (SSNyquist list)) [a]) ->
                    "(Proj_{" ++ (show list) ++ "}(" ++ showIn a ++ "))"
                Just (Op _dims (Inject (SSNyquist list)) [a]) ->
                    "(Inj_{" ++ (show list) ++ "}(" ++ showIn a ++ "))"
                Just (Op _dims Prod inputs) ->
                    "(" ++
                    prettySepHex "*" [Expression n exprs | n <- inputs] ++ ")"
                Just (Op _dims Sum inputs) ->
                    "(" ++
                    prettySepHex "+" [Expression n exprs | n <- inputs] ++ ")"
                Just (Op _dims (SCZ e) inputs) ->
                    "ConvZip[" ++
                    prettyHex e ++
                    "](" ++
                    prettySepHex "," [Expression n exprs | n <- inputs] ++ ")"
                Just (Op _dims (Compound _) inputs) ->
                    "{Compound(" ++
                    unwords (map (('(' :) . (++ ")")) $ map showIn inputs) ++
                    ")}"
                Just (Op _dims op inputs) ->
                    "{" ++ nice op ++ "(" ++ unwords (map showIn inputs) ++ "))"

rectNum ::
       forall a. Show a
    => a
    -> Boundary
    -> [Char]
rectNum num ZeroMargin = "z" ++ show num
rectNum num Reflective = "r" ++ show num
rectNum num Torus = "t" ++ show num
rectNum num (ConstMargin c) = "c_" ++ show c ++ "?" ++ show num

showSep ::
       forall a. Show a
    => [Char]
    -> [a]
    -> [Char]
showSep _ [] = []
showSep _ [a] = show a
showSep sep (b:bs) = concat [show b, sep, showSep sep bs]

prettySepHex ::
       forall a. PrettyHex a
    => [Char]
    -> [a]
    -> [Char]
prettySepHex _ [] = []
prettySepHex _ [a] = prettyHex a
prettySepHex sep (b:bs) = concat [prettyHex b, sep, prettySepHex sep bs]

{-

Returns the list of the dimensions for non-subspace expressions
-}
dimList :: Dims -> [Int]
dimList Dim0 = []
dimList (Dim1 i) = [i]
dimList (Dim2 (i1, i2)) = [i1, i2]
dimList (Dim3 (i1, i2, i3)) = [i1, i2, i3]
dimList (Dim4 (i1, i2, i3, i4)) = [i1, i2, i3, i4]
dimList DimUnknown = []
dimList _ = error "HE.dimList not defined for subspace lists"

{-

Returns the product of the dimensions for non-subspace expressions
-}
dimProd :: Dims -> Int
dimProd Dim0 = 1
dimProd (Dim1 i) = i
dimProd (Dim2 (i1, i2)) = i1 * i2
dimProd (Dim3 (i1, i2, i3)) = i1 * i2 * i3
dimProd (Dim4 (i1, i2, i3, i4)) = i1 * i2 * i3 * i4
dimProd (Dim3SL1 (_i1, i2, i3) keep) = keep * i2 * i3
dimProd (CDims subdims) = sum $ map dimProd subdims
dimProd x =
    error ("HE.dimProd not defined for subspace lists with dims: " ++ show x)

dimProdE :: (Internal, Node) -> Int
dimProdE (exprs, n) = dimProd (getDimE exprs n)

{-
Returns the appropriate zero element for 2D,3D or 4D
-}
relZero :: Dims -> [Int]
relZero (Dim1 _) = [0]
relZero (Dim2 _) = [0, 0]
relZero (Dim3 _) = [0, 0, 0]
relZero (Dim4 _) = [0, 0, 0, 0]
relZero _ =
    error
        "HE.relZero only defined for real and complex OneD, TwoD, ThreeD and FourD"

{-

-}
class ShowHex a where
    showHex :: a -> String

{-

-}
dimLength :: Dims -> Int
dimLength (Dim1 _) = 1
dimLength (Dim2 _) = 2
dimLength (Dim3 _) = 3
dimLength (Dim4 _) = 4
dimLength _ = error "HE.dimLength not defined for subspace lists"

{-

Instances to enable DeepSeq full evaluation.
-}
instance NFData ExpressionEdge where
    rnf (Var dim name) = C.length name `seq` rnf dim
    rnf (DVar dim name) = C.length name `seq` rnf dim
    rnf (RelElem a bnd idxs) = rnf a `seq` rnf bnd `seq` rnf idxs
    rnf (Const dim d) = rnf dim `seq` rnf d
    rnf (Op dims op args) = rnf dims `seq` rnf op `seq` rnf args

instance NFData Boundary where
    rnf Reflective = ()
    rnf Torus = ()
    rnf ZeroMargin = ()
    rnf (ConstMargin d) = rnf d

instance NFData Dims where
    rnf Dim0 = ()
    rnf (Dim1 d) = rnf d
    rnf (Dim2 d) = rnf d
    rnf (Dim3 d) = rnf d
    rnf (Dim4 d) = rnf d
    rnf DimUnknown = ()
    rnf (CDims dims) = rnf dims
    rnf x = error $ "HE.NFData not implemented: " ++ show x

instance NFData OpId where
    rnf Sum = ()
    rnf Neg = ()
    rnf SubMask = ()
    rnf Prod = ()
    rnf Div = ()
    rnf NegMask = ()
    rnf Sqrt = ()
    rnf Sin = ()
    rnf Cos = ()
    rnf Tan = ()
    rnf Exp = ()
    rnf Log = ()
    rnf Sinh = ()
    rnf Cosh = ()
    rnf Tanh = ()
    rnf Asin = ()
    rnf Acos = ()
    rnf Atan = ()
    rnf Asinh = ()
    rnf Acosh = ()
    rnf Atanh = ()
    rnf Dot = ()
    rnf (MapND expr bytestring) = C.length bytestring `seq` rnf expr -- hack found on mailing list
    rnf ScaleV = ()
    rnf RealPart = ()
    rnf ImagPart = ()
    rnf RealImag = ()
    rnf (FT True) = ()
    rnf (FT False) = ()
    rnf (PFT True Row) = ()
    rnf (PFT True Column) = ()
    rnf (PFT True Slice) = ()
    rnf (PFT False Row) = ()
    rnf (PFT False Column) = ()
    rnf (PFT False Slice) = ()
    rnf (Transpose SCR) = ()
    rnf (Transpose SCSR) = ()
    rnf (Transpose SCRS) = ()
    rnf (Transpose SSRC) = ()
    rnf (Transpose SSCR) = ()
    rnf (Transpose SRSC) = ()
    rnf (Compound _) = ()
    rnf (Extract _) = ()
    rnf (Project _) = ()
    rnf (Inject _) = ()
    rnf (SCZ e) = rnf e
    rnf x = error $ "HE.NFData not implemented: " ++ show x

instance NFData Expression where
    rnf (Expression n exprs) = rnf exprs `seq` rnf n

{-

-}
class SimilarSum m where
    decompose :: m -> (Internal, [Node])

class All1D m

class All2D m

class All3D m

class All4D m

class All5D m

class All6D m

{-

Other issues:
  in-place operations
  units

Returns the third element of a triple
-}
fst3 :: forall t t1 t2. (t, t1, t2) -> t
fst3 (x, _y, _z) = x

snd3 :: forall t t1 t2. (t, t1, t2) -> t1
snd3 (_x, y, _z) = y

thrd3 :: forall t t1 t2. (t, t1, t2) -> t2
thrd3 (_x, _y, z) = z

{-

A sum function which returns an error for an empty list.
-}
sum1 ::
       forall a. Num a
    => [a]
    -> a
sum1 [] = error "sum1"
sum1 (a:b:rest) = (a + (sum1 (b : rest)))
sum1 [a] = a
{-


-}
