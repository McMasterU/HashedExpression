{-
(c) 2010 Christopher Kumar Anand

Experiment in common subexpressions without monads and better expression simplification.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module HashedInstances where

import Data.ByteString ()
import qualified Data.ByteString.Char8 as C
import Data.IntMap ()
import qualified Data.IntMap as I
import qualified Data.List as L
import qualified Data.Map as Map
import HashedExpression
import LinearCombination
import qualified Numeric

--import Debug.Trace
{-

Instances which build scalar expressions.
-}
instance Num Scalar where
    negate (Scalar (Expression n exprs)) =
        Scalar $ addEdge' exprs' (Op Dim0 Prod [neg, n])
      where
        (exprs', neg) = addEdge exprs (Const Dim0 (-1))
    fromInteger i = Scalar $ (Expression h (I.fromList [(h, i'')]))
      where
        h = hash i''
        i' = fromInteger i
        i'' = Const Dim0 i'
    (Scalar expr1) + (Scalar expr2) =
        let (Expression _ es, (n1, n2)) = merge expr1 expr2
         in case (maybeConst expr1, maybeConst expr2) of
                (_, _) -> Scalar $ addEdge' es (Op Dim0 Sum [n1, n2])
    (Scalar expr1) * (Scalar expr2) =
        let (Expression _ es, (n1, n2)) = merge expr1 expr2
         in case (maybeConst expr1, maybeConst expr2) of
                (_, _) -> Scalar $ addEdge' es (Op Dim0 Prod [n1, n2])
    abs (Scalar (Expression n exprs)) =
        Scalar $ addEdge' exprs (Op Dim0 Abs [n])
    signum (Scalar (Expression n exprs)) =
        Scalar $ addEdge' exprs (Op Dim0 Signum [n])

{-           (Just x, Just y) -> Scalar $ addEdge' es (Const Dim0 $ x + y)
           (Just 0, _) -> Scalar $ Expression n2 es
           (_, Just 0) -> Scalar $ Expression n1 es
-}
{-           (Just 0, _) -> Scalar expr1  -- is 0
           (_, Just 0) -> Scalar expr2  -- is 0
           (Just 1, _) -> Scalar expr2
           (_, Just 1) -> Scalar expr1
           (Just x, Just y) -> Scalar $ addEdge' es (Const Dim0 $ x * y)
-}
{-

Instances which build complex scalar expressions.
-}
instance Num ScalarC where
    negate (ScalarC (Expression n exprs)) =
        ScalarC $ addEdge' exprs' (Op Dim0 Prod [neg, n])
      where
        (exprs', neg) = addEdge exprs (Const Dim0 (-1))
    fromInteger i = iRe $ Scalar $ (Expression h (I.fromList [(h, i'')]))
      where
        h = hash i''
        i' = fromInteger i
        i'' = Const Dim0 i'
    (ScalarC expr1) + (ScalarC expr2) =
        let (Expression _ es, (n1, n2)) = merge expr1 expr2
         in case (maybeConst expr1, maybeConst expr2) of
                (Just x, Just y) -> ScalarC $ addEdge' es (Const Dim0 $ x + y)
                (Just 0, _) -> ScalarC $ Expression n2 es
                (_, Just 0) -> ScalarC $ Expression n1 es
                (_, _) -> ScalarC $ addEdge' es (Op Dim0 Sum [n1, n2])
    (ScalarC expr1) * (ScalarC expr2) =
        let (Expression _ es, (n1, n2)) = merge expr1 expr2
         in case (maybeConst expr1, maybeConst expr2) of
                (Just 0, _) -> ScalarC $ Expression n1 es -- is 0
                (_, Just 0) -> ScalarC $ Expression n2 es -- is 0
                (Just 1, _) -> ScalarC $ Expression n2 es
                (_, Just 1) -> ScalarC $ Expression n1 es
                (Just x, Just y) -> ScalarC $ addEdge' es (Const Dim0 $ x * y)
                (_, _) -> ScalarC $ addEdge' es (Op Dim0 Prod [n1, n2])
    abs (ScalarC (Expression n exprs)) =
        ScalarC $ addEdge' exprs (Op Dim0 Abs [n])
    signum (ScalarC (Expression n exprs)) =
        ScalarC $ addEdge' exprs (Op Dim0 Signum [n])

{-


Instance to convert back and forth between real and complex nodes.
-}
instance Complex ScalarC Scalar where
    (Scalar reExpr) +: (Scalar imExpr) =
        let (Expression _ es, (reN, imN)) = merge reExpr imExpr
         in ScalarC $ addEdge' es (Op Dim0 RealImag [reN, imN])
    iRe (Scalar (Expression n exprs)) =
        let Expression n0 exprs1 = addEdge' exprs (Const Dim0 0)
         in ScalarC $ addEdge' exprs1 (Op Dim0 RealImag [n, n0])
    iIm (Scalar (Expression n exprs)) =
        let Expression n0 exprs1 = addEdge' exprs (Const Dim0 0)
         in ScalarC $ addEdge' exprs1 (Op Dim0 RealImag [n0, n])
    xRe (ScalarC (Expression n exprs)) =
        Scalar $ addEdge' exprs (Op Dim0 RealPart [n])
    xIm (ScalarC (Expression n exprs)) =
        Scalar $ addEdge' exprs (Op Dim0 ImagPart [n])

{-

FIXME:  should we be simplifying as expressions are being created?
Float-valued expressions for now:
-}
instance Fractional Scalar where
    (Scalar expr1) / (Scalar expr2) =
        let (Expression _ es, (n1, n2)) = merge expr1 expr2
         in case (maybeConst expr1, maybeConst expr2) of
                (_, Just 0) ->
                    error $
                    "divide by zero constant in " ++ show (expr1, expr2, es)
                (Just 0, Just _y) -> Scalar $ Expression n1 es -- is 0 / nonZero
                (Just x, Just y) -> Scalar $ addEdge' es (Const Dim0 $ x / y)
                (_, Just 1) -> Scalar $ Expression n1 es
                (_, _) -> Scalar (addEdge' es (Op Dim0 Div [n1, n2]))
    recip x = 1 / x
    fromRational i = fromDbl $ fromRational i

{-

Float-valued expressions for now:
-}
instance Fractional ScalarC where
    (ScalarC expr1) / (ScalarC expr2) =
        let (Expression _ es, (n1, n2)) = merge expr1 expr2
         in case (maybeConst expr1, maybeConst expr2) of
                (_, Just 0) ->
                    error $
                    "divide by zero constant in " ++ show (expr1, expr2, es)
                (Just 0, _) -> ScalarC $ Expression n1 es -- is 0
                (Just x, Just y) -> ScalarC $ addEdge' es (Const Dim0 $ x / y)
                (_, Just 1) -> ScalarC $ Expression n1 es
                (_, _) -> ScalarC (addEdge' es (Op Dim0 Div [n1, n2]))
    recip x = 1 / x
    fromRational i = (fromDbl $ fromRational i) +: (fromDbl 0)

{-


-}
instance Floating Scalar where
    sqrt (Scalar expr1@(Expression n es)) =
        Scalar $
        case maybeConst expr1 of
            Just x -> addEdge' es (Const Dim0 $ sqrt x)
            _ -> addEdge' es (Op Dim0 Sqrt [n])
    pi = Scalar $ Expression h (I.fromList [(h, i'')])
      where
        h = hash i''
        i'' = Const Dim0 pi
    exp (Scalar expr1@(Expression n es)) =
        Scalar $
        case maybeConst expr1 of
            Just x -> addEdge' es (Const Dim0 $ exp x)
            _ -> addEdge' es (Op Dim0 Exp [n])
    cos (Scalar expr1@(Expression n es)) =
        Scalar $
        case maybeConst expr1 of
            Just x -> addEdge' es (Const Dim0 $ cos x)
            _ -> addEdge' es (Op Dim0 Cos [n])
    sin (Scalar expr1@(Expression n es)) =
        Scalar $
        case maybeConst expr1 of
            Just x -> addEdge' es (Const Dim0 $ sin x)
            _ -> addEdge' es (Op Dim0 Sin [n])
    tan (Scalar expr1@(Expression n es)) =
        Scalar $
        case maybeConst expr1 of
            Just x -> addEdge' es (Const Dim0 $ tan x)
            _ -> addEdge' es (Op Dim0 Tan [n])
    log (Scalar expr1@(Expression n es)) =
        Scalar $
        case maybeConst expr1 of
            Just x -> addEdge' es (Const Dim0 $ log x)
            _ -> addEdge' es (Op Dim0 Log [n])
    asin (Scalar expr1@(Expression n es)) =
        Scalar $
        case maybeConst expr1 of
            Just x -> addEdge' es (Const Dim0 $ asin x)
            _ -> addEdge' es (Op Dim0 Asin [n])
    acos (Scalar expr1@(Expression n es)) =
        Scalar $
        case maybeConst expr1 of
            Just x -> addEdge' es (Const Dim0 $ acos x)
            _ -> addEdge' es (Op Dim0 Acos [n])
    atan (Scalar expr1@(Expression n es)) =
        Scalar $
        case maybeConst expr1 of
            Just x -> addEdge' es (Const Dim0 $ atan x)
            _ -> addEdge' es (Op Dim0 Atan [n])
    sinh (Scalar expr1@(Expression n es)) =
        Scalar $
        case maybeConst expr1 of
            Just x -> addEdge' es (Const Dim0 $ sinh x)
            _ -> addEdge' es (Op Dim0 Sinh [n])
    cosh (Scalar expr1@(Expression n es)) =
        Scalar $
        case maybeConst expr1 of
            Just x -> addEdge' es (Const Dim0 $ cosh x)
            _ -> addEdge' es (Op Dim0 Cosh [n])
    tanh (Scalar expr1@(Expression n es)) =
        Scalar $
        case maybeConst expr1 of
            Just x -> addEdge' es (Const Dim0 $ atanh x)
            _ -> addEdge' es (Op Dim0 Tanh [n])
    atanh (Scalar expr1@(Expression n es)) =
        Scalar $
        case maybeConst expr1 of
            Just x -> addEdge' es (Const Dim0 $ atanh x)
            _ -> addEdge' es (Op Dim0 Atanh [n])
    acosh (Scalar expr1@(Expression n es)) =
        Scalar $
        case maybeConst expr1 of
            Just x -> addEdge' es (Const Dim0 $ acosh x)
            _ -> addEdge' es (Op Dim0 Acosh [n])
    asinh (Scalar expr1@(Expression n es)) =
        Scalar $
        case maybeConst expr1 of
            Just x -> addEdge' es (Const Dim0 $ asinh x)
            _ -> addEdge' es (Op Dim0 Asinh [n])

{-

Instances of a complex scalar expression.
-}
instance Floating ScalarC where
    sqrt (ScalarC (Expression n es)) = ScalarC $ addEdge' es (Op Dim0 Sqrt [n])
    pi = (fromDbl pi) +: (fromDbl 0)
    exp (ScalarC (Expression n es)) = ScalarC $ addEdge' es (Op Dim0 Exp [n])
    cos (ScalarC (Expression n es)) = ScalarC $ addEdge' es (Op Dim0 Cos [n])
    sin (ScalarC (Expression n es)) = ScalarC $ addEdge' es (Op Dim0 Sin [n])
    tan (ScalarC (Expression n es)) = ScalarC $ addEdge' es (Op Dim0 Tan [n])
    log (ScalarC (Expression n es)) = ScalarC $ addEdge' es (Op Dim0 Log [n])
    asin (ScalarC (Expression n es)) = ScalarC $ addEdge' es (Op Dim0 Asin [n])
    acos (ScalarC (Expression n es)) = ScalarC $ addEdge' es (Op Dim0 Acos [n])
    atan (ScalarC (Expression n es)) = ScalarC $ addEdge' es (Op Dim0 Atan [n])
    sinh (ScalarC (Expression n es)) = ScalarC $ addEdge' es (Op Dim0 Sinh [n])
    cosh (ScalarC (Expression n es)) = ScalarC $ addEdge' es (Op Dim0 Cosh [n])
    atanh x = error $ "HInst. atanh not defined for Complex Scalar " ++ show x
    acosh x = error $ "HInst. acosh not defined for Complex Scalar " ++ show x
    asinh x = error $ "HInst. asinh not defined for Complex Scalar " ++ show x

{-


-}
instance RealVectorSpace OneD Scalar where
    scale (Scalar s) (OneD v) =
        let (Expression _ es, (n1, n2)) = merge s v
         in OneD $ addEdge' es $ Op (getDimE es n2) ScaleV [n1, n2]
    dot (OneD v1) (OneD v2) =
        let (Expression _ es, (n1, n2)) = merge v1 v2
         in if getDimE es n1 == getDimE es n2
                then Scalar $ addEdge' es (Op Dim0 Dot [n1, n2])
                else error $ "dot 1D sizes differ "
    subMask (OneD v1) (OneD v2) =
        let (Expression _ es, (n1, n2)) = merge v1 v2
         in if getDimE es n1 == getDimE es n2
                then OneD $ addEdge' es (Op (getDimE es n1) SubMask [n1, n2])
                else error $
                     "subMask 1D sizes differ " ++
                     show (getDimE es n1, getDimE es n2)
    negMask (OneD v1) (OneD v2) =
        let (Expression _ es, (n1, n2)) = merge v1 v2
         in if getDimE es n1 == getDimE es n2
                then OneD $ addEdge' es (Op (getDimE es n1) NegMask [n1, n2])
                else error $ "negMask 1D sizes differ "
    projSS ss (OneD (Expression n e)) = OneD eOut
      where
        eOut = addEdge' e $ Op (projDim (getDimE e n) ss) (Project ss) [n]
    injectSS ss (OneD (Expression n e)) = OneD eOut
      where
        eOut = addEdge' e $ Op (injectDim (getDimE e n) ss) (Inject ss) [n]
    mapR fun (OneD (Expression n e)) = OneD eOut
      where
        eOut =
            addEdge' e $
            Op (getDimE e n) (MapND mapOut $ C.pack "anonymous") [n]
        Scalar mapOut = fun $ var "anonymous"

{-

-}
instance RealVectorSpace TwoD Scalar where
    scale (Scalar s) (TwoD v) =
        let (Expression _ es, (n1, n2)) = merge s v
         in TwoD $ addEdge' es (Op (getDimE es n2) ScaleV [n1, n2])
    dot (TwoD v1) (TwoD v2) =
        let (Expression _ es, (n1, n2)) = merge v1 v2
         in if getDimE es n1 == getDimE es n2
                then Scalar $ addEdge' es (Op Dim0 Dot [n1, n2])
                else error $ "dot 1D sizes differ " ++ show (v1, v2)
    subMask (TwoD v1) (TwoD v2) =
        let (Expression _ es, (n1, n2)) = merge v1 v2
         in if getDimE es n1 == getDimE es n2
                then TwoD $ addEdge' es (Op (getDimE es n1) SubMask [n1, n2])
                else error $
                     "subMask 1D sizes differ " ++
                     show (getDimE es n1, getDimE es n2)
    negMask (TwoD v1) (TwoD v2) =
        let (Expression _ es, (n1, n2)) = merge v1 v2
         in if getDimE es n1 == getDimE es n2
                then TwoD $ addEdge' es (Op (getDimE es n1) NegMask [n1, n2])
                else error $ "negMask 1D sizes differ "
    projSS ss (TwoD (Expression n e)) = TwoD eOut
      where
        inDim@(Dim2 (d1, d2)) = getDimE e n
        outDims =
            case ss of
                SSCrop _bounds dims ->
                    if dims == [d1, d2]
                        then projDim inDim ss
                        else error $
                             "HI.projSS adding " ++
                             show (d1, d2, ss) ++ "\n" ++ pretty (e, n)
                _ -> mt ("projSS ok " ++ show (d1, d2, ss)) $ projDim inDim ss
        eOut = addEdge' e $ Op outDims (Project ss) [n]
    injectSS ss (TwoD (Expression n e)) = TwoD eOut
      where
        eOut = addEdge' e (Op (injectDim (getDimE e n) ss) (Inject ss) [n])
    mapR fun (TwoD (Expression n e)) = TwoD eOut
      where
        eOut =
            addEdge' e $
            Op (getDimE e n) (MapND mapOut $ C.pack "anonymous") [n]
        Scalar mapOut = fun $ var "anonymous"

{-

The simpler version (copied from OneD) seems to work and doesn't print out stuff.  What's the difference?

If the dimensions line up, then there is no difference in what the function does; however, if there is an SSCrop and the dimensions do not line up, then there is an error.  I've replaced the trace with an mt so that it only traces in debug.

HashedExamples, line 587, ft2d, raises this error, so it's useful
-}
{--
  projSS ss (TwoD (Expression n e)) = TwoD eOut
    where
      eOut = addEdge' e $ Op (projDim (getDimE e n) ss) (Project ss) [n]
--}
--}
{-

-}
instance RealVectorSpace TwoDSparse Scalar where
    scale (Scalar s) (TwoDSparse sL@(SparseList2D _ _ _ _) v) =
        let (Expression _ es, (n1, n2)) = merge s v
         in TwoDSparse sL $ addEdge' es (Op (getDimE es n2) ScaleV [n1, n2])
    subMask a b = error $ "2DS.subMask not implemented for " ++ show (a, b)
    negMask a b = error $ "2DS.negMask not implemented for " ++ show (a, b)
    dot (TwoDSparse sampleList1 v1) (TwoDSparse sampleList2 v2) =
        let (Expression _ es, (n1, n2)) = merge v1 v2
         in if getDimE es n1 == getDimE es n2 && sampleList1 == sampleList2
                then Scalar $ addEdge' es (Op Dim0 Dot [n1, n2])
                else error $
                     "dot 2d sparse sizes" ++
                     show
                         ( getDimE es n1
                         , getDimE es n2
                         , sampleList1
                         , sampleList2)
    projSS _ss (TwoDSparse _sL _) = error "TwoDSparse.projSS no sense"
    injectSS _ss (TwoDSparse _sL _) = error "TwoDSparse.injectSS no sense"
    mapR fun (TwoDSparse sL@(SparseList2D _ _ _ _) (Expression n e)) =
        TwoDSparse sL eOut
      where
        eOut =
            addEdge' e $
            Op (getDimE e n) (MapND mapOut $ C.pack "anonymous") [n]
        Scalar mapOut = fun $ var "anonymous"

{-

-}
instance SubsampledSpace TwoD TwoDSparse SparseList2D where
    projSparse sL@(SparseList2D len skipDim _sL2 (min2, max2)) (TwoD (Expression n e)) =
        let (ok, outDims) =
                case (skipDim, getDimE e n) of
                    (Dii1, Dim2 dd@(d, _)) ->
                        (min2 >= 0 && max2 < d, Dim2SL1 dd len)
                    (Dii2, Dim2 dd@(_, d)) ->
                        (min2 >= 0 && max2 < d, Dim2SL2 dd len)
                    dd ->
                        error $ "projSparse expected two dimensions " ++ show dd
         in if ok
                then TwoDSparse sL $
                     addEdge' e $ Op outDims (Project (SSList2D sL)) [n]
                else error $
                     "projSparse bad sampleList " ++ show (getDimE e n, sL)
    injectSparse (TwoDSparse sL@(SparseList2D _len sparseDim _sL2 (min2, max2)) (Expression n e)) =
        let (ok, outDims) =
                case getDimE e n of
                    Dim2SL1 dd@(d, _) _ ->
                        (sparseDim == Dii2 && min2 >= 0 && max2 < d, Dim2 dd)
                    Dim2SL2 dd@(_, d) _ ->
                        (sparseDim == Dii1 && min2 >= 0 && max2 < d, Dim2 dd)
                    dd ->
                        error $
                        "injectSparse expected two dimensions " ++ show dd
         in if ok
                then TwoD $ addEdge' e $ Op outDims (Inject (SSList2D sL)) [n]
                else error $
                     "injectSparse bad sampleList " ++ show (outDims, sL)

{-

instance (Complex vc v, SubsampledSpace v vs sl, Complex vcs vs) => SubsampledSpace vc vcs sl where
  projSparse sL v = (projSparse sL $ xRe v) +: (projSparse sL $ xIm v)
  injectSparse v = (injectSparse $ xRe v) +: (injectSparse $ xIm v)
%\end{code}

-}
instance SubsampledSpace ThreeD ThreeDSparse SparseList3D where
    projSparse sL@(SparseList3D len skipDim _sL2 (min2, max2) _sL3 (min3, max3)) (ThreeD (Expression n e)) =
        let (ok, outDims) =
                case (skipDim, getDimE e n) of
                    (Diii1, Dim3 dd@(_, d2, d3)) ->
                        ( min2 >= 0 && max2 < d2 && min3 >= 0 && max3 < d3
                        , Dim3SL1 dd len)
                    (Diii2, Dim3 dd@(d3, _, d2)) ->
                        ( min2 >= 0 && max2 < d2 && min3 >= 0 && max3 < d3
                        , Dim3SL2 dd len)
                    (Diii3, Dim3 dd@(d2, d3, _)) ->
                        ( min2 >= 0 && max2 < d2 && min3 >= 0 && max3 < d3
                        , Dim3SL3 dd len)
                    dd ->
                        error $
                        "projSparse expected three dimensions " ++ show dd
         in if ok
                then ThreeDSparse sL $
                     addEdge' e $ Op outDims (Project (SSList3D sL)) [n]
                else error $
                     "projSparse bad sampleList " ++ show (getDimE e n, sL)
    injectSparse (ThreeDSparse sL@(SparseList3D _len sparseDim _sL2 (min2, max2) _sL3 (min3, max3)) (Expression n e)) =
        let (ok, outDims) =
                case (sparseDim, getDimE e n) of
                    (Diii1, Dim3SL1 dd@(_, d2, d3) _) ->
                        ( min2 >= 0 && max2 < d2 && min3 >= 0 && max3 < d3
                        , Dim3 dd)
                    (Diii2, Dim3SL2 dd@(d3, _, d2) _) ->
                        ( min2 >= 0 && max2 < d2 && min3 >= 0 && max3 < d3
                        , Dim3 dd)
                    (Diii3, Dim3SL3 dd@(d2, d3, _) _) ->
                        ( min2 >= 0 && max2 < d2 && min3 >= 0 && max3 < d3
                        , Dim3 dd)
                    dd ->
                        error $
                        "injectSparse expected three dimensions " ++ show dd
         in if ok
                then ThreeD $ addEdge' e $ Op outDims (Inject (SSList3D sL)) [n]
                else error $
                     "injectSparse bad sampleList " ++ show (outDims, sL)

{-

-}
instance RealVectorSpace ThreeD Scalar where
    scale (Scalar s) (ThreeD v) =
        let (Expression _ es, (n1, n2)) = merge s v
         in ThreeD $ addEdge' es (Op (getDimE es n2) ScaleV [n1, n2])
    subMask (ThreeD v1) (ThreeD v2) =
        let (Expression _ es, (n1, n2)) = merge v1 v2
         in if getDimE es n1 == getDimE es n2
                then ThreeD $ addEdge' es (Op (getDimE es n1) SubMask [n1, n2])
                else error $
                     "subMask 1D sizes differ " ++
                     show (getDimE es n1, getDimE es n2)
    negMask (ThreeD v1) (ThreeD v2) =
        let (Expression _ es, (n1, n2)) = merge v1 v2
         in if getDimE es n1 == getDimE es n2
                then ThreeD $ addEdge' es (Op (getDimE es n1) NegMask [n1, n2])
                else error $ "negMask 3D sizes differ "
    dot (ThreeD v1) (ThreeD v2) =
        let (Expression _ es, (n1, n2)) = merge v1 v2
         in if getDimE es n1 == getDimE es n2
                then Scalar $ addEdge' es (Op Dim0 Dot [n1, n2])
                else error $ "dot 3D sizes differ " ++ show (v1, v2)
    projSS ss (ThreeD (Expression n e)) = ThreeD eOut
      where
        eOut = addEdge' e (Op (projDim (getDimE e n) ss) (Project ss) [n])
    injectSS ss (ThreeD (Expression n e)) = ThreeD eOut
      where
        eOut = addEdge' e (Op (injectDim (getDimE e n) ss) (Inject ss) [n])
    mapR fun (ThreeD (Expression n e)) = ThreeD eOut
      where
        eOut =
            addEdge' e $
            Op (getDimE e n) (MapND mapOut $ C.pack "anonymous") [n]
        Scalar mapOut = fun $ var "anonymous"

{-
-}
instance RealVectorSpace OneDC Scalar where
    scale s v = (s `scale` (xRe v)) +: (s `scale` (xIm v))
    subMask v1 v2 = (subMask (xRe v1) (xRe v2)) +: (subMask (xIm v1) (xIm v2))
    negMask v1 v2 = (negMask (xRe v1) (xRe v2)) +: (negMask (xIm v1) (xIm v2))
    dot v1 v2 = (dot (xRe v1) (xRe v2)) + (dot (xIm v1) (xIm v2))
    projSS ss v = (projSS ss (xRe v)) +: (projSS ss (xIm v))
    injectSS ss v = (injectSS ss (xRe v)) +: (injectSS ss (xIm v))
    mapR fun v = (mapR fun (xRe v)) +: (mapR fun (xIm v))

instance RealVectorSpace TwoDC Scalar where
    scale s v = (s `scale` (xRe v)) +: (s `scale` (xIm v))
    subMask v1 v2 = (subMask (xRe v1) (xRe v2)) +: (subMask (xIm v1) (xIm v2))
    negMask v1 v2 = (negMask (xRe v1) (xRe v2)) +: (negMask (xIm v1) (xIm v2))
    dot v1 v2 = (dot (xRe v1) (xRe v2)) + (dot (xIm v1) (xIm v2))
    projSS ss v = (projSS ss (xRe v)) +: (projSS ss (xIm v))
    injectSS ss v = (injectSS ss (xRe v)) +: (injectSS ss (xIm v))
    mapR fun v = (mapR fun (xRe v)) +: (mapR fun (xIm v))

instance RealVectorSpace ThreeDC Scalar where
    scale s v = (s `scale` (xRe v)) +: (s `scale` (xIm v))
    subMask v1 v2 = (subMask (xRe v1) (xRe v2)) +: (subMask (xIm v1) (xIm v2))
    negMask v1 v2 = (negMask (xRe v1) (xRe v2)) +: (negMask (xIm v1) (xIm v2))
    dot v1 v2 = (dot (xRe v1) (xRe v2)) + (dot (xIm v1) (xIm v2))
    projSS ss v = (projSS ss (xRe v)) +: (projSS ss (xIm v))
    injectSS ss v = (injectSS ss (xRe v)) +: (injectSS ss (xIm v))
    mapR fun v = (mapR fun (xRe v)) +: (mapR fun (xIm v))

{-
-}
instance (RealVectorSpace v s, Num s) => RealVectorSpace (v, v) s where
    scale s (x, y) = (scale s x, scale s y)
    subMask (a', a'') (b', b'') = (subMask a' b', subMask a'' b'')
    negMask (a', a'') (b', b'') = (negMask a' b', negMask a'' b'')
    injectSS ss (x, y) = (injectSS ss x, injectSS ss y)
    projSS ss (x, y) = (projSS ss x, projSS ss y)
    dot (x1, y1) (x2, y2) = (dot x1 x2) + (dot y1 y2)
    mapR fun (x1, x2) = (mapR fun x1, mapR fun x2)

instance (RealVectorSpace v s, Num s, Show v) => RealVectorSpace [v] s where
    scale s xs = map (scale s) xs
    subMask a b = [subMask a' b' | (a', b') <- zip a b]
    negMask a b = [negMask a' b' | (a', b') <- zip a b]
    injectSS ss xs = map (injectSS ss) xs
    projSS ss xs = map (projSS ss) xs
    dot xs ys =
        if length xs == length ys
            then sum $ zipWith dot xs ys
            else error $ "dotL length mismatch " ++ show (xs, ys)
             -- *** infinite list will cause infinite recursion
    mapR fun = map (mapR fun)

{-
-}
instance RealVectorSpace ThreeDSparse Scalar where
    scale (Scalar s) (ThreeDSparse sL v) =
        let (Expression _ es, (n1, n2)) = merge s v
         in ThreeDSparse sL $ addEdge' es (Op (getDimE es n2) ScaleV [n1, n2])
    subMask a b = error $ "3DS.subMask not implemented for " ++ show (a, b)
    negMask a b = error $ "3DS.negMask not implemented for " ++ show (a, b)
    dot (ThreeDSparse sL1 v1) (ThreeDSparse sL2 v2) =
        let (Expression _ es, (n1, n2)) = merge v1 v2
         in if getDimE es n1 == getDimE es n2
                then Scalar $ addEdge' es (Op Dim0 Dot [n1, n2])
                else error $ "dot 1D sizes differ " ++ show (v1, v2, sL1, sL2)
    projSS _ _ = error "RVS.ThreeDSparse.projSS"
    injectSS _ _ = error "RVS.ThreeDSparse.projSS"
    mapR fun (ThreeDSparse sL (Expression n e)) = ThreeDSparse sL eOut
      where
        eOut =
            addEdge' e $
            Op (getDimE e n) (MapND mapOut $ C.pack "anonymous") [n] -- JLMP switch to SCZ
        Scalar mapOut = fun $ var "anonymous"

{-


Instances which build 1D expressions.
-}
instance Num OneD where
    negate (OneD (Expression n exprs)) =
        OneD $ addEdge' exprs' (Op (getDimE exprs n) ScaleV [neg, n])
      where
        (exprs', neg) = addEdge exprs (Const Dim0 (-1))
    fromInteger x = error $ "can't promote integer to 1d vector " ++ show x
    (OneD expr1) + (OneD expr2) =
        let (Expression _ es, (n1, n2)) = merge expr1 expr2
         in if getDimE' expr1 == getDimE' expr2
                then OneD $ addEdge' es (Op (getDimE' expr1) Sum [n1, n2])
                else error $
                     "can't add 1d vectors of lengths " ++ show (expr1, expr2)
    _ * _ = error "can't multiply 1d vectors"
    abs _ = error "can't abs 1d vectors"
    signum _ = error "can't signum 1d vectors"

{-

Instance to convert back and forth between real and complex nodes.
-}
instance Complex OneDC OneD where
    reO@(OneD reExpr) +: imO@(OneD imExpr) =
        let (Expression _ es, (reN, imN)) = merge reExpr imExpr
         in if getDimE' reExpr == getDimE' imExpr
                then OneDC $
                     addEdge' es (Op (getDimE' reExpr) RealImag [reN, imN])
                else error $
                     "HI.Complex OneDC +: dimension mismatch " ++
                     show (reO, imO)
    iRe (OneD (Expression n exprs)) =
        let Expression n0 exprs1 = addEdge' exprs (Const (getDimE exprs n) 0)
         in OneDC $ addEdge' exprs1 (Op (getDimE exprs n) RealImag [n, n0])
    iIm (OneD (Expression n exprs)) =
        let Expression n0 exprs1 = addEdge' exprs (Const (getDimE exprs n) 0)
         in OneDC $ addEdge' exprs1 (Op (getDimE exprs n) RealImag [n0, n])
    xRe (OneDC (Expression n exprs)) =
        OneD $ addEdge' exprs (Op (getDimE exprs n) RealPart [n])
    xIm (OneDC (Expression n exprs)) =
        OneD $ addEdge' exprs (Op (getDimE exprs n) ImagPart [n])

{-

Instances which build 1D Complex expressions.
-}
instance Num OneDC where
    negate (OneDC (Expression n exprs)) =
        OneDC $ addEdge' exprs' (Op (getDimE exprs n) ScaleV [neg, n])
      where
        (exprs', neg) = addEdge exprs (Const Dim0 (-1))
    fromInteger x = error $ "can't promote integer to 1d vector " ++ show x
    (OneDC expr1) + (OneDC expr2) =
        let (Expression _ es, (n1, n2)) = merge expr1 expr2
         in if getDimE' expr1 == getDimE' expr2
                then OneDC $ addEdge' es (Op (getDimE' expr1) Sum [n1, n2])
                else error $
                     "can't add 1d complex vectors of lengths " ++
                     show (expr1, expr2)
    _ * _ = error "can't multiply 1d complex vectors"
    abs _ = error "can't abs 1d complex vectors"
    signum _ = error "can't signum 1d complex vectors"

{-

Instances which build 2D expressions.
-}
instance Num TwoD where
    negate (TwoD (Expression n exprs)) =
        TwoD $ addEdge' exprs' (Op (getDimE exprs n) ScaleV [neg, n])
      where
        (exprs', neg) = addEdge exprs (Const Dim0 (-1))
    fromInteger x = error $ "can't promote integer to 2d vector " ++ show x
    (TwoD expr1) + (TwoD expr2) =
        let (Expression _ es, (n1, n2)) = merge expr1 expr2
         in if getDimE' expr1 == getDimE' expr2
                then TwoD $ addEdge' es (Op (getDimE' expr1) Sum [n1, n2])
                else error $
                     "can't add 2d vectors of lengths " ++ show (expr1, expr2)
    _ * _ = error "can't multiply 2d vectors"
    abs _ = error "can't abs 2d vectors"
    signum _ = error "can't signum 2d vectors"

{-

Instance to convert back and forth between real and complex nodes.
-}
instance Complex TwoDC TwoD where
    reO@(TwoD reExpr) +: imO@(TwoD imExpr) =
        let (Expression _ es, (reN, imN)) = merge reExpr imExpr
         in if getDimE' reExpr == getDimE' imExpr
                then TwoDC $
                     addEdge' es (Op (getDimE' reExpr) RealImag [reN, imN])
                else error $
                     "HI.Complex TwoDC +: dimension mismatch " ++
                     show (reO, imO)
    iRe (TwoD (Expression n exprs)) =
        let Expression n0 exprs1 = addEdge' exprs (Const (getDimE exprs n) 0)
         in TwoDC $ addEdge' exprs1 (Op (getDimE exprs n) RealImag [n, n0])
    iIm (TwoD (Expression n exprs)) =
        let Expression n0 exprs1 = addEdge' exprs (Const (getDimE exprs n) 0)
         in TwoDC $ addEdge' exprs1 (Op (getDimE exprs n) RealImag [n0, n])
    xRe (TwoDC (Expression n exprs)) =
        TwoD $ addEdge' exprs (Op (getDimE exprs n) RealPart [n])
    xIm (TwoDC (Expression n exprs)) =
        TwoD $ addEdge' exprs (Op (getDimE exprs n) ImagPart [n])

{-

Instance to convert back and forth between real and complex nodes.
-}
instance Complex TwoDCSparse TwoDSparse where
    reO@(TwoDSparse sLre@(SparseList2D _len _ _ _) reExpr) +: imO@(TwoDSparse sLim imExpr) =
        let (Expression _ es, (reN, imN)) = merge reExpr imExpr
         in if getDimE' reExpr == getDimE' imExpr
                then TwoDCSparse sLre $
                     addEdge' es (Op (getDimE' reExpr) RealImag [reN, imN])
                else error $
                     "HI.Complex TwoDCSparse +: dimension mismatch " ++
                     show (reO, imO, sLre, sLim)
    iRe (TwoDSparse sL@(SparseList2D _len _ _ _) (Expression n exprs)) =
        let Expression n0 exprs1 = addEdge' exprs (Const (getDimE exprs n) 0)
         in TwoDCSparse sL $
            addEdge' exprs1 (Op (getDimE exprs n) RealImag [n, n0])
    iIm (TwoDSparse sL@(SparseList2D _len _ _ _) (Expression n exprs)) =
        let Expression n0 exprs1 = addEdge' exprs (Const (getDimE exprs n) 0)
         in TwoDCSparse sL $
            addEdge' exprs1 (Op (getDimE exprs n) RealImag [n0, n])
    xRe (TwoDCSparse sL@(SparseList2D _len _ _ _) (Expression n exprs)) =
        TwoDSparse sL $ addEdge' exprs (Op (getDimE exprs n) RealPart [n])
    xIm (TwoDCSparse sL@(SparseList2D _len _ _ _) (Expression n exprs)) =
        TwoDSparse sL $ addEdge' exprs (Op (getDimE exprs n) ImagPart [n])

{-
-}
instance Complex ThreeDCSparse ThreeDSparse where
    reO@(ThreeDSparse sLre@(SparseList3D _len _ _ _ _ _) reExpr) +: imO@(ThreeDSparse sLim imExpr) =
        let (Expression _ es, (reN, imN)) = merge reExpr imExpr
         in if getDimE' reExpr == getDimE' imExpr
                then ThreeDCSparse sLre $
                     addEdge' es (Op (getDimE' reExpr) RealImag [reN, imN])
                else error $
                     "HI.Complex ThreeDSparse +: dimension mismatch " ++
                     show (reO, imO, sLre, sLim)
    iRe (ThreeDSparse sL@(SparseList3D _len _ _ _ _ _) (Expression n exprs)) =
        let Expression n0 exprs1 = addEdge' exprs (Const (getDimE exprs n) 0)
         in ThreeDCSparse sL $
            addEdge' exprs1 (Op (getDimE exprs n) RealImag [n, n0])
    iIm (ThreeDSparse sL@(SparseList3D _len _ _ _ _ _) (Expression n exprs)) =
        let Expression n0 exprs1 = addEdge' exprs (Const (getDimE exprs n) 0)
         in ThreeDCSparse sL $
            addEdge' exprs1 (Op (getDimE exprs n) RealImag [n0, n])
    xRe (ThreeDCSparse sL@(SparseList3D _len _ _ _ _ _) (Expression n exprs)) =
        ThreeDSparse sL $ addEdge' exprs (Op (getDimE exprs n) RealPart [n])
    xIm (ThreeDCSparse sL@(SparseList3D _len _ _ _ _ _) (Expression n exprs)) =
        ThreeDSparse sL $ addEdge' exprs (Op (getDimE exprs n) ImagPart [n])

{-

Instances which build 1D Complex expressions.
-}
instance ComplexVectorSpace OneDC OneD ScalarC Scalar where
    scaleC (ScalarC se) (OneDC oe) =
        let (Expression _ es, (ns, no)) = merge se oe
         in OneDC $ addEdge' es (Op (getDimE' oe) ScaleV [ns, no])
    scaleR (Scalar se) (OneDC oe) =
        let (Expression _ es, (ns, no)) = merge se oe
         in OneDC $ addEdge' es (Op (getDimE' oe) ScaleV [ns, no])
    mapC funC (OneDC (Expression n e)) = OneDC eOut
      where
        eOut =
            addEdge' e $
            Op (getDimE e n) (MapND mapOut $ C.pack "anonymous") [n]
        ScalarC mapOut = funC $ varc "anonymous"
    realPartV (OneDC (Expression n exprs)) =
        OneD $ addEdge' exprs (Op (getDimE exprs n) RealPart [n])
    imagPartV (OneDC (Expression n exprs)) =
        OneD $ addEdge' exprs (Op (getDimE exprs n) ImagPart [n])
    magV _ = error "magV not implemented"

{-

Instances which build 2D Complex expressions.
-}
instance ComplexVectorSpace TwoDC TwoD ScalarC Scalar where
    scaleC (ScalarC se) (TwoDC oe) =
        let (Expression _ es, (ns, no)) = merge se oe
         in TwoDC $ addEdge' es (Op (getDimE' oe) ScaleV [ns, no])
    scaleR (Scalar se) (TwoDC oe) =
        let (Expression _ es, (ns, no)) = merge se oe
         in TwoDC $ addEdge' es (Op (getDimE' oe) ScaleV [ns, no])
    mapC funC (TwoDC (Expression n e)) = TwoDC eOut
      where
        eOut =
            addEdge' e $
            Op (getDimE e n) (MapND mapOut $ C.pack "anonymous") [n]
        ScalarC mapOut = funC $ varc "anonymous"
    realPartV (TwoDC (Expression n exprs)) =
        TwoD $ addEdge' exprs (Op (getDimE exprs n) RealPart [n])
    imagPartV (TwoDC (Expression n exprs)) =
        TwoD $ addEdge' exprs (Op (getDimE exprs n) ImagPart [n])
    magV (TwoDC (Expression n exprs)) =
        let (exprs1, re) = addEdge exprs $ Op (getDimE exprs n) RealPart [n]
            (exprs2, im) = addEdge exprs1 $ Op (getDimE exprs n) ImagPart [n]
            Scalar sczE =
                sqrt $
                (relElem 0 ZeroMargin [0, 0]) ^ 2 +
                (relElem 1 ZeroMargin [0, 0]) ^ 2
         in TwoD $ addEdge' exprs2 $ mkSCZ' (getDimE exprs n) sczE [re, im]

{-

Instances which build 2D Complex expressions.
-}
instance ComplexVectorSpace TwoDCSparse TwoDSparse ScalarC Scalar where
    scaleC (ScalarC se) (TwoDCSparse sL oe) =
        let (Expression _ es, (ns, no)) = merge se oe
         in TwoDCSparse sL $ addEdge' es (Op (getDimE' oe) ScaleV [ns, no])
    scaleR (Scalar se) (TwoDCSparse sL oe) =
        let (Expression _ es, (ns, no)) = merge se oe
         in TwoDCSparse sL $ addEdge' es (Op (getDimE' oe) ScaleV [ns, no])
    mapC funC (TwoDCSparse sL (Expression n e)) = TwoDCSparse sL eOut
      where
        eOut =
            addEdge' e $
            Op (getDimE e n) (MapND mapOut $ C.pack "anonymous") [n]
        ScalarC mapOut = funC $ varc "anonymous"
    realPartV (TwoDCSparse sL (Expression n exprs)) =
        TwoDSparse sL $ addEdge' exprs (Op (getDimE exprs n) RealPart [n])
    imagPartV (TwoDCSparse sL (Expression n exprs)) =
        TwoDSparse sL $ addEdge' exprs (Op (getDimE exprs n) ImagPart [n])
    magV _ = error "magV not implemented"

{-


Instances which build 3D Complex expressions.
-}
instance ComplexVectorSpace ThreeDCSparse ThreeDSparse ScalarC Scalar where
    scaleC (ScalarC se) (ThreeDCSparse sL@(SparseList3D _len _ _ _ _ _) oe) =
        let (Expression _ es, (ns, no)) = merge se oe
         in ThreeDCSparse sL $ addEdge' es (Op (getDimE' oe) ScaleV [ns, no])
    scaleR (Scalar se) (ThreeDCSparse sL@(SparseList3D _len _ _ _ _ _) oe) =
        let (Expression _ es, (ns, no)) = merge se oe
         in ThreeDCSparse sL $ addEdge' es (Op (getDimE' oe) ScaleV [ns, no])
    mapC funC (ThreeDCSparse sL@(SparseList3D _ _ _ _ _ _) (Expression n e)) =
        ThreeDCSparse sL eOut
      where
        eOut =
            addEdge' e $
            Op (getDimE e n) (MapND mapOut $ C.pack "anonymous") [n]
        ScalarC mapOut = funC $ varc "anonymous"
    realPartV (ThreeDCSparse sL@(SparseList3D _len _ _ _ _ _) (Expression n exprs)) =
        ThreeDSparse sL $ addEdge' exprs (Op (getDimE exprs n) RealPart [n])
    imagPartV (ThreeDCSparse sL@(SparseList3D _len _ _ _ _ _) (Expression n exprs)) =
        ThreeDSparse sL $ addEdge' exprs (Op (getDimE exprs n) ImagPart [n])
    magV _ = error "magV not implemented"

{-

Instance which build 2D Complex expressions
-}
instance Num TwoDCSparse where
    negate (TwoDCSparse sL (Expression n exprs)) =
        TwoDCSparse sL $ addEdge' exprs' (Op (getDimE exprs n) ScaleV [neg, n])
      where
        (exprs', neg) = addEdge exprs (Const Dim0 (-1))
    fromInteger x =
        error $ "can't promote integer to 2d sparse vector " ++ show x
    (TwoDCSparse sL1 expr1) + (TwoDCSparse sL2 expr2) =
        let (Expression _ es, (n1, n2)) = merge expr1 expr2
         in if getDimE' expr1 == getDimE' expr2
                then TwoDCSparse sL1 $
                     addEdge' es (Op (getDimE' expr1) Sum [n1, n2])
                else error $
                     "can't add 2d complex vectors of lengths " ++
                     show (sL1, sL2)
    _ * _ = error "can't multiply 2d complex vectors"
    abs _ = error "can't abs 2d complex vectors"
    signum _ = error "can't signum 2d complex vectors"

{-

Instance which build 2D Complex expressions
-}
instance Num TwoDC where
    negate (TwoDC (Expression n exprs)) =
        TwoDC $ addEdge' exprs' (Op (getDimE exprs n) ScaleV [neg, n])
      where
        (exprs', neg) = addEdge exprs (Const Dim0 (-1))
    fromInteger x =
        error $ "can't promote integer to 2d complex vector " ++ show x
    (TwoDC expr1) + (TwoDC expr2) =
        let (Expression _ es, (n1, n2)) = merge expr1 expr2
         in if getDimE' expr1 == getDimE' expr2
                then TwoDC $ addEdge' es (Op (getDimE' expr1) Sum [n1, n2])
                else error $ "can't add 2d complex vectors of lengths "
    _ * _ = error "can't multiply 2d complex vectors"
    abs _ = error "can't abs 2d complex vectors"
    signum _ = error "can't signum 2d complex vectors"

{-

Instances which build 3D expressions.
-}
instance Num ThreeD where
    negate (ThreeD (Expression n exprs)) =
        let (exprs', neg) = addEdge exprs (Const Dim0 (-1))
         in ThreeD $ addEdge' exprs' (Op (getDimE exprs n) ScaleV [neg, n])
    fromInteger x = error $ "can't promote integer to 3d vector " ++ show x
    (ThreeD expr1) + (ThreeD expr2) =
        let (Expression _ es, (n1, n2)) = merge expr1 expr2
         in if getDimE' expr1 == getDimE' expr2
                then ThreeD $ addEdge' es (Op (getDimE' expr1) Sum [n1, n2])
                else error $ "can't add 3d vectors of lengths "
    _ * _ = error "can't multiply 3d vectors"
    abs _ = error "can't abs 3d vectors"
    signum _ = error "can't signum 3d vectors"

{-

Instance which build 3D Complex Sparse expressions
-}
instance Num ThreeDCSparse where
    negate (ThreeDCSparse sL (Expression n exprs)) =
        ThreeDCSparse sL $
        addEdge' exprs' (Op (getDimE exprs n) ScaleV [neg, n])
      where
        (exprs', neg) = addEdge exprs (Const Dim0 (-1))
    fromInteger x =
        error $ "can't promote integer to 3d complex sparse vector " ++ show x
    (ThreeDCSparse sL1 expr1) + (ThreeDCSparse sL2 expr2) =
        let (Expression _ es, (n1, n2)) = merge expr1 expr2
         in if getDimE' expr1 == getDimE' expr2 && sL1 == sL2
                then ThreeDCSparse sL1 $
                     addEdge' es (Op (getDimE' expr1) Sum [n1, n2])
                else error $ "can't add 3d complex vectors of lengths "
    _ * _ = error "can't multiply 3d complex vectors"
    abs _ = error "can't abs 3d complex vectors"
    signum _ = error "can't signum 3d complex vectors"

{-




Instance to convert back and forth between real and complex nodes.
-}
instance Complex ThreeDC ThreeD where
    reO@(ThreeD reExpr) +: imO@(ThreeD imExpr) =
        let (Expression _ es, (reN, imN)) = merge reExpr imExpr
         in if getDimE' reExpr == getDimE' imExpr
                then ThreeDC $
                     addEdge' es (Op (getDimE' reExpr) RealImag [reN, imN])
                else error $
                     "HI.Complex ThreeDC +: dimension mismatch " ++
                     show (reO, imO)
    iRe (ThreeD (Expression n exprs)) =
        let Expression n0 exprs1 = addEdge' exprs (Const (getDimE exprs n) 0)
         in ThreeDC $ addEdge' exprs1 (Op (getDimE exprs n) RealImag [n, n0])
    iIm (ThreeD (Expression n exprs)) =
        let Expression n0 exprs1 = addEdge' exprs (Const (getDimE exprs n) 0)
         in ThreeDC $ addEdge' exprs1 (Op (getDimE exprs n) RealImag [n0, n])
    xRe (ThreeDC (Expression n exprs)) =
        ThreeD $ addEdge' exprs (Op (getDimE exprs n) RealPart [n])
    xIm (ThreeDC (Expression n exprs)) =
        ThreeD $ addEdge' exprs (Op (getDimE exprs n) ImagPart [n])

{-



Instances which build 3D complex expressions.
-}
instance Num ThreeDC where
    negate (ThreeDC (Expression n exprs)) =
        let (exprs', neg) = addEdge exprs (Const Dim0 (-1))
         in ThreeDC $ addEdge' exprs' (Op (getDimE exprs n) ScaleV [neg, n])
    fromInteger x =
        error $ "can't promote integer to 3d complex vector " ++ show x
    (ThreeDC expr1) + (ThreeDC expr2) =
        let (Expression _ es, (n1, n2)) = merge expr1 expr2
         in if getDimE' expr1 == getDimE' expr2
                then ThreeDC $ addEdge' es (Op (getDimE' expr1) Sum [n1, n2])
                else error $
                     "can't add 3d complex vectors of lengths " ++
                     show (expr1, expr2)
    _ * _ = error "can't multiply 3d complex vectors"
    abs _ = error "can't abs 3d complex vectors"
    signum _ = error "can't signum 3d complex vectors"

{-



Instances for pairs
-}
instance (Num v) => Num (v, v) where
    (x, y) + (u, v) = (x + u, y + v)
    negate (x, y) = (negate x, negate y)
    fromInteger x = error $ "can't promote integer to pair vector " ++ show x
    _ * _ = error "can't multiply pairs"
    abs _ = error "can't abs pairs"
    signum _ = error "can't signum pairs"

{-

-}
instance (Integral a, Show a) => ShowHex (LinearCombination a) where
    showHex (LC c combos) =
        let nonConst = concatMap dispPair $ Map.toList combos
            dispPair (_, 0) = ""
            dispPair (name, 1) = " + " ++ name
            dispPair (name, c) = " + " ++ Numeric.showHex c (" * " ++ name)
         in case (c, null nonConst) of
                (0, True) -> "0"
                (0, _) -> drop 3 nonConst
                _ -> Numeric.showHex c (" + " ++ nonConst)

{-

-}
instance (Integral a, Show a) => ShowHex (PositiveCombination a) where
    showHex (PC combos) =
        if null nonZero
            then "0"
            else nonZero
      where
        nonZero =
            concat $
            L.intersperse " + " $ concatMap dispPair $ Map.toAscList combos
        dispPair (_, 0) = []
        dispPair (names, 1) = [showNames names]
        dispPair (names, c) = [showNames (showNeg c : names)]
        showNeg c =
            if c < 0
                then "(-" ++ Numeric.showHex (-c) ")"
                else Numeric.showHex c ""
        showNames names = concat $ L.intersperse " * " names

{-
instance (Show a, Eq a, Num a, Ord a) => Show (PositiveCombination a) where
  show (PC combos) = if null nonZero then "0" else nonZero
    where
      nonZero = concat $ L.intersperse " + " $ concatMap dispPair $ Map.toAscList combos

      dispPair (_,0) = []
      dispPair (names,1) = [showNames names]
      dispPair (names,c) = [showNames (showNeg c : names)]

      showNeg c = if c < 0 then "(" ++ show c ++ ")" else show c

      showNames names = concat $ L.intersperse " * " names


-}
instance ShowHex Scalar where
    showHex (Scalar e@(Expression node exprs)) =
        case I.lookup node exprs of
            Nothing ->
                "showHex Scalar found no node " ++
                (take 1000 $ show (node, exprs))
            Just (Var dims name) ->
                case dims of
                    Dim0 -> prettyHex e
                    _ ->
                        error $
                        "showHex Scalar got non-scalar " ++ show (Var dims name)
            Just (DVar dims name) ->
                case dims of
                    Dim0 -> prettyHex e
                    _ ->
                        error $
                        "showHex Scalar got non-scalar " ++ show (Var dims name)
            Just (Op dims op _inputs) ->
                case (dims, op) of
                    (Dim0, _) -> prettyHex e
                    _ ->
                        error $
                        "showHex Scalar got non-scalar " ++ show (dims, op)
            Just (Const dims d) ->
                case dims of
                    Dim0 -> Numeric.showHex (round d) ""
                    _ -> error $ "showHex Scalar has dims ! " ++ show dims
            Just _ -> prettyHex e

{-

-}
instance Show Scalar where
    show (Scalar e@(Expression node exprs)) =
        case I.lookup node exprs of
            Nothing ->
                "show Scalar found no node " ++ (take 1000 $ show (node, exprs))
            Just (Var dims name) ->
                case dims of
                    Dim0 -> pretty e
                    _ ->
                        error $
                        "show Scalar got non-scalar " ++ show (Var dims name)
            Just (DVar dims name) ->
                case dims of
                    Dim0 -> pretty e
                    _ ->
                        error $
                        "show Scalar got non-scalar " ++ show (Var dims name)
            Just (Op dims op _inputs) ->
                case (dims, op) of
                    (Dim0, _) -> pretty e
                    _ ->
                        error $ "show Scalar got non-scalar " ++ show (dims, op)
            Just (Const dims _) ->
                case dims of
                    Dim0 -> pretty e
                    DimUnknown ->
                        (pretty e) ++
                        (if skipDebug
                             then ""
                             else "?")
                    _ -> error $ "show Scalar has dims ! " ++ show dims
            Just _ -> pretty e

instance Show ScalarC where
    show (ScalarC e@(Expression node exprs)) =
        case I.lookup node exprs of
            Nothing ->
                "show Scalar found no node " ++ (take 1000 $ show (node, exprs))
            Just (Var dims name) ->
                case dims of
                    Dim0 -> pretty e
                    _ ->
                        error $
                        "show Scalar got non-scalar " ++ show (Var dims name)
            Just (DVar dims name) ->
                case dims of
                    Dim0 -> pretty e
                    _ ->
                        error $
                        "show Scalar got non-scalar " ++ show (Var dims name)
            Just (Op dims op _inputs) ->
                case (dims, op) of
                    (Dim0, _) -> pretty e
                    _ ->
                        error $ "show Scalar got non-scalar " ++ show (dims, op)
            Just (Const dims _) ->
                case dims of
                    Dim0 -> pretty e
                    _ -> error $ "show Scalar has dims ! " ++ show dims
            Just _ -> pretty e

instance Show OneD where
    show (OneD e@(Expression node exprs)) =
        case I.lookup node exprs of
            Nothing -> "show OneD no node " ++ (take 1000 $ show (node, exprs))
            Just (Var dims name) ->
                case dims of
                    Dim1 _ -> pretty e
                    _ -> error $ "show OneD non-scalar " ++ show (Var dims name)
            Just (DVar dims name) ->
                case dims of
                    Dim1 _ -> pretty e
                    _ ->
                        error $ "show OneD non-scalar " ++ show (DVar dims name)
            Just (Op dims op _inputs) ->
                case dims of
                    Dim1 _ -> pretty e
                    _ -> error $ "show OneD non-scalar " ++ show (dims, op)
            Just (Const dims _) ->
                case dims of
                    Dim1 _ -> pretty e
                    _ -> error $ "show OneD non-scalar " ++ show (dims, e)
            Just _ -> pretty e

instance Show OneDC where
    show (OneDC e@(Expression node exprs)) =
        case I.lookup node exprs of
            Nothing -> "show OneD no node " ++ (take 1000 $ show (node, exprs))
            Just (Var dims name) ->
                case dims of
                    Dim1 _ -> pretty e
                    _ ->
                        error $ "show OneDC non-scalar " ++ show (Var dims name)
            Just (DVar dims name) ->
                case dims of
                    Dim1 _ -> pretty e
                    _ ->
                        error $ "show OneDC non-scalar " ++ show (Var dims name)
            Just (Op dims op _inputs) ->
                case dims of
                    Dim1 _ -> pretty e
                    _ -> error $ "show OneDC non-scalar " ++ show (dims, op)
            Just (Const dims _) ->
                case dims of
                    Dim1 _ -> pretty e
                    _ -> error $ "show OneDC non-scalar " ++ show (dims, e)
            Just _ -> pretty e

instance Show TwoD where
    show (TwoD e@(Expression node exprs)) =
        case I.lookup node exprs of
            Nothing -> "show TwoD no node " ++ (take 1000 $ show (node, exprs))
            Just (Var dims name) ->
                case dims of
                    Dim2 _ -> pretty e
                    _ -> error $ "show TwoD non-scalar " ++ show (Var dims name)
            Just (DVar dims name) ->
                case dims of
                    Dim2 _ -> pretty e
                    _ -> error $ "show TwoD non-scalar " ++ show (Var dims name)
            Just (Op dims op _inputs) ->
                case dims of
                    Dim2 _ -> pretty e
                    _ -> error $ "show TwoD non-scalar " ++ show (dims, op)
            Just (Const dims _) ->
                case dims of
                    Dim2 _ -> pretty e
                    _ -> error $ "show TwoD non-scalar " ++ show (dims, e)
            Just _ -> pretty e

instance Show TwoDC where
    show (TwoDC e@(Expression node exprs)) =
        case I.lookup node exprs of
            Nothing -> "show TwoDC no node " ++ (take 1000 $ show (node, exprs))
            Just (Var dims name) ->
                case dims of
                    Dim2 _ -> pretty e
                    _ ->
                        error $ "show TwoDC non-scalar " ++ show (Var dims name)
            Just (DVar dims name) ->
                case dims of
                    Dim2 _ -> pretty e
                    _ ->
                        error $ "show TwoDC non-scalar " ++ show (Var dims name)
            Just (Op dims op _inputs) ->
                case dims of
                    Dim2 _ -> pretty e
                    _ -> error $ "show TwoDC non-scalar " ++ show (dims, op)
            Just (Const dims _) ->
                case dims of
                    Dim2 _ -> pretty e
                    _ -> error $ "show TwoDC non-scalar " ++ show (e)
            Just _ -> pretty e

instance Show TwoDSparse where
    show (TwoDSparse (SparseList2D _len _ _ _) e@(Expression node exprs)) =
        case I.lookup node exprs of
            Nothing ->
                "show TwoDSparse no node " ++ (take 1000 $ show (node, exprs))
            Just (Var dims name) ->
                case dims of
                    Dim2SL1 _ _ -> pretty e
                    Dim2SL2 _ _ -> pretty e
                    _ ->
                        error $
                        "show TwoDSparse non-scalar " ++ show (Var dims name)
            Just (DVar dims name) ->
                case dims of
                    Dim2SL1 _ _ -> pretty e
                    Dim2SL2 _ _ -> pretty e
                    _ ->
                        error $
                        "show TwoDSparse non-scalar " ++ show (Var dims name)
            Just (Op dims op _inputs) ->
                case dims of
                    Dim2SL1 _ _ -> pretty e
                    Dim2SL2 _ _ -> pretty e
                    _ ->
                        error $ "show TwoDSparse non-scalar " ++ show (dims, op)
            Just (Const dims _) ->
                case dims of
                    Dim2SL1 _ _ -> pretty e
                    Dim2SL2 _ _ -> pretty e
                    _ -> error $ "show TwoDSparse non-scalar " ++ show (e)
            Just _ -> pretty e

instance Show TwoDCSparse where
    show (TwoDCSparse (SparseList2D _len _ _ _) e@(Expression node exprs)) =
        case I.lookup node exprs of
            Nothing ->
                "show TwoDCSparse no node " ++ (take 1000 $ show (node, exprs))
            Just (Var dims name) ->
                case dims of
                    Dim2SL1 _ _ -> pretty e
                    Dim2SL2 _ _ -> pretty e
                    _ ->
                        error $
                        "show TwoDCSparse non-scalar " ++ show (Var dims name)
            Just (DVar dims name) ->
                case dims of
                    Dim2SL1 _ _ -> pretty e
                    Dim2SL2 _ _ -> pretty e
                    _ ->
                        error $
                        "show TwoDCSparse non-scalar " ++ show (Var dims name)
            Just (Op dims op _inputs) ->
                case dims of
                    Dim2SL1 _ _ -> pretty e
                    Dim2SL2 _ _ -> pretty e
                    _ ->
                        error $
                        "show TwoDCSparse non-scalar " ++ show (dims, op)
            Just (Const dims _) ->
                case dims of
                    Dim2SL1 _ _ -> pretty e
                    Dim2SL2 _ _ -> pretty e
                    _ -> error $ "show TwoDCSparse non-scalar " ++ show (e)
            Just _ -> pretty e

instance Show ThreeD where
    show (ThreeD e@(Expression node exprs)) =
        case I.lookup node exprs of
            Nothing ->
                "show ThreeD no node " ++ (take 1000 $ show (node, exprs))
            Just (Var dims name) ->
                case dims of
                    Dim3 _ -> pretty e
                    _ ->
                        error $
                        "show ThreeD non-scalar " ++ show (Var dims name)
            Just (DVar dims name) ->
                case dims of
                    Dim3 _ -> pretty e
                    _ ->
                        error $
                        "show ThreeD non-scalar " ++ show (Var dims name)
            Just (Op dims op _inputs) ->
                case dims of
                    Dim3 _ -> pretty e
                    _ -> error $ "show ThreeD non-scalar " ++ show (dims, op)
            Just (Const dims _) ->
                case dims of
                    Dim3 _ -> pretty e
                    _ -> error $ "show ThreeD non-scalar " ++ show (e)
            Just _ -> pretty e

instance Show ThreeDC where
    show (ThreeDC e@(Expression node exprs)) =
        case I.lookup node exprs of
            Nothing ->
                "show ThreeDC no node " ++ (take 1000 $ show (node, exprs))
            Just (Var dims name) ->
                case dims of
                    Dim3 _ -> pretty e
                    _ ->
                        error $
                        "show ThreeDC non-scalar " ++ show (Var dims name)
            Just (DVar dims name) ->
                case dims of
                    Dim3 _ -> pretty e
                    _ ->
                        error $
                        "show ThreeDC non-scalar " ++ show (Var dims name)
            Just (Op dims op _inputs) ->
                case dims of
                    Dim3 _ -> pretty e
                    _ -> error $ "show ThreeDC non-scalar " ++ show (dims, op)
            Just (Const dims _) ->
                case dims of
                    Dim3 _ -> pretty e
                    _ -> error $ "show ThreeDC non-scalar " ++ show (e)
            Just _ -> pretty e

instance Show ThreeDSparse where
    show (ThreeDSparse (SparseList3D _len _ _ _ _ _) e@(Expression node exprs)) =
        case I.lookup node exprs of
            Nothing ->
                "show ThreeDSparse no node " ++ (take 1000 $ show (node, exprs))
            Just (Var dims name) ->
                case dims of
                    Dim3SL1 _ _ -> pretty e
                    Dim3SL2 _ _ -> pretty e
                    Dim3SL3 _ _ -> pretty e
                    _ ->
                        error $
                        "show ThreeDSparse non-scalar " ++ show (Var dims name)
            Just (DVar dims name) ->
                case dims of
                    Dim3SL1 _ _ -> pretty e
                    Dim3SL2 _ _ -> pretty e
                    Dim3SL3 _ _ -> pretty e
                    _ ->
                        error $
                        "show ThreeDSparse non-scalar " ++ show (Var dims name)
            Just (Op dims op _inputs) ->
                case dims of
                    Dim3SL1 _ _ -> pretty e
                    Dim3SL2 _ _ -> pretty e
                    Dim3SL3 _ _ -> pretty e
                    _ ->
                        error $
                        "show ThreeDSparse non-scalar " ++ show (dims, op)
            Just (Const dims _) ->
                case dims of
                    Dim3SL1 _ _ -> pretty e
                    Dim3SL2 _ _ -> pretty e
                    Dim3SL3 _ _ -> pretty e
                    _ -> error $ "show ThreeDSparse non-scalar " ++ show (e)
            Just _ -> pretty e

instance Show ThreeDCSparse where
    show (ThreeDCSparse (SparseList3D _len _ _ _ _ _) e@(Expression node exprs)) =
        case I.lookup node exprs of
            Nothing ->
                "show ThreeDCSparse no node " ++
                (take 1000 $ show (node, exprs))
            Just (Var dims name) ->
                case dims of
                    Dim3SL1 _ _ -> pretty e
                    Dim3SL2 _ _ -> pretty e
                    Dim3SL3 _ _ -> pretty e
                    _ ->
                        error $
                        "show ThreeDCSparse non-scalar " ++ show (Var dims name)
            Just (DVar dims name) ->
                case dims of
                    Dim3SL1 _ _ -> pretty e
                    Dim3SL2 _ _ -> pretty e
                    Dim3SL3 _ _ -> pretty e
                    _ ->
                        error $
                        "show ThreeDCSparse non-scalar " ++ show (Var dims name)
            Just (Op dims op _inputs) ->
                case dims of
                    Dim3SL1 _ _ -> pretty e
                    Dim3SL2 _ _ -> pretty e
                    Dim3SL3 _ _ -> pretty e
                    _ ->
                        error $
                        "show ThreeDCSparse non-scalar " ++ show (dims, op)
            Just (Const dims _) ->
                case dims of
                    Dim3SL1 _ _ -> pretty e
                    Dim3SL2 _ _ -> pretty e
                    Dim3SL3 _ _ -> pretty e
                    _ -> error $ "show ThreeDCSparse non-scalar " ++ show (e)
            Just _ -> pretty e

pretty' (e, n) = pretty $ Expression n e

prettyAll (e, n) = unlines $ pa [] [n]
  where
    pa _ [] = []
    pa visited (next:tovisit) =
        (("----- " ++ show next ++ "   sized " ++ show (getDimE e next)) :
         ("      " ++ pretty' (e, next)) :
         case I.lookup next e of
             Just (Op _ _ args) ->
                 pa
                     (next : visited)
                     ((L.nub (tovisit ++ args)) L.\\ (next : visited))
             _ -> pa (next : visited) tovisit)

--  TODO ?? Show instance should do dynamic dim typechecking ***
{-

invFt takes scaling into account, but not ft.  Probably whatever we do should mean that ft(invft(x)) = x.  Leave the older version of the invFt which scales, but use the new one.
-}
instance Rectangular OneDC where
    ft (OneDC (Expression n exprs)) =
        OneDC $ addEdge' exprs (Op (getDimE exprs n) (FT True) [n])
    invFt (OneDC (Expression n exprs)) =
        OneDC $ addEdge' exprs (Op (getDimE exprs n) (FT False) [n])
    rowPFT (OneDC (Expression n exprs)) =
        OneDC $ addEdge' exprs (Op (getDimE exprs n) (PFT True Row) [n])
    columnPFT _e = error "partial ft not defined for OneDC"
    colPFT _ _e = error "partial ft not defined for OneDC"
    slicePFT _e = error "partial ft not defined for OneDC"
    invRowPFT (OneDC (Expression n exprs)) =
        OneDC $ addEdge' exprs (Op (getDimE exprs n) (PFT False Row) [n])
    invColumnPFT _e = error "partial ft not defined for OneDC"
    invSlicePFT _e = error "partial ft not defined for OneDC"
    transp swap = error $ "transpose " ++ show swap ++ " not defined for OneDC"

{--
  invFt (OneDC (Expression n exprs)) =
    let
      (e',n') = addEdge exprs $ Op (getDimE exprs n) (FT False) [n]
      scale = case getDimE exprs n of
                Dim1 d1 -> d1
                d -> error $ "invFT 1d got dim " ++ show d
      (e'',s) = addEdge e' $ Const Dim0 $ 1 / fromIntegral scale
    in
      OneDC $ addEdge' e'' $ Op (getDimE exprs n) (ScaleV) [s,n']
--}
{-

-}
instance Rectangular TwoDC where
    ft (TwoDC (Expression n exprs)) =
        TwoDC $ addEdge' exprs (Op (getDimE exprs n) (FT True) [n])
    invFt (TwoDC (Expression n exprs)) =
        let (e', n') = addEdge exprs $ Op (getDimE exprs n) (FT False) [n]
            scale =
                case getDimE exprs n of
                    Dim2 (d1, d2) -> d1 * d2
                    d -> error $ "invFT 2d got dim " ++ show d
            (e'', s) = addEdge e' $ Const Dim0 $ 1 / fromIntegral scale
         in TwoDC $ addEdge' e'' $ Op (getDimE exprs n) (ScaleV) [s, n']
    rowPFT (TwoDC (Expression n exprs)) =
        TwoDC $ addEdge' exprs (Op (getDimE exprs n) (PFT True Row) [n])
    colPFT dir (TwoDC (Expression n exprs)) =
        TwoDC $ addEdge' exprs (Op (getDimE exprs n) (PFT dir Column) [n])
    columnPFT (TwoDC (Expression n exprs)) =
        TwoDC $ addEdge' exprs (Op (getDimE exprs n) (PFT True Column) [n])
    slicePFT _e = error "slicePFT not defined for TwoDC"
    invRowPFT (TwoDC (Expression n exprs)) =
        TwoDC $ -- FIXME: do we do the 1/dims for for the partial??
        addEdge' exprs (Op (getDimE exprs n) (PFT False Row) [n])
    invColumnPFT (TwoDC (Expression n exprs)) =
        TwoDC $ addEdge' exprs (Op (getDimE exprs n) (PFT False Column) [n])
    invSlicePFT _e = error "invSlicePFT not defined for TwoDC"
    transp swap (TwoDC (Expression n exprs)) =
        TwoDC $
        addEdge' exprs (Op (trDims swap $ getDimE exprs n) (Transpose swap) [n])

{-

-}
instance Rectangular ThreeDC where
    ft (ThreeDC (Expression n exprs)) =
        ThreeDC $ addEdge' exprs (Op (getDimE exprs n) (FT True) [n])
    invFt (ThreeDC (Expression n exprs)) =
        let (e', n') = addEdge exprs $ Op (getDimE exprs n) (FT False) [n]
            scale =
                case getDimE exprs n of
                    Dim3 (d1, d2, d3) -> d1 * d2 * d3
                    d -> error $ "invFT 3d got dim " ++ show d
            (e'', s) = addEdge e' $ Const Dim0 $ 1 / fromIntegral scale
         in ThreeDC $ addEdge' e'' $ Op (getDimE exprs n) (ScaleV) [s, n']
    rowPFT (ThreeDC (Expression n exprs)) =
        ThreeDC $ addEdge' exprs (Op (getDimE exprs n) (PFT True Row) [n])
    colPFT dir (ThreeDC (Expression n exprs)) =
        ThreeDC $ addEdge' exprs (Op (getDimE exprs n) (PFT dir Column) [n])
    columnPFT (ThreeDC (Expression n exprs)) =
        ThreeDC $ addEdge' exprs (Op (getDimE exprs n) (PFT True Column) [n])
    slicePFT (ThreeDC (Expression n exprs)) =
        ThreeDC $ addEdge' exprs (Op (getDimE exprs n) (PFT True Slice) [n])
    invRowPFT (ThreeDC (Expression n exprs)) =
        ThreeDC $ -- FIXME: do we do the 1/dims for for the partial??
        addEdge' exprs (Op (getDimE exprs n) (PFT False Row) [n])
    invColumnPFT (ThreeDC (Expression n exprs)) =
        ThreeDC $ addEdge' exprs (Op (getDimE exprs n) (PFT False Column) [n])
    invSlicePFT (ThreeDC (Expression n exprs)) =
        ThreeDC $ addEdge' exprs (Op (getDimE exprs n) (PFT False Slice) [n])
    transp swap (ThreeDC (Expression n exprs)) =
        ThreeDC $
        addEdge' exprs (Op (trDims swap $ getDimE exprs n) (Transpose swap) [n])

{-

-}
instance SimilarSum [ThreeD] where
    decompose lst =
        let exprs = map unThreeD lst
            dims = map getDimE' exprs
            (Expression _ allExprs, nodes) = mergeL exprs
         in if 1 == (length $ L.nub dims)
                then (allExprs, nodes)
                else error $ "HI.decompose [ThreeD] " ++ show (map pretty exprs)

{-

-}
instance SimilarSum ThreeD where
    decompose (ThreeD (Expression n exprs)) = (exprs, [n])

{-

-}
instance SimilarSum [ThreeDC] where
    decompose lst =
        let exprs = map unThreeDC lst
            allC = L.all nodeIsComplex' exprs
            dims = map getDimE' exprs
            (Expression _ allExprs, nodes) = mergeL exprs
         in if 1 == (length $ L.nub dims) && allC
                then (allExprs, nodes)
                else error $
                     "HI.decompose [ThreeDC] " ++ show (map pretty exprs)

{-

-}
instance All1D OneD

instance All1D OneDC

instance All2D TwoD

instance All2D TwoDC

instance All3D ThreeD

instance All3D ThreeDC

{-

-}
instance (SimilarSum a, SimilarSum b) => SimilarSum (a, b) where
    decompose (a, b) =
        let (ea, as) = decompose a
            (eb, bs) = decompose b
            pas = map (ea, ) as
            pbs = map (eb, ) bs
            (allExprs, nodes) = mergeL' (pas ++ pbs)
         in if map (uncurry getDimE) (take 1 pas) ==
               map (uncurry getDimE) (take 1 pbs)
                then (allExprs, nodes)
                else error $
                     "HI.decompose (a,b) " ++
                     show (map pretty pas, map pretty pbs)

{-

-}
instance (All1D a, All1D b) => All1D (a, b)

instance (All2D a, All2D b) => All2D (a, b)

instance (All3D a, All3D b) => All3D (a, b)

instance (All4D a, All4D b) => All4D (a, b)

instance (All5D a, All5D b) => All5D (a, b)

instance (All6D a, All6D b) => All6D (a, b)

{-

-}
instance (SimilarSum a, All1D a) => Regularizable a [((Int), Double)] where
    huber dk vs = helpROneD RKHuber dk $ decompose vs
    tukey dk c vs = helpROneD (RKTukey c) dk $ decompose vs
    l2Bi dk vs = helpROneD RKL2 dk $ decompose vs
    l1Bi dk vs = helpROneD RKL1 dk $ decompose vs
    l1l2Bi dk vs = helpROneD RKL1mL2 dk $ decompose vs
    gmBi dk vs = helpROneD RKGM dk $ decompose vs

helpROneD :: RangeKernel -> [((Int), Double)] -> (Internal, [Node]) -> Scalar
helpROneD rk dk (exprs, nodes) =
    case map (getDimE exprs) nodes of
        (Dim1 _):_ ->
            Scalar $ addEdge' exprs (Op Dim0 (Reglzr (DW1d dk) rk) nodes)
        dims ->
            error $
            "HI." ++
            show rk ++
            " OneD applied to " ++ show (dims, map (pretty . (exprs, )) nodes)

{-

-}
instance (SimilarSum a, All2D a) => Regularizable a [((Int, Int), Double)] where
    huber dk vs = helpRTwoD RKHuber dk $ decompose vs
    tukey dk c vs = helpRTwoD (RKTukey c) dk $ decompose vs
    l2Bi dk vs = helpRTwoD RKL2 dk $ decompose vs
    l1Bi dk vs = helpRTwoD RKL1 dk $ decompose vs
    l1l2Bi dk vs = helpRTwoD RKL1mL2 dk $ decompose vs
    gmBi dk vs = helpRTwoD RKGM dk $ decompose vs

helpRTwoD ::
       RangeKernel -> [((Int, Int), Double)] -> (Internal, [Node]) -> Scalar
helpRTwoD rk dk (exprs, nodes) =
    case map (getDimE exprs) nodes of
        (Dim2 _):_ ->
            Scalar $ addEdge' exprs (Op Dim0 (Reglzr (DW2d dk) rk) nodes)
        dims ->
            error $
            "HI." ++
            show rk ++
            " TwoD applied to " ++ show (dims, map (pretty . (exprs, )) nodes)

{-

-}
instance (SimilarSum a, All3D a) =>
         Regularizable a [((Int, Int, Int), Double)] where
    huber dk vs = helpRThreeD RKHuber dk $ decompose vs
    tukey dk c vs = helpRThreeD (RKTukey c) dk $ decompose vs
    l2Bi dk vs = helpRThreeD RKL2 dk $ decompose vs
    l1Bi dk vs = helpRThreeD RKL1 dk $ decompose vs
    l1l2Bi dk vs = helpRThreeD RKL1mL2 dk $ decompose vs
    gmBi dk vs = helpRThreeD RKGM dk $ decompose vs

helpRThreeD ::
       RangeKernel
    -> [((Int, Int, Int), Double)]
    -> (Internal, [Node])
    -> Scalar
helpRThreeD rk dk (exprs, nodes) =
    case map (getDimE exprs) nodes of
        (Dim3 _):_ ->
            Scalar $ addEdge' exprs (Op Dim0 (Reglzr (DW3d dk) rk) nodes)
        dims ->
            error $
            "HI." ++
            show rk ++
            " ThreeD applied to " ++ show (dims, map (pretty . (exprs, )) nodes)
{-

-}
{-

-}
