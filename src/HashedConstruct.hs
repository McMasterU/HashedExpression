{-
In addition to the somewhat type-safe wrapped expressions with Num and other
instances,
we have an unsafe instance which lazily constructs an constructor for
an expression.
This will make it easier to read the new subexpression being written
since the expression map doesn't have to be threaded through line by line,
and |Op|s don't appear explicity,
but the result still has to be applied to the right expression map,
or hard-to-pointpoint errors will occur.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HashedConstruct where

import HashedExpression

--import HashedInstances ()
import Data.IntMap ()
import qualified Data.IntMap as I
import qualified Data.List as L
import Debug.Trace

{-

FIXME:  with a different type
data ConstructNew = ConstructZero
                  | ConstructOne
                  | ConstructEdge Internal -> (Internal,Node)
we would be able to propagate zeros and ones in type-generic form

The type of subexpressions will be
-}
type Construct = Internal -> (Internal, Node)

comp :: ((Internal, Node) -> (Internal, Node)) -> Construct -> Construct
comp fun c e = fun $ c e

{-

Concatenate constructors of lists:
-}
concatC :: [Internal -> (Internal, [Node])] -> Internal -> (Internal, [Node])
concatC (c1:cs) e =
    let (e1, ns1) = c1 e
        (e2, ns2) = concatC cs e1
     in (e2, ns1 ++ ns2)
concatC [] e = (e, [])

{-

Map this over nodes found in the existing graph.  Used by HashedMatch.
-}
sourceNode n e =
    case I.lookup n e of
        Just _ -> (e, n)
        Nothing -> error $ "sourceNode " ++ show n ++ " not found in " ++ show e

{-

-}
instance Show Construct where
    show _ = error "can't show Constructor because we don't know its context"

instance Eq Construct where
    _a == _b =
        error "can't compare Constructor because we don't know its context"

{-

-}
instance Num Construct where
    negate x e =
        let (e', x') = x e
         in addEdge e' $ Op (getDimE e' x') Neg [x']
    (+) x y e =
        let (e', x') = x e --FIXME why not sumE [x,y]
            (e'', y') = y e'
         in addEdge e'' $ Op (getDimE e'' y') Sum [x', y']
    (*) x y e =
        let (e', x') = x e
            (e'', y') = y e'
         in addEdge e'' $ Op (getDimE e'' y') Prod [x', y']
    fromInteger i e = addEdge e $ Const Dim0 $ fromIntegral i
    abs x e =
        let (e', x') = x e
         in addEdge e' $ Op Dim0 Abs [x']
    signum x e =
        let (e', x') = x e
         in addEdge e' $ Op Dim0 Signum [x']

{-

-}
instance Fractional Construct where
    x / y =
        \e ->
            let (e', x') = x e
                (e'', y') = y e'
             in if Dim0 == getDimE e'' y'
                    then addEdge e'' $ Op Dim0 Div [x', y']
                    else error $ "Construct.div bad dims " ++ show (x', y', e)
    fromRational i e = addEdge e $ Const Dim0 $ fromRational i

{-

-}
fun1 op x e =
    let (e', x') = x e
     in if Dim0 == getDimE e' x'
            then addEdge e' $ Op Dim0 Sqrt [x']
            else error $ "Construct." ++ show op ++ " bad dims " ++ show (x', e)

instance Floating Construct where
    sqrt = fun1 Sqrt
    pi e = addEdge e $ Const Dim0 pi
    exp = fun1 Exp
    cos = fun1 Cos
    sin = fun1 Sin
    tan = fun1 Tan
    log = fun1 Log
    asin = fun1 Asin
    acos = fun1 Acos
    atan = fun1 Atan
    sinh = fun1 Sinh
    cosh = fun1 Cosh
    tanh = fun1 Tanh
    atanh = fun1 Atanh
    acosh = fun1 Acosh
    asinh = fun1 Asinh

getDimC x exprs = getDimE exprs (snd $ x exprs)

scaleX d x e =
    let (e1, dN) = addEdge e $ Const Dim0 d
     in genProd e1 [dN, snd $ x e]

mkConst dims d e = addEdge e $ Const dims d

{-

We can avoid a lot of simplifying later by using |sumE| instead of |sum|,
and |prodE| instead of |prod|.
-}
sumE, prodE :: [Construct] -> Construct
sumE [] = error "HashedConstruct: Can't sumE empty list."
sumE xs =
    \e ->
        let (e', xs'@(x':_)) = L.mapAccumR (\e' x -> x e') e xs
         in case xs' of
                [x] -> (e', x)
                _ -> addEdge e' $ Op (getDimE e' x') Sum $ nodeSort' e' xs'

prodE [] = error "HashedConstruct: Can't prodE empty list."
prodE xs =
    \e ->
        let (e', xs'@(x':_)) = L.mapAccumR (\e' x -> x e') e xs
         in case xs' of
                [x] -> (e', x)
                _ -> addEdge e' $ Op (getDimE e' x') Prod $ nodeSort' e' xs'

sumO, prodO :: (Internal -> (Internal, [Node])) -> Construct
sumO f e =
    let (e', ns@(n1:_)) = f e
     in case ns of
            [x] -> (e', x)
            _ -> addEdge e' $ Op (getDimE e' n1) Sum $ nodeSort' e' ns

prodO f e =
    let (e', ns@(n1:_)) = f e
     in case ns of
            [x] -> (e', x)
            _ -> addEdge e' $ Op (getDimE e' n1) Prod $ nodeSort' e' ns

sumN, prodN :: (Internal, [Node]) -> (Internal, Node)
sumN (e, ns@(n1:_)) =
    case ns of
        [x] -> (e, x)
        _ -> addEdge e $ Op (getDimE e n1) Sum $ nodeSort' e ns
sumN (e, []) = error $ "sumN [] " ++ show e

-- CKA: if we removed the dims from Zeros, we could determine the dims from context, and not return an error here
prodN (e, ns@(n1:_)) =
    case ns of
        [x] -> (e, x)
        _ -> addEdge e $ Op (getDimE e n1) Prod $ nodeSort' e ns
prodN (e, []) = addEdge e $ Const Dim0 1

{-

-}
instance Complex Construct Construct where
    x +: y =
        \e ->
            let (e', x') = x e
                (e'', y') = y e'
             in addEdge e'' $ Op (getDimE e' x') RealImag [x', y']
    iRe x e =
        let (e', x') = x e
            (e'', zero) =
                case getDimE e' x' of
                    Dim0 -> 0 e'
                    dims -> addEdge e' $ Const dims 0
         in addEdge e'' $ Op (getDimE e' x') RealImag [x', zero]
    iIm x e =
        let (e', x') = x e
            (e'', zero) =
                case getDimE e' x' of
                    Dim0 -> 0 e'
                    dims -> addEdge e' $ Const dims 0
         in addEdge e'' $ Op (getDimE e' x') RealImag [zero, x']
    xRe z e =
        let (e', z') = z e
         in addEdge e' $ Op (getDimE e' z') RealPart [z']
    xIm z e =
        let (e', z') = z e
         in addEdge e' $ Op (getDimE e' z') ImagPart [z']

reImC (x, y) = x +: y

xReImC z = (xRe z, xIm z)

mapP f (x, y) = (f x, f y)

mapCplx f = reImC . mapP f . xReImC

{-

-}
instance RealVectorSpace Construct Construct where
    scale s v e =
        let (e', s') = s e
            (e'', v') = v e'
         in addEdge e'' $ Op (getDimE e'' v') ScaleV [s', v']
    subMask m v e =
        let (e', m') = m e
            (e'', v') = v e'
         in addEdge e'' $ Op (getDimE e'' v') SubMask [m', v']
    negMask m v e =
        let (e', m') = m e
            (e'', v') = v e'
         in addEdge e'' $ Op (getDimE e'' v') NegMask [m', v']
    dot s v e =
        let (e', s') = s e
            (e'', v') = v e'
         in addEdge e'' $ Op Dim0 Dot [s', v']
    mapR _fun _n = error "mapR not implemented for unsafe interface"
     -- CKA we can't use a function :: Construct -> Construct to make the mapping function
     {- \ e -> addEdge e $ Op (getDimE e n) (MapND mapOut $ C.pack "anonymous") [n]
      where
        Scalar mapOut = fun $ var "anonymous"
     -}
    projSS ss v e =
        let (e', v') = v e
            inDim@(Dim2 (d1, d2)) = getDimE e' v'
            outDims =
                case ss of
                    SSCrop _bounds dims ->
                        if dims == [d1, d2]
                            then projDim inDim ss
                            else error $
                                 "HC.projSS adding " ++
                                 show (d1, d2, ss) ++ "\n" ++ pretty (e', v')
                    _ ->
                        trace ("projSS ok " ++ show (d1, d2, ss)) $
                        projDim inDim ss
         in addEdge e' $ Op outDims (Project ss) [v']
                       --addEdge e' $ Op (projDim (getDimE e' v') ss) (Project ss) [v']
    injectSS ss v e =
        let (e', v') = v e
         in addEdge e' $ Op (injectDim (getDimE e' v') ss) (Inject ss) [v']

{-

-}
instance ComplexVectorSpace Construct Construct Construct Construct where
    scaleC s v e =
        let (e', s') = s e
            (e'', v') = v e'
         in addEdge e'' $ Op (getDimE e'' v') ScaleV [s', v']
    scaleR s v e =
        let (e', s') = s e
            (e'', v') = v e'
         in addEdge e'' $ Op (getDimE e'' v') ScaleV [s', v']
    mapC _ = error "mapC not implemented for unsafe interface"
    realPartV = xRe
    imagPartV = xIm
    magV _ = error "magV not implemented"

instance Rectangular Construct where
    ft x e =
        let (e', x') = x e
         in addEdge e' $ Op (getDimE e' x') (FT True) [x']
    invFt x e =
        let (e', x') = x e
         in addEdge e' $ Op (getDimE e' x') (FT False) [x']
    rowPFT x e =
        let (e', x') = x e
         in addEdge e' $ Op (getDimE e' x') (PFT True Row) [x']
    colPFT dir x e =
        let (e', x') = x e
         in addEdge e' $ Op (getDimE e' x') (PFT dir Column) [x']
    columnPFT x e =
        let (e', x') = x e
         in addEdge e' $ Op (getDimE e' x') (PFT True Column) [x']
    slicePFT x e =
        let (e', x') = x e
         in addEdge e' $ Op (getDimE e' x') (PFT True Slice) [x']
    invRowPFT x e =
        let (e', x') = x e
         in addEdge e' $ Op (getDimE e' x') (PFT False Row) [x']
    invColumnPFT x e =
        let (e', x') = x e
         in addEdge e' $ Op (getDimE e' x') (PFT False Column) [x']
    invSlicePFT x e =
        let (e', x') = x e
         in addEdge e' $ Op (getDimE e' x') (PFT False Slice) [x']
    transp swap x e =
        let (e', x') = x e
         in addEdge e' $ Op (trDims swap $ getDimE e' x') (Transpose swap) [x']
{-

This only works for one dimension at a time
%\begin{code}
instance ConvZip Construct Construct (Int,Int) where
  conv bndry pairs x = \ e ->
    let
      (es,n) = x e
      Scalar expr = sum [c * (relElem 0 bndry [i,j]) | ((i,j),c) <- pairs]
      dims = getDimE n es
    in addEdge es $ mkSCZ dims expr [n]

  conv1Zip1 ker x = \ e ->
    let  (es,n) = x e
         Scalar expr = ker (\ bnd (i,j) -> relElem 0 bnd [i,j])
         dims = getDimE n es
    in addEdge' es $ mkSCZ dims expr [n]

  conv2Zip1 ker (n1',n2') = \ e ->
    let
      (e1,n1) = n1' e
      (e2,n2) = n2' e1
      Scalar expr = ker (\ bnd (i,j) -> relElem 0 bnd [i,j],\ bnd (i,j) -> relElem 1 bnd [i,j],\ bnd (i,j) -> relElem 2 bnd [i,j],\ bnd (i,j) -> relElem 3 bnd [i,j])
      dims = getDimE n1 e2
    in addEdge e2 $ mkSCZ dims expr [n1,n2]

  conv3Zip1 ker (n1',n2',n3') = \ e ->
    let
      (e1,n1) = n1' e
      (e2,n2) = n2' e
      (e3,n3) = n3' e
      Scalar expr = ker (\ bnd (i,j) -> relElem 0 bnd [i,j],\ bnd (i,j) -> relElem 1 bnd [i,j],\ bnd (i,j) -> relElem 2 bnd [i,j],\ bnd (i,j) -> relElem 3 bnd [i,j])
      dims = getDimE n1 e3
    in addEdge e3 $ mkSCZ dims expr [n1,n2,n3]

  conv4Zip1 ker (n1',n2',n3',n4') = \ e ->
    let
      (e1,n1) = n1' e
      (e2,n2) = n2' e
      (e3,n3) = n3' e
      (e4,n4) = n4' e
      Scalar expr = ker (\ bnd (i,j) -> relElem 0 bnd [i,j],\ bnd (i,j) -> relElem 1 bnd [i,j],\ bnd (i,j) -> relElem 2 bnd [i,j],\ bnd (i,j) -> relElem 3 bnd [i,j])
      dims = getDimE n1 e4
    in addEdge e4 $ mkSCZ dims expr [n1,n2,n3,n4]
%\end{code}

-}
{-

-}
