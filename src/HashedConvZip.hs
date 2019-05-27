{-
(c) 2010 Christopher Kumar Anand

Experiment in common subexpressions without monads and better expression simplification.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module HashedConvZip where

import Data.ByteString ()
import HashedExpression
import HashedSimplify

--import qualified Data.IntMap as I
--import Debug.Trace
{-


-}
instance ConvZip OneD Scalar Int where
    conv bndry pairs (OneD (Expression n es)) =
        let Scalar expr = sum [c * relElem 0 bndry [idx] | (idx, c) <- pairs]
         in OneD $ addEdge' es $ mkSCZ (getDimE es n) expr [n]
    conv1Zip1 ker (OneD (Expression n es)) =
        let Scalar expr = ker (\bnd idx -> relElem 0 bnd [idx])
         in OneD $ addEdge' es $ mkSCZ (getDimE es n) expr [n]
    conv2Zip1 ker (arg1@(OneD e1), arg2@(OneD e2)) =
        let Scalar expr =
                ker
                    ( \bnd idx -> relElem 0 bnd [idx]
                    , \bnd idx -> relElem 1 bnd [idx])
            (Expression _ es, (n1, n2)) = merge e1 e2
         in if getDimE' e1 == getDimE' e2
                then OneD $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2]
                else error $ "conv2Zip1 OneD don't match " ++ show (arg1, arg2)
    conv3Zip1 ker (arg1@(OneD e1), arg2@(OneD e2), arg3@(OneD e3)) =
        let Scalar expr =
                ker
                    ( \bnd idx -> relElem 0 bnd [idx]
                    , \bnd idx -> relElem 1 bnd [idx]
                    , \bnd idx -> relElem 2 bnd [idx])
            (Expression _ es, [n1, n2, n3]) = mergeL [e1, e2, e3]
         in if getDimE' e1 == getDimE' e2 && getDimE' e1 == getDimE' e3
                then OneD $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3]
                else error $
                     "conv3Zip1 OneD don't match " ++ show (arg1, arg2, arg3)
    conv4Zip1 ker (arg1@(OneD e1), arg2@(OneD e2), arg3@(OneD e3), arg4@(OneD e4)) =
        let Scalar expr =
                ker
                    ( \bnd idx -> relElem 0 bnd [idx]
                    , \bnd idx -> relElem 1 bnd [idx]
                    , \bnd idx -> relElem 2 bnd [idx]
                    , \bnd idx -> relElem 3 bnd [idx])
            (Expression _ es, [n1, n2, n3, n4]) = mergeL [e1, e2, e3, e4]
         in if getDimE' e1 == getDimE' e2 &&
               getDimE' e1 == getDimE' e3 && getDimE' e1 == getDimE' e4
                then OneD $
                     addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3, n4]
                else error $
                     "conv4Zip1 OneD don't match " ++
                     show (arg1, arg2, arg3, arg4)
    czMap f (OneD (Expression n es)) =
        let Scalar expr = f (relElem 0 ZeroMargin [0])
         in OneD $ addEdge' es $ mkSCZ (getDimE es n) expr [n]
    czZip f (OneD e1) (OneD e2) =
        let Scalar expr =
                f (relElem 0 ZeroMargin [0]) (relElem 1 ZeroMargin [0])
            (Expression _ es, (n1, n2)) = merge e1 e2
         in if getDimE' e1 == getDimE' e2
                then OneD $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2]
                else error "czZip OneD don't match "
    czZip3 f arg1@(OneD e1) arg2@(OneD e2) arg3@(OneD e3) =
        let Scalar expr =
                f
                    (relElem 0 ZeroMargin [0])
                    (relElem 1 ZeroMargin [0])
                    (relElem 2 ZeroMargin [0])
            (Expression _ es, [n1, n2, n3]) = mergeL [e1, e2, e3]
         in if getDimE' e1 == getDimE' e2 && getDimE' e1 == getDimE' e3
                then OneD $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3]
                else error $
                     "czZip3 OneD don't match " ++ show (arg1, arg2, arg3)
    czZip4 f arg1@(OneD e1) arg2@(OneD e2) arg3@(OneD e3) arg4@(OneD e4) =
        let Scalar expr =
                f
                    (relElem 0 ZeroMargin [0])
                    (relElem 1 ZeroMargin [0])
                    (relElem 2 ZeroMargin [0])
                    (relElem 3 ZeroMargin [0])
            (Expression _ es, [n1, n2, n3, n4]) = mergeL [e1, e2, e3, e4]
         in if getDimE' e1 == getDimE' e2 &&
               getDimE' e1 == getDimE' e3 && getDimE' e1 == getDimE' e4
                then OneD $
                     addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3, n4]
                else error $
                     "conv4Zip1 OneD don't match " ++
                     show (arg1, arg2, arg3, arg4)

{-
%\begin{code}
instance ConvZip OneDC ScalarC Int where
  conv bndry pairs (OneDC (Expression n es)) = let
      ScalarC expr = sum [c * (relElem 0 bndry [idx]) | (idx,c) <- pairs]
    in OneDC $ addEdge' es $ mkSCZ (getDimE es n) expr [n]

  conv1Zip1 ker (OneDC (Expression n es)) = let
      ScalarC expr = ker (\ bnd idx -> relElem 0 bnd [idx])
    in OneDC $ addEdge' es $ mkSCZ (getDimE es n) expr [n]

  conv2Zip1 ker (arg1@(OneDC e1), arg2@(OneDC e2)) = let
      ScalarC expr = ker (\ bnd idx -> relElem 0 bnd [idx],\ bnd idx -> relElem 1 bnd [idx])
      (Expression _ es, (n1,n2)) = merge e1 e2
    in if getDimE' e1 == getDimE' e2
          then OneDC $ addEdge' es $ mkSCZ (getDimE' e1) op [n1,n2]
          else error $ "conv2Zip1 OneDC don't match " ++ show (arg1,arg2)

  conv3Zip1 ker (arg1@(OneDC e1), arg2@(OneDC e2), arg3@(OneDC e3)) = let
      ScalarC expr = ker (\ bnd idx -> relElem 0 bnd [idx],\ bnd idx -> relElem 1 bnd [idx],\ bnd idx -> relElem 2 bnd [idx])
      (Expression _ es, [n1,n2,n3]) = mergeL [e1,e2,e3]
    in if getDimE' e1 == getDimE' e2 && getDimE' e1 == getDimE' e3
          then OneDC $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1,n2,n3]
          else error $ "conv3Zip1 OneDC don't match " ++ show ((getDimE' e1),arg1,arg2,arg3)

  conv4Zip1 ker (arg1@(OneDC e1), arg2@(OneDC e2), arg3@(OneDC e3), arg4@(OneDC e4)) = let
      ScalarC expr = ker (\ bnd idx -> relElem 0 bnd [idx],\ bnd idx -> relElem 1 bnd [idx],\ bnd idx -> relElem 2 bnd [idx],\ bnd idx -> relElem 3 bnd [idx])
      (Expression _ es, [n1,n2,n3,n4]) = mergeL [e1,e2,e3,e4]
    in if getDimE' e1 == getDimE' e2 && getDimE' e1 == getDimE' e3 && getDimE' e1 == getDimE' e4
          then OneDC $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1,n2,n3,n4]
          else error $ "conv4Zip1 OneDC don't match " ++ show (arg1,arg2,arg3,arg4)

  czMap f (OneDC (Expression n es)) = let
      ScalarC expr = f (relElem 0 ZeroMargin [0])
    in OneDC $ addEdge' es $ mkSCZ (getDimE es n) expr [n]

  czZip f (OneDC e1) (OneDC e2) = let
      ScalarC expr = f (relElem 0 ZeroMargin [0]) (relElem 1 ZeroMargin [0])
      (Expression _ es, (n1,n2)) = merge e1 e2
    in if getDimE' e1 == getDimE' e2
          then  OneDC $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1,n2]
          else error $ "czZip OneDC don't match "

  czZip3 f arg1@(OneDC e1) arg2@(OneDC e2) arg3@(OneDC e3) = let
      ScalarC expr = f (relElem 0 ZeroMargin [0]) (relElem 1 ZeroMargin [0]) (relElem 2 ZeroMargin [0])
      (Expression _ es, [n1,n2,n3]) = mergeL [e1,e2,e3]
    in if getDimE' e1 == getDimE' e2 && getDimE' e1 == getDimE' e3
          then OneDC $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1,n2,n3]
          else error $ "czZip3 OneDC don't match " ++ show (arg1,arg2,arg3)

  czZip4 f arg1@(OneDC e1) arg2@(OneDC e2) arg3@(OneDC e3) arg4@(OneDC e4) = let
      ScalarC expr = f (relElem 0 ZeroMargin [0]) (relElem 1 ZeroMargin [0]) (relElem 2 ZeroMargin [0]) (relElem 3 ZeroMargin [0])
      (Expression _ es, [n1,n2,n3,n4]) = mergeL [e1,e2,e3,e4]
    in if getDimE' e1 == getDimE' e2 && getDimE' e1 == getDimE' e3 && getDimE' e1 == getDimE' e4
          then OneDC $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1,n2,n3,n4]
          else error $ "conv4Zip1 OneDC don't match " ++ show (arg1,arg2,arg3,arg4)
%\end{code}

-}
instance ConvZip TwoD Scalar (Int, Int) where
    conv bndry pairs (TwoD (Expression n es)) =
        let Scalar expr =
                sum [c * relElem 0 bndry [i, j] | ((i, j), c) <- pairs]
         in TwoD $ addEdge' es $ mkSCZ (getDimE es n) expr [n]
    conv1Zip1 ker (TwoD (Expression n es)) =
        let Scalar expr = ker (\bnd (i, j) -> relElem 0 bnd [i, j])
         in TwoD $ addEdge' es $ mkSCZ (getDimE es n) expr [n]
    conv2Zip1 ker (arg1@(TwoD e1), arg2@(TwoD e2)) =
        let Scalar expr =
                ker
                    ( \bnd (i, j) -> relElem 0 bnd [i, j]
                    , \bnd (i, j) -> relElem 1 bnd [i, j])
            (Expression _ es, (n1, n2)) = merge e1 e2
         in if getDimE' e1 == getDimE' e2
                then TwoD $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2]
                else error $ "conv2Zip1 TwoD don't match " ++ show (arg1, arg2)
    conv3Zip1 ker (arg1@(TwoD e1), arg2@(TwoD e2), arg3@(TwoD e3)) =
        let Scalar expr =
                ker
                    ( \bnd (i, j) -> relElem 0 bnd [i, j]
                    , \bnd (i, j) -> relElem 1 bnd [i, j]
                    , \bnd (i, j) -> relElem 2 bnd [i, j])
            (Expression _ es, [n1, n2, n3]) = mergeL [e1, e2, e3]
         in if getDimE' e1 == getDimE' e2 && getDimE' e1 == getDimE' e3
                then TwoD $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3]
                else error $
                     "conv3Zip1 TwoD don't match " ++ show (arg1, arg2, arg3)
    conv4Zip1 ker (arg1@(TwoD e1), arg2@(TwoD e2), arg3@(TwoD e3), arg4@(TwoD e4)) =
        let Scalar expr =
                ker
                    ( \bnd (i, j) -> relElem 0 bnd [i, j]
                    , \bnd (i, j) -> relElem 1 bnd [i, j]
                    , \bnd (i, j) -> relElem 2 bnd [i, j]
                    , \bnd (i, j) -> relElem 3 bnd [i, j])
            (Expression _ es, [n1, n2, n3, n4]) = mergeL [e1, e2, e3, e4]
         in if getDimE' e1 == getDimE' e2 &&
               getDimE' e1 == getDimE' e3 && getDimE' e1 == getDimE' e4
                then TwoD $
                     addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3, n4]
                else error $
                     "conv4Zip1 TwoD don't match " ++
                     show (arg1, arg2, arg3, arg4)
    czMap f (TwoD (Expression n es)) =
        let Scalar expr = f (relElem 0 ZeroMargin [0, 0])
         in TwoD $ addEdge' es $ mkSCZ (getDimE es n) expr [n]
    czZip f (TwoD e1) (TwoD e2) =
        let Scalar expr =
                f (relElem 0 ZeroMargin [0, 0]) (relElem 1 ZeroMargin [0, 0])
            (Expression _ es, (n1, n2)) = merge e1 e2
         in if getDimE' e1 == getDimE' e2
                then TwoD $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2]
                else error $
                     "czZip TwoDs don't match " ++ pretty e1 ++ pretty e2
    czZip3 f arg1@(TwoD e1) arg2@(TwoD e2) arg3@(TwoD e3) =
        let Scalar expr =
                f
                    (relElem 0 ZeroMargin [0, 0])
                    (relElem 1 ZeroMargin [0, 0])
                    (relElem 2 ZeroMargin [0, 0])
            (Expression _ es, [n1, n2, n3]) = mergeL [e1, e2, e3]
         in if getDimE' e1 == getDimE' e2 && getDimE' e1 == getDimE' e3
                then TwoD $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3]
                else error $
                     "czZip3 TwoD don't match " ++ show (arg1, arg2, arg3)
    czZip4 f arg1@(TwoD e1) arg2@(TwoD e2) arg3@(TwoD e3) arg4@(TwoD e4) =
        let Scalar expr =
                f
                    (relElem 0 ZeroMargin [0, 0])
                    (relElem 1 ZeroMargin [0, 0])
                    (relElem 2 ZeroMargin [0, 0])
                    (relElem 3 ZeroMargin [0, 0])
            (Expression _ es, [n1, n2, n3, n4]) = mergeL [e1, e2, e3, e4]
         in if getDimE' e1 == getDimE' e2 &&
               getDimE' e1 == getDimE' e3 && getDimE' e1 == getDimE' e4
                then TwoD $
                     addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3, n4]
                else error $
                     "conv4Zip1 TwoDs don't match " ++
                     show (arg1, arg2, arg3, arg4)

{-
-}
instance ConvZip TwoDC Scalar (Int, Int) where
    conv bndry pairs (TwoDC (Expression n es)) =
        let Scalar expr =
                sum [c * relElem 0 bndry [i, j] | ((i, j), c) <- pairs]
         in TwoDC $ addEdge' es $ mkSCZ (getDimE es n) expr [n]
    conv1Zip1 ker (TwoDC (Expression n es)) =
        let Scalar expr = ker (\bnd (i, j) -> relElem 0 bnd [i, j])
         in TwoDC $ addEdge' es $ mkSCZ (getDimE es n) expr [n]
    conv2Zip1 ker (arg1@(TwoDC e1), arg2@(TwoDC e2)) =
        let Scalar expr =
                ker
                    ( \bnd (i, j) -> relElem 0 bnd [i, j]
                    , \bnd (i, j) -> relElem 1 bnd [i, j])
            (Expression _ es, (n1, n2)) = merge e1 e2
         in if getDimE' e1 == getDimE' e2
                then TwoDC $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2]
                else error $ "conv2Zip1 TwoDC don't match " ++ show (arg1, arg2)
    conv3Zip1 ker (arg1@(TwoDC e1), arg2@(TwoDC e2), arg3@(TwoDC e3)) =
        let Scalar expr =
                ker
                    ( \bnd (i, j) -> relElem 0 bnd [i, j]
                    , \bnd (i, j) -> relElem 1 bnd [i, j]
                    , \bnd (i, j) -> relElem 2 bnd [i, j])
            (Expression _ es, [n1, n2, n3]) = mergeL [e1, e2, e3]
         in if getDimE' e1 == getDimE' e2 && getDimE' e1 == getDimE' e3
                then TwoDC $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3]
                else error $
                     "conv3Zip1 TwoDC don't match " ++ show (arg1, arg2, arg3)
    conv4Zip1 ker (arg1@(TwoDC e1), arg2@(TwoDC e2), arg3@(TwoDC e3), arg4@(TwoDC e4)) =
        let Scalar expr =
                ker
                    ( \bnd (i, j) -> relElem 0 bnd [i, j]
                    , \bnd (i, j) -> relElem 1 bnd [i, j]
                    , \bnd (i, j) -> relElem 2 bnd [i, j]
                    , \bnd (i, j) -> relElem 3 bnd [i, j])
            (Expression _ es, [n1, n2, n3, n4]) = mergeL [e1, e2, e3, e4]
         in if getDimE' e1 == getDimE' e2 &&
               getDimE' e1 == getDimE' e3 && getDimE' e1 == getDimE' e4
                then TwoDC $
                     addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3, n4]
                else error $
                     "conv4Zip1 TwoDC don't match " ++
                     show (arg1, arg2, arg3, arg4)
    czMap f (TwoDC (Expression n es)) =
        let Scalar expr = f (relElem 0 ZeroMargin [0, 0])
         in TwoDC $ addEdge' es $ mkSCZ (getDimE es n) expr [n]
    czZip f (TwoDC e1) (TwoDC e2) =
        let Scalar expr =
                f (relElem 0 ZeroMargin [0, 0]) (relElem 1 ZeroMargin [0, 0])
            (Expression _ es, (n1, n2)) = merge e1 e2
         in if getDimE' e1 == getDimE' e2
                then TwoDC $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2]
                else error "czZip TwoDC (dima,dimb)s don't match "
    czZip3 f arg1@(TwoDC e1) arg2@(TwoDC e2) arg3@(TwoDC e3) =
        let Scalar expr =
                f
                    (relElem 0 ZeroMargin [0, 0])
                    (relElem 1 ZeroMargin [0, 0])
                    (relElem 2 ZeroMargin [0, 0])
            (Expression _ es, [n1, n2, n3]) = mergeL [e1, e2, e3]
         in if getDimE' e1 == getDimE' e2 && getDimE' e1 == getDimE' e3
                then TwoDC $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3]
                else error $
                     "czZip3 TwoDC (dima,dimb)s don't match " ++
                     show (arg1, arg2, arg3)
    czZip4 f arg1@(TwoDC e1) arg2@(TwoDC e2) arg3@(TwoDC e3) arg4@(TwoDC e4) =
        let Scalar expr =
                f
                    (relElem 0 ZeroMargin [0, 0])
                    (relElem 1 ZeroMargin [0, 0])
                    (relElem 2 ZeroMargin [0, 0])
                    (relElem 3 ZeroMargin [0, 0])
            (Expression _ es, [n1, n2, n3, n4]) = mergeL [e1, e2, e3, e4]
         in if getDimE' e1 == getDimE' e2 &&
               getDimE' e1 == getDimE' e3 && getDimE' e1 == getDimE' e4
                then TwoDC $
                     addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3, n4]
                else error $
                     "conv4Zip1 TwoDC (dima,dimb)s don't match " ++
                     show (arg1, arg2, arg3, arg4)

{-
-}
instance ConvZip ThreeD Scalar (Int, Int, Int) where
    conv bndry pairs (ThreeD (Expression n es)) =
        let Scalar expr =
                sum [c * relElem 0 bndry [i, j, k] | ((i, j, k), c) <- pairs]
         in ThreeD $ addEdge' es $ mkSCZ (getDimE es n) expr [n]
    conv1Zip1 ker (ThreeD (Expression n es)) =
        let Scalar expr = ker (\bnd (i, j, k) -> relElem 0 bnd [i, j, k])
         in ThreeD $ addEdge' es $ mkSCZ (getDimE es n) expr [n]
    conv2Zip1 ker (arg1@(ThreeD e1), arg2@(ThreeD e2)) =
        let Scalar expr =
                ker
                    ( \bnd (i, j, k) -> relElem 0 bnd [i, j, k]
                    , \bnd (i, j, k) -> relElem 1 bnd [i, j, k])
            (Expression _ es, (n1, n2)) = merge e1 e2
         in if getDimE' e1 == getDimE' e2
                then ThreeD $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2]
                else error $
                     "conv2Zip1 ThreeD don't match " ++ show (arg1, arg2)
    conv3Zip1 ker (arg1@(ThreeD e1), arg2@(ThreeD e2), arg3@(ThreeD e3)) =
        let Scalar expr =
                ker
                    ( \bnd (i, j, k) -> relElem 0 bnd [i, j, k]
                    , \bnd (i, j, k) -> relElem 1 bnd [i, j, k]
                    , \bnd (i, j, k) -> relElem 2 bnd [i, j, k])
            (Expression _ es, [n1, n2, n3]) = mergeL [e1, e2, e3]
         in if getDimE' e1 == getDimE' e2 && getDimE' e1 == getDimE' e3
                then ThreeD $
                     addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3]
                else error $
                     "conv3Zip1 ThreeD don't match " ++ show (arg1, arg2, arg3)
    conv4Zip1 ker (arg1@(ThreeD e1), arg2@(ThreeD e2), arg3@(ThreeD e3), arg4@(ThreeD e4)) =
        let Scalar expr =
                ker
                    ( \bnd (i, j, k) -> relElem 0 bnd [i, j, k]
                    , \bnd (i, j, k) -> relElem 1 bnd [i, j, k]
                    , \bnd (i, j, k) -> relElem 2 bnd [i, j, k]
                    , \bnd (i, j, k) -> relElem 3 bnd [i, j, k])
            (Expression _ es, [n1, n2, n3, n4]) = mergeL [e1, e2, e3, e4]
         in if getDimE' e1 == getDimE' e2 &&
               getDimE' e1 == getDimE' e3 && getDimE' e1 == getDimE' e4
                then ThreeD $
                     addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3, n4]
                else error $
                     "conv4Zip1 ThreeD don't match " ++
                     show (arg1, arg2, arg3, arg4)
    czMap f (ThreeD (Expression n es)) =
        let Scalar expr = f (relElem 0 ZeroMargin [0, 0, 0])
         in ThreeD $ addEdge' es $ mkSCZ (getDimE es n) expr [n]
    czZip f arg1@(ThreeD e1) arg2@(ThreeD e2) =
        let Scalar expr =
                f
                    (relElem 0 ZeroMargin [0, 0, 0])
                    (relElem 1 ZeroMargin [0, 0, 0])
            (Expression _ es, (n1, n2)) = merge e1 e2
         in if getDimE' e1 == getDimE' e2
                then ThreeD $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2]
                else error $ "czZip ThreeDs don't match " ++ show (arg1, arg2)
    czZip3 f arg1@(ThreeD e1) arg2@(ThreeD e2) arg3@(ThreeD e3) =
        let Scalar expr =
                f
                    (relElem 0 ZeroMargin [0, 0, 0])
                    (relElem 1 ZeroMargin [0, 0, 0])
                    (relElem 2 ZeroMargin [0, 0, 0])
            (Expression _ es, [n1, n2, n3]) = mergeL [e1, e2, e3]
         in if getDimE' e1 == getDimE' e2 && getDimE' e1 == getDimE' e3
                then ThreeD $
                     addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3]
                else error $
                     "czZip3 ThreeDs don't match " ++ show (arg1, arg2, arg3)
    czZip4 f arg1@(ThreeD e1) arg2@(ThreeD e2) arg3@(ThreeD e3) arg4@(ThreeD e4) =
        let Scalar expr =
                f
                    (relElem 0 ZeroMargin [0, 0, 0])
                    (relElem 1 ZeroMargin [0, 0, 0])
                    (relElem 2 ZeroMargin [0, 0, 0])
                    (relElem 3 ZeroMargin [0, 0, 0])
            (Expression _ es, [n1, n2, n3, n4]) = mergeL [e1, e2, e3, e4]
         in if getDimE' e1 == getDimE' e2 &&
               getDimE' e1 == getDimE' e3 && getDimE' e1 == getDimE' e4
                then ThreeD $
                     addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3, n4]
                else error $
                     "conv4Zip1 ThreeDs don't match " ++
                     show (arg1, arg2, arg3, arg4)

{-
-}
instance ConvZip ThreeDC Scalar (Int, Int, Int) where
    conv bndry pairs (ThreeDC (Expression n es)) =
        let Scalar expr =
                sum [c * relElem 0 bndry [i, j, k] | ((i, j, k), c) <- pairs]
         in ThreeDC $ addEdge' es $ mkSCZ (getDimE es n) expr [n]
    conv1Zip1 ker (ThreeDC (Expression n es)) =
        let Scalar expr = ker (\bnd (i, j, k) -> relElem 0 bnd [i, j, k])
         in ThreeDC $ addEdge' es $ mkSCZ (getDimE es n) expr [n]
    conv2Zip1 ker (arg1@(ThreeDC e1), arg2@(ThreeDC e2)) =
        let Scalar expr =
                ker
                    ( \bnd (i, j, k) -> relElem 0 bnd [i, j, k]
                    , \bnd (i, j, k) -> relElem 1 bnd [i, j, k])
            (Expression _ es, (n1, n2)) = merge e1 e2
         in if getDimE' e1 == getDimE' e2
                then ThreeDC $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2]
                else error $
                     "conv2Zip1 ThreeDC don't match " ++ show (arg1, arg2)
    conv3Zip1 ker (arg1@(ThreeDC e1), arg2@(ThreeDC e2), arg3@(ThreeDC e3)) =
        let Scalar expr =
                ker
                    ( \bnd (i, j, k) -> relElem 0 bnd [i, j, k]
                    , \bnd (i, j, k) -> relElem 1 bnd [i, j, k]
                    , \bnd (i, j, k) -> relElem 2 bnd [i, j, k])
            (Expression _ es, [n1, n2, n3]) = mergeL [e1, e2, e3]
         in if getDimE' e1 == getDimE' e2 && getDimE' e1 == getDimE' e3
                then ThreeDC $
                     addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3]
                else error $
                     "conv3Zip1 ThreeDC don't match " ++ show (arg1, arg2, arg3)
    conv4Zip1 ker (arg1@(ThreeDC e1), arg2@(ThreeDC e2), arg3@(ThreeDC e3), arg4@(ThreeDC e4)) =
        let Scalar expr =
                ker
                    ( \bnd (i, j, k) -> relElem 0 bnd [i, j, k]
                    , \bnd (i, j, k) -> relElem 1 bnd [i, j, k]
                    , \bnd (i, j, k) -> relElem 2 bnd [i, j, k]
                    , \bnd (i, j, k) -> relElem 3 bnd [i, j, k])
            (Expression _ es, [n1, n2, n3, n4]) = mergeL [e1, e2, e3, e4]
         in if getDimE' e1 == getDimE' e2 &&
               getDimE' e1 == getDimE' e3 && getDimE' e1 == getDimE' e4
                then ThreeDC $
                     addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3, n4]
                else error $
                     "conv4Zip1 ThreeDC don't match " ++
                     show (arg1, arg2, arg3, arg4)
    czMap f (ThreeDC (Expression n es)) =
        let Scalar expr = f (relElem 0 ZeroMargin [0, 0, 0])
         in ThreeDC $ addEdge' es $ mkSCZ (getDimE es n) expr [n]
    czZip f arg1@(ThreeDC e1) arg2@(ThreeDC e2) =
        let Scalar expr =
                f
                    (relElem 0 ZeroMargin [0, 0, 0])
                    (relElem 1 ZeroMargin [0, 0, 0])
            (Expression _ es, (n1, n2)) = merge e1 e2
         in if getDimE' e1 == getDimE' e2
                then ThreeDC $ addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2]
                else error $
                     "czZip ThreeDC dims don't match " ++ show (arg1, arg2)
    czZip3 f arg1@(ThreeDC e1) arg2@(ThreeDC e2) arg3@(ThreeDC e3) =
        let Scalar expr =
                f
                    (relElem 0 ZeroMargin [0, 0, 0])
                    (relElem 1 ZeroMargin [0, 0, 0])
                    (relElem 2 ZeroMargin [0, 0, 0])
            (Expression _ es, [n1, n2, n3]) = mergeL [e1, e2, e3]
         in if getDimE' e1 == getDimE' e2 && getDimE' e1 == getDimE' e3
                then ThreeDC $
                     addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3]
                else error $
                     "czZip3 ThreeDC dims don't match " ++
                     show (arg1, arg2, arg3)
    czZip4 f arg1@(ThreeDC e1) arg2@(ThreeDC e2) arg3@(ThreeDC e3) arg4@(ThreeDC e4) =
        let Scalar expr =
                f
                    (relElem 0 ZeroMargin [0, 0, 0])
                    (relElem 1 ZeroMargin [0, 0, 0])
                    (relElem 2 ZeroMargin [0, 0, 0])
                    (relElem 3 ZeroMargin [0, 0, 0])
            (Expression _ es, [n1, n2, n3, n4]) = mergeL [e1, e2, e3, e4]
         in if getDimE' e1 == getDimE' e2 &&
               getDimE' e1 == getDimE' e3 && getDimE' e1 == getDimE' e4
                then ThreeDC $
                     addEdge' es $ mkSCZ (getDimE' e1) expr [n1, n2, n3, n4]
                else error $
                     "conv4Zip1 ThreeDC dims don't match " ++
                     show (arg1, arg2, arg3, arg4)
{-

-}
{-

-}
