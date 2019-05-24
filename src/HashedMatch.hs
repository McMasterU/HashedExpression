{-
(c) 2010 Christopher Kumar Anand, Jessica LM Pavlin

Helper functions/instances to make pattern gaurds involving Expressions easier to read.

-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module HashedMatch where

import HashedConstruct (Construct, sourceNode)
import HashedExpression

import Control.Monad (liftM)
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap as I
import Data.Maybe
import qualified Data.Set as Set

--import HashedInstances ()
import Prelude (Bool(..), Double, ($), (==))
import Prelude ((++), error, show)
import qualified Prelude as P

{-

Inverse of |sourceNode|
-}
{-

Predicates
-}
isScalar :: Internal -> Node -> Bool
isScalar e n
    | Dim0 <- getDimE e n = True
isScalar _ _ = False

{-


Each helper function looks for a basic operation or pattern of operations,
and if it finds it, returns Just (nodes in the pattern),
otherwise it returns |Nothing|.
-}
var :: Internal -> Node -> Maybe (C.ByteString, Dims)
var e n =
    case I.lookup n e of
        Just (Var dims name) -> Just (name, dims)
        _ -> Nothing

{-

-}
dot :: Internal -> Node -> Maybe (Construct, Construct)
dot e n =
    case I.lookup n e of
        Just (Op _dims Dot [n1, n2]) -> Just (sourceNode n1, sourceNode n2)
        _ -> Nothing

{-

-}
plus :: Internal -> Node -> Maybe (Construct, Construct)
plus e n =
    case I.lookup n e of
        Just (Op _dims Sum [n1, n2]) -> Just (sourceNode n1, sourceNode n2)
        _ -> Nothing

{-

-}
subMask :: Internal -> Node -> Maybe (Construct, Construct)
subMask e n =
    case I.lookup n e of
        Just (Op _dims SubMask [n1, n2]) -> Just (sourceNode n1, sourceNode n2)
        _ -> Nothing

{-

-}
negMask :: Internal -> Node -> Maybe (Construct, Construct)
negMask e n =
    case I.lookup n e of
        Just (Op _dims NegMask [n1, n2]) -> Just (sourceNode n1, sourceNode n2)
        _ -> Nothing

{-

-}
negate :: Internal -> Node -> Maybe Construct
negate e n =
    case I.lookup n e of
        Just (Op _dims Neg [node]) -> Just $ sourceNode node
        _ -> Nothing

{-

-}
sum :: Internal -> Node -> Maybe [Construct]
sum e n =
    case I.lookup n e of
        Just (Op _dims Sum nodes) -> Just $ P.map sourceNode nodes
        _ -> Nothing

{-

-}
times :: Internal -> Node -> Maybe (Construct, Construct)
times e n =
    case I.lookup n e of
        Just (Op _dims Prod [n1, n2]) -> Just (sourceNode n1, sourceNode n2)
        _ -> Nothing

{-

-}
div :: Internal -> Node -> Maybe (Construct, Construct)
div e n =
    case I.lookup n e of
        Just (Op _dims Div [n1, n2]) -> Just (sourceNode n1, sourceNode n2)
        _ -> Nothing

{-

-}
prod :: Internal -> Node -> Maybe [Construct]
prod e n =
    case I.lookup n e of
        Just (Op _dims Prod nodes) -> Just $ P.map sourceNode nodes
        _ -> Nothing

{-

-}
scz :: Internal -> Node -> Maybe (Expression, [Construct])
scz e n =
    case I.lookup n e of
        Just (Op _dims (SCZ sczE) nodes) ->
            Just $ (sczE, P.map sourceNode nodes)
        _ -> Nothing

{-

-}
pftC :: Internal -> Node -> Maybe (Construct -> Construct)
pftC e n =
    case I.lookup n e of
        Just (Op dims op@(PFT _fwdInv _dir) [_node]) ->
            Just
                (\sn ->
                     (\e0 ->
                          let (e1, n1) = sn e0
                           in addEdge e1 (Op dims op [n1])))
        _ -> Nothing

{-

-}
pft :: Internal -> Node -> Maybe ((Bool, Dir), Construct)
pft e n =
    case I.lookup n e of
        Just (Op _dims (PFT fwdInv dir) [node]) ->
            Just ((fwdInv, dir), sourceNode node)
        _ -> Nothing

{-

-}
ft :: Internal -> Node -> Maybe (Bool, Construct)
ft e n =
    case I.lookup n e of
        Just (Op _dims (FT fwdInv) [node]) -> Just $ (fwdInv, sourceNode node)
        _ -> Nothing

{-

-}
fwdFt :: Internal -> Node -> Maybe Construct
fwdFt e n =
    case I.lookup n e of
        Just (Op _dims (FT True) [node]) -> Just $ sourceNode node
        _ -> Nothing

{-

-}
invFt :: Internal -> Node -> Maybe Construct
invFt e n =
    case I.lookup n e of
        Just (Op _dims (FT False) [node]) -> Just $ sourceNode node
        _ -> Nothing

{-

-}
fwdCFt :: Internal -> Node -> Maybe Construct
fwdCFt e n =
    case I.lookup n e of
        Just (Op _dims (PFT True Column) [node]) -> Just $ sourceNode node
        _ -> Nothing

{-

-}
invCFt :: Internal -> Node -> Maybe Construct
invCFt e n =
    case I.lookup n e of
        Just (Op _dims (PFT False Column) [node]) -> Just $ sourceNode node
        _ -> Nothing

{-

-}
reIm :: Internal -> Node -> Maybe (Construct, Construct)
reIm e n =
    case I.lookup n e of
        Just (Op _dims RealImag [node1, node2]) ->
            Just (sourceNode node1, sourceNode node2)
        _ -> Nothing

{-

-}
xRe :: Internal -> Node -> Maybe Construct
xRe e n =
    case I.lookup n e of
        Just (Op _dims RealPart [node]) -> Just $ sourceNode node
        _ -> Nothing

{-

-}
transpose :: Internal -> Node -> Maybe (Swap, Construct)
transpose e n =
    case I.lookup n e of
        Just (Op _dims (Transpose dir) [node]) -> Just $ (dir, sourceNode node)
        _ -> Nothing

{-

-}
xIm :: Internal -> Node -> Maybe Construct
xIm e n =
    case I.lookup n e of
        Just (Op _dims ImagPart [node]) -> Just $ sourceNode node
        _ -> Nothing

{-

-}
iRe :: Internal -> Node -> Maybe Construct
iRe e n =
    case I.lookup n e of
        Just (Op _dims RealImag [re, im]) ->
            if isZero $ I.lookup im e
                then Just $ sourceNode re
                else Nothing
        _ -> Nothing

{-

-}
iIm :: Internal -> Node -> Maybe Construct
iIm e n =
    case I.lookup n e of
        Just (Op _dims RealImag [re, im]) ->
            if isZero $ I.lookup re e
                then Just $ sourceNode im
                else Nothing
        _ -> Nothing

{-

-}
scale :: Internal -> Node -> Maybe (Construct, Construct)
scale e n =
    case I.lookup n e of
        Just (Op _dims ScaleV [s, v]) -> Just (sourceNode s, sourceNode v)
        _ -> Nothing

{-

-}
proj :: Internal -> Node -> Maybe (Subspace, Construct)
proj e n =
    case I.lookup n e of
        Just (Op _dims (Project ss) [v]) -> Just (ss, sourceNode v)
        _ -> Nothing

{-

-}
inject :: Internal -> Node -> Maybe (Subspace, Construct)
inject e n =
    case I.lookup n e of
        Just (Op _dims (Inject ss) [v]) -> Just (ss, sourceNode v)
        _ -> Nothing

{-

-}
zeros :: Internal -> Node -> Maybe Construct
zeros e n =
    case I.lookup n e of
        Just (Const dims 0) -> Just (\e -> addEdge e $ Const dims 0)
        _ -> Nothing

{-

-}
fun1 :: OpId -> Internal -> Node -> Maybe Construct
fun1 op e n =
    case I.lookup n e of
        Just (Op Dim0 op1 [arg]) ->
            if op == op1
                then Just $ sourceNode arg
                else Nothing
        _ -> Nothing

sqrt = fun1 Sqrt

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

{-

-}
const :: Internal -> Node -> Maybe (Double, Construct)
const e n =
    case I.lookup n e of
        Just (Const _ d) -> Just (d, sourceNode n)
        _ -> Nothing

{-

Match a composed pattern to one or more consumers of the current node
-}
mAll, mOne, mAny ::
       Consumers -> (Internal -> Node -> Maybe a) -> Internal -> Node -> Maybe a
mAll consumers match exprs node =
    case I.lookup node consumers of
        Nothing ->
            error $ "mAll " ++ show (node, consumers, pretty (exprs, node))
        Just nodes ->
            case catMaybes $ P.map (match exprs) $ Set.toList nodes of
                [a] -> Just a
                _ -> Nothing

mOne consumers match exprs node =
    case liftM Set.toList $ I.lookup node consumers of
        Nothing ->
            error $ "mOne " ++ show (node, consumers, pretty (exprs, node))
        Just [consumer] -> match exprs consumer
        Just _ -> Nothing

mAny consumers match exprs node =
    case I.lookup node consumers of
        Nothing ->
            error $ "mAny " ++ show (node, consumers, pretty (exprs, node))
        Just nodes ->
            case catMaybes $ P.map (match exprs) $ Set.toList nodes of
                (a:_) -> Just a
                _ -> Nothing

{-

-- FIXME to make constructors and matchers consistent, and for JFP paper
We could define compuations using compose and parallel constructors, and mirror those
into this hierarchy.
The same constructors could be used to extract the decoration from
the edges making the structure of the decoration retreived unambiguous.
The constructors for building complicated matches or complicated constructors
should be related to operations in pi-calculus.
%\begin{code}
class Composition a b c | a b -> c where
    o :: a -> b -> c

instance Composition  (Internal -> Node -> Maybe (x1,Construct))
                      (Internal -> Node -> Maybe (x2,b))
                      (Internal -> Node -> Maybe ((x1,x2),b))
  where
    o f g e n = case f e n of
                   Just (x2,n2) -> let (e',n2') = n2 e in case g e' n2' of
                                                            Just (x1,n1) ->
                   _ -> Nothing

instance Composition  (Internal -> Node -> Maybe (b,Construct))
                      (Internal -> Node -> Maybe a)
                      (Internal -> Node -> Maybe (b,a))
  where
    o f g e n = case f e n of
                   Just (b,n2) -> let (e',n2') = n2 e in liftM (b,) $ g e' n2'

instance Composition  (Internal -> Node -> Maybe (Construct,Construct))
                      (Internal -> Node -> Maybe a,Internal -> Node -> Maybe b)
                      (Internal -> Node -> Maybe (a,b))
  where
    o f (g,h) e n = case f e n of
                      Just (n1,n2) -> let (e',n2') = n2 e
                                          (e'',n1') = n1 e'
                                      in case (g e n, h e n) of
                                           (Just a, Just b) -> Just (a,b)
                                           _ -> Nothing
%\end{code}
                      (Internal -> Node -> Maybe Construct)
                       (Internal -> Node -> Maybe [Construct])
                       (IntMap ExpressionEdge -> I.Key -> Maybe [Construct]))
--FIXME : make this a class
 need a class for things which are nodes and need to be expanded
 need a class for things which are not nodes and just get carried along
 *** this is if we want to maximally expand the tree going backwards
--this would allow partial matching of the dependency tree, and make the tree structure
  and existence of decorations transparent

Composition of matchers
o :: (Internal -> Node -> Maybe (Internal -> (Internal,b)))
  -> (Internal -> b -> Maybe (Internal -> (Internal,a)))
  -> Internal -> N -> Maybe (Internal -> (Internal,a))
-}
o match1 match2 e n =
    case match1 e n of
        Just n2 ->
            let (e', n2') = n2 e
             in match2 e' n2'
        _ -> Nothing

o1 match1 match2 e n =
    case match1 e n of
        Just (a, n2) ->
            let (e', n2') = n2 e
             in liftM (a, ) $ match2 e' n2'
        _ -> Nothing

o11 match1 match2 e n =
    case match1 e n of
        Just (a, n2) ->
            let (e', n2') = n2 e
             in case match2 e' n2' of
                    Just (b, n3) -> Just ((a, b), n3)
                    Nothing -> Nothing
        _ -> Nothing

oReIm match2 e n =
    case reIm e n of
        Just (re, im) ->
            let (_, re') = re e
                (_, im') = im e
             in case (match2 e re', match2 e im') of
                    (Just n1, Just n2) -> Just (n1, n2)
                    _ -> Nothing
        _ -> Nothing
{-

-}
