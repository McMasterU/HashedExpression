{-
\documentclass[10pt]{amsart}
\usepackage{fancyvrb}
\DefineVerbatimEnvironment{code}{Verbatim}{fontsize=\small}
\usepackage{comment}
\usepackage{geometry}
\usepackage{listings}
\usepackage{algpseudocode}
\usepackage{algorithm}

\begin{document}

\title{HashedDerivative.lhs}
\maketitle

(c) 2010 Christopher Kumar Anand

Caluclating Derivatives.
\begin{comment}
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module HashedDerivative where

import HashedConstruct
import HashedExpression
import HashedInstances (pretty')

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C

--import HashedMatch (o)
--import qualified HashedMatch as M
import Data.Maybe (catMaybes)

--import Data.IntMap (IntMap)
import qualified Data.IntMap as I

import qualified Data.List as List

--import Data.Map (Map)
import qualified Data.Map as Map

--import Debug.Trace
import HashedSimplify (isDeepZero, mkSCZ, simplify')
import HashedTransformable ()

{-
\end{comment}

use functions to prevent warnings, but keep modules in scope for debugging
-}
p = C.pack

mp = map C.pack

lookupDebug = Map.lookup

{-

Wrapper for expression with function and gradient
-}
data FGrad =
    FGrad
        Internal -- expression map
        Node -- Compound node, contains compound grad and function
        Node -- the function node
        [(Node, ByteString)] -- parameters (with respect to which we don't differentiate)
        [((Node, Node), ByteString)] -- (variables, pdiffs)
    deriving (Eq)

instance Show FGrad where
    show (FGrad exprs _top fun param vars) =
        "{f = " ++
        p fun ++
        "; params = {" ++
        (concat $ List.intersperse ", " $ map (show . snd) param) ++
        "}" ++
        "; vars = {" ++
        (concat $ List.intersperse ", " $ map (show . snd) vars) ++
        "}" ++ "}" ++ concatMap pd vars
      where
        p n = pretty $ Expression n exprs
        pd ((v, ddv), _n) = "; df/d" ++ p v ++ " = " ++ p ddv

{-

Variables for calculating gradients.
We need to
-}
data Vars =
    Vars [(Subspace, ExpressionEdge)]
    deriving (Show, Eq, Ord)

{-

Apply partDiff to a list of nodes, accumulating new nodes so generated.
-}
class Diff a where
    partDiff :: ByteString -> a -> a
    diff :: [ByteString] -> a -> a

instance Diff Scalar where
    partDiff name (Scalar (Expression n exprs)) = Scalar $ Expression n' exprs'
      where
        (exprs', n') =
            partDiff'
                ( \dims1 n1 exprs1 _node1 ->
                      if name == n1
                          then if dims1 == Dim0
                                   then addEdge' exprs1 (Const Dim0 1)
                                   else error
                                            "partDiff can only differentiate with respect to a scalar"
                          else addEdge' exprs1 (Const Dim0 0)
                , \a b c -> error $ "partDiff found Rel elem " ++ show (a, b, c))
                exprs
                n
    diff names (Scalar (Expression n exprs)) = Scalar $ Expression n' exprs'
      where
        (exprs', n') =
            partDiff'
                ( \dims1 n1 exprs1 _node ->
                      if n1 `elem` names
                          then addEdge' exprs1 (DVar dims1 n1)
                          else addEdge' exprs1 (Const dims1 0)
                , \a b c -> error $ "diff found Rel elem " ++ show (a, b, c))
                exprs
                n

noDiff _dims _n exprs node = Expression node exprs

mkE :: Node -> Internal -> String -> Expression
mkE node exprs err =
    case I.lookup node exprs of
        Just _ -> Expression node exprs
        Nothing ->
            error $
            "mkE node not in table \"" ++ err ++ "\"" ++ show (node, exprs)

partDiff' ::
       ( Dims -> ByteString -> Internal -> Node -> Expression
       , ExpressionEdge -> Internal -> Node -> Expression {- must be RelElem -}
        )
    -> Internal
    -> Node
    -> (Internal, Node)
partDiff' (dVar, dRel) exprs node = (exprs', node')
  where
    prettyInput = pretty $ Expression node exprs
    gateScalar _op result = result
    Expression node' exprs' =
        simplify $
        case I.lookup node exprs of
            Nothing ->
                error $
                "partDiff didn't find node " ++ take 200 (show (node, exprs))
            Just (Const dims _) -> addEdge' exprs (Const dims 0)
            Just (Var dims n) -> dVar dims n exprs node
            Just (DVar dims _n) -> addEdge' exprs (Const dims 0)
            Just re@(RelElem _ _ _) -> dRel re exprs node
            Just (Op dims op inputs)
            -- for operations which commute with differentiation, calculate once
             ->
                let d1Input =
                        let (d1E', node) =
                                case inputs of
                                    [input] ->
                                        partDiff' (dVar, dRel) exprs input
                                    _ ->
                                        error $
                                        "partDiff " ++
                                        show op ++
                                        " needs 1 input " ++
                                        take 200 (show inputs)
                         in addEdge' d1E' (Op dims op [node])
                 in case op of
                        Transpose swap ->
                            error $
                            "derivative doesn't know how to handle transpose " ++
                            show swap
                        Reglzr _dk _rk ->
                            error "HD.partDiff' Rglzr not implemented"
                        GradReglzr _dk _rk ->
                            error "HD.partDiff' Rglzr not implemented"
                        RealPart -> d1Input
                        ImagPart -> d1Input
                        RealImag ->
                            case inputs of
                                [i1, i2] ->
                                    let (e1, dn1) =
                                            partDiff' (dVar, dRel) exprs i1
                                        (e2, dn2) = partDiff' (dVar, dRel) e1 i2
                                     in addEdge'
                                            e2
                                            (Op dims RealImag [dn1, dn2])
                                _ ->
                                    error $
                                    "partDiff " ++
                                    show op ++
                                    " needs 2 input " ++ take 200 (show inputs)
                        SubMask ->
                            case inputs of
                                [i1, i2] ->
                                    let (e1, dn1) =
                                            partDiff' (dVar, dRel) exprs i1
                                        (e2, dn2) =
                                            partDiff' (dVar, dRel) exprs i2
                                     in if isDeepZero e1 dn1
                                            then addEdge'
                                                     e2
                                                     (Op dims NegMask [i1, dn2])
                                            else error $
                                                 "partDiff SubMask first arg is not constant " ++
                                                 pretty (exprs, i1)
                                _ ->
                                    error $
                                    "partDiff " ++
                                    show op ++
                                    " needs 2 input " ++ take 200 (show inputs)
                        NegMask ->
                            case inputs of
                                [i1, i2] ->
                                    let (e1, dn1) =
                                            partDiff' (dVar, dRel) exprs i1
                                        (e2, dn2) =
                                            partDiff' (dVar, dRel) exprs i2
                                     in if isDeepZero e1 dn1
                                            then addEdge'
                                                     e2
                                                     (Op dims NegMask [i1, dn2])
                                            else error $
                                                 "partDiff NegMask first arg is not constant " ++
                                                 pretty (exprs, i1)
                                _ ->
                                    error $
                                    "partDiff " ++
                                    show op ++
                                    " needs 2 input " ++
                                    (take 200 $ show inputs)
                        Sum ->
                            let (exprs', nodes) =
                                    List.mapAccumR
                                        (partDiff' (dVar, dRel))
                                        exprs
                                        inputs
                                (exprs'', mergedConsts) =
                                    mergeConstsBy (+) (exprs', nodes)
                                nonZeros = filterOut isZero exprs'' mergedConsts
                             in case length nonZeros of
                                    0 -> addEdge' exprs (Const Dim0 0)
                                    1 ->
                                        mkE (head nonZeros) exprs'' $
                                        "pD:Sum " ++
                                        (concatMap
                                             (pretty . flip Expression exprs)
                                             inputs) ++
                                        " | " ++
                                        (concatMap
                                             (pretty . flip Expression exprs')
                                             nodes) ++
                                        prettyInput
                                    _ -> addEdge' exprs'' (Op dims Sum nonZeros)
                        Neg -> d1Input -- means $\d(-f) = - \d(f)$  (a common pattern)
                        Abs ->
                            error
                                "partDIff encountered abs() which is not differentiable"
                        Signum ->
                            error
                                "partDIff encountered signum() which is not differentiable"
                        Prod ->
                            let (exprs', diffFactors :: [Node]) =
                                    List.mapAccumR
                                        (partDiff' (dVar, dRel))
                                        exprs
                                        inputs
                                diffRest =
                                    zip diffFactors $ pickOne inputs [] :: [( Node
                                                                            , ( Node
                                                                              , [Node]))]
                                withoutZeros =
                                    filter
                                        (not .
                                         isZero . (flip I.lookup exprs') . fst)
                                        diffRest
                                (exprs'', prods) =
                                    List.mapAccumR
                                        (\ee (dterm, (_term, others)) ->
                                             let (ee1, newOps) =
                                                     mergeConstsBy
                                                         (*)
                                                         (ee, dterm : others)
                                                 newOps' =
                                                     case map (`I.lookup` ee1)
                                                              newOps of
                                                         (Just (Const Dim0 0):_) ->
                                                             Const Dim0 0
                                                         [] -> Const Dim0 0
                                                         [Just (Const Dim0 1)] ->
                                                             Const Dim0 1
                                                         [Just (Const Dim0 1), Just x] ->
                                                             x
                                                         ((Just (Const Dim0 1)):_xs) ->
                                                             Op Dim0 Prod $
                                                             tail newOps
                                                         _xs ->
                                                             Op Dim0 Prod newOps
                                              in addEdge ee1 newOps')
                                        exprs'
                                        withoutZeros
                             in case prods of
                                    [] -> addEdge' exprs'' (Const Dim0 0)
                                    [term] -> Expression term exprs''
                                    terms ->
                                        addEdge' exprs'' (Op dims Sum terms)
                        Div ->
                            let ((num, denom), (exprs', [diffNum, diffDenom])) =
                                    case inputs of
                                        (num:denom:[]) ->
                                            ( (num, denom)
                                            , List.mapAccumR
                                                  (partDiff' (dVar, dRel))
                                                  exprs
                                                  [num, denom])
                                        _ ->
                                            error $
                                            "partDiff Div needs 2 inputs " ++
                                            (take 200 $ show inputs)
                                numS = Scalar $ Expression num exprs'
                                denomS = Scalar $ Expression denom exprs'
                                diffNumS = Scalar $ Expression diffNum exprs'
                                diffDenomS =
                                    Scalar $ Expression diffDenom exprs'
                                Scalar e =
                                    case diffNumS of
                                        0 ->
                                            -(diffDenomS * numS) /
                                             (denomS * denomS)
                                        _ ->
                                            diffNumS / denomS -
                                            (diffDenomS * numS) /
                                            (denomS * denomS)
                             in gateScalar Div e
                        Sqrt ->
                            gateScalar Sqrt $
                            let (es', diffN) =
                                    partDiff' (dVar, dRel) exprs (head inputs) -- safe via getScalar
                                (es'', result) =
                                    (0.5 * (sourceNode diffN) /
                                     (sourceNode node))
                                        es'
                             in Expression result es''
                        Exp ->
                            gateScalar Exp $
                            let (es', diffN) =
                                    partDiff' (dVar, dRel) exprs (head inputs) -- safe via getScalar
                                (es'', result) =
                                    ((sourceNode diffN) * (sourceNode node)) es'
                             in Expression result es''
                        Log ->
                            gateScalar Log $
                            let (es', diffN) =
                                    partDiff' (dVar, dRel) exprs (head inputs) -- safe via getScalar
                                diffX = (Scalar (Expression diffN es'))
                                x = (Scalar (Expression (head inputs) es'))
                                Scalar e = diffX / x
                             in e
                        Sin ->
                            gateScalar Sin $
                            let (es', diffN) =
                                    partDiff' (dVar, dRel) exprs (head inputs) -- safe via getScalar
                                diffX = (Scalar (Expression diffN es'))
                                cosX =
                                    cos (Scalar (Expression (head inputs) es'))
                                Scalar e = diffX * cosX
                             in e
                        Cos ->
                            gateScalar Cos $
                            let (es', diffN) =
                                    partDiff' (dVar, dRel) exprs (head inputs) -- safe via getScalar
                                diffX = (Scalar (Expression diffN es'))
                                negSinX =
                                    -sin (Scalar (Expression (head inputs) es'))
                                Scalar e = diffX * negSinX
                             in e
                        Tan ->
                            gateScalar Tan $
                            let (es', diffN) =
                                    partDiff' (dVar, dRel) exprs (head inputs) -- safe via getScalar
                                diffX = (Scalar (Expression diffN es'))
                                secSqrX =
                                    recip
                                        ((cos (Scalar
                                                   (Expression (head inputs) es'))) ^
                                         2)
                                Scalar e = diffX * secSqrX
                             in e
                        Acos ->
                            gateScalar Acos $
                            let (es', diffN) =
                                    partDiff' (dVar, dRel) exprs (head inputs) -- safe via getScalar
                                diffX = (Scalar (Expression diffN es'))
                                aCosDiffX =
                                    -recip
                                         (sqrt
                                              (1 -
                                               (Scalar
                                                    (Expression
                                                         (head inputs)
                                                         es')) ^
                                               2))
                                Scalar e = diffX * aCosDiffX
                             in e
                        Asin ->
                            gateScalar Asin $
                            let (es', diffN) =
                                    partDiff' (dVar, dRel) exprs (head inputs) -- safe via getScalar
                                diffX = (Scalar (Expression diffN es'))
                                aSinDiffX =
                                    recip
                                        (sqrt
                                             (1 -
                                              (Scalar
                                                   (Expression (head inputs) es')) ^
                                              2))
                                Scalar e = diffX * aSinDiffX
                             in e
                        Atan ->
                            gateScalar Atan $
                            let (es', diffN) =
                                    partDiff' (dVar, dRel) exprs (head inputs) -- safe via getScalar
                                diffX = (Scalar (Expression diffN es'))
                                aTanDiffX =
                                    recip
                                        (1 +
                                         (Scalar (Expression (head inputs) es')) ^
                                         2)
                                Scalar e = diffX * aTanDiffX
                             in e
                        Cosh ->
                            gateScalar Cosh $
                            let (es', diffN) =
                                    partDiff' (dVar, dRel) exprs (head inputs) -- safe via getScalar
                                diffX = (Scalar (Expression diffN es'))
                                sinhX =
                                    sinh (Scalar (Expression (head inputs) es'))
                                Scalar e = diffX * sinhX
                             in e
                        Sinh ->
                            gateScalar Sinh $
                            let (es', diffN) =
                                    partDiff' (dVar, dRel) exprs (head inputs) -- safe via getScalar
                                diffX = (Scalar (Expression diffN es'))
                                coshX =
                                    cosh (Scalar (Expression (head inputs) es'))
                                Scalar e = diffX * coshX
                             in e
                        Tanh ->
                            gateScalar Tanh $
                            let (es', diffN) =
                                    partDiff' (dVar, dRel) exprs (head inputs) -- safe via getScalar
                                diffX = (Scalar (Expression diffN es'))
                                diffTanhX =
                                    recip
                                        (cosh
                                             (Scalar
                                                  (Expression (head inputs) es'))) ^
                                    2
                                Scalar e = diffX * diffTanhX
                             in e
                        Acosh ->
                            gateScalar Acosh $
                            let (es', diffN) =
                                    partDiff' (dVar, dRel) exprs (head inputs) -- safe via getScalar
                                diffX = (Scalar (Expression diffN es'))
                                diffAcosX =
                                    recip
                                        (sqrt
                                             ((Scalar
                                                   (Expression (head inputs) es')) ^
                                              2 +
                                              1))
                                Scalar e = diffX * diffAcosX
                             in e
                        Asinh ->
                            gateScalar Asinh $
                            let (es', diffN) =
                                    partDiff' (dVar, dRel) exprs (head inputs) -- safe via getScalar
                                diffX = (Scalar (Expression diffN es'))
                                diffAsinX =
                                    recip
                                        (sqrt
                                             ((Scalar
                                                   (Expression (head inputs) es')) ^
                                              2 -
                                              1))
                                Scalar e = diffX * diffAsinX
                             in e
                        Atanh ->
                            gateScalar Atanh $
                            let (es', diffN) =
                                    partDiff' (dVar, dRel) exprs (head inputs) -- safe via getScalar
                                diffX = (Scalar (Expression diffN es'))
                                diffAtanX =
                                    recip
                                        (1 -
                                         ((Scalar (Expression (head inputs) es')) ^
                                          2))
                                Scalar e = diffX * diffAtanX
                             in e
                        Dot ->
                            let ((x, y), (exprs', [dX, dY])) =
                                    case inputs of
                                        (x:y:[]) ->
                                            ( (x, y)
                                            , List.mapAccumR
                                                  (partDiff' (dVar, dRel))
                                                  exprs
                                                  [x, y])
                                        _ ->
                                            error $
                                            "partDiff Dot needs 2 inputs " ++
                                            (take 200 $ show inputs)
                                dXdotY =
                                    Scalar $
                                    addEdge' exprs' (Op Dim0 Dot [dX, y])
                                dYdotX =
                                    Scalar $
                                    addEdge' exprs' (Op Dim0 Dot [dY, x])
                                Scalar e = dXdotY + dYdotX
                             in e
                        MapND _ _ ->
                            error "partDiff unimplemented           MapND ->  "
                        SCZ e@(Expression sczNode sczExprs)
                  -- scalar diffs:  diff combining expression by the scalar
                  --                and scale the result of the
                         ->
                            let allVars = variables sczExprs sczNode
                                scalars =
                                    map fst $
                                    filter
                                        ((== Dim0) . getDimE sczExprs . snd)
                                        allVars
                                forEachScalar ex s = (ex''', scaledNode)
                                  where
                                    (ex', sczNode) =
                                        addEdge
                                            ex
                                            (mkSCZ
                                                 dims
                                                 (case partDiff s $ Scalar e of
                                                      Scalar x -> x)
                                                 inputs)
                                    Expression diffNode ex'' =
                                        dVar Dim0 s ex' node
                                    (ex''', scaledNode) =
                                        addEdge
                                            ex''
                                            (Op dims ScaleV [diffNode, sczNode])
                                (exprs', scalarDiffs) =
                                    List.mapAccumR forEachScalar exprs scalars
                  -- vector diffs:  just replace vector inputs by their differentials, one at a time
                  -- differentials of vector inputs need to be computed using the chain rule, with the
                  -- differentials added as a new input
                  --
                                (exprs'', oneDiffedAtATime :: [Maybe Node]) =
                                    List.mapAccumR chainRule exprs' $
                                    zip [0 ..] inputs
                  -- calculate derivative inside SCZ with respect to RelElem at the same time
                  -- as differentiating the input, but ignore zero inputs on the outside
                                chainRule ::
                                       Internal
                                    -> (Int, Node)
                                    -> (Internal, Maybe Node)
                                chainRule e (inputNum, input) =
                                    if isDeepZero e' dInput
                                        then (e, Nothing)
                                        else (e'', Just scz)
                                  where
                                    (e', dInput) =
                                        partDiff' (dVar, dRel) e input
                                    (sczExprs', sczNode') =
                                        partDiff'
                                            (noDiff, dThisRel)
                                            sczExprs
                                            sczNode
                                    (e'', scz) =
                                        addEdge e' $
                                        mkSCZ
                                            dims
                                            (mkE sczNode' sczExprs' $
                                             "scz " ++ prettyInput) $
                                        inputs ++ [dInput]
                                    dThisRel (RelElem array bndy reIdx) dTExprs _dTNode =
                                        if inputNum == array
                                            then addEdge' dTExprs $
                                                 RelElem
                                                     (length inputs)
                                                     bndy
                                                     reIdx -- differential in new input
                                            else addEdge' dTExprs $ Const Dim0 0 -- this was wrong, mkE dTNode dTExprs $ "leave node alone " ++ prettyInput
                                    dThisRel errEE errE errN =
                                        error $
                                        "dThisRel found " ++
                                        show (errEE, errE, errN)
                -- then add up all the terms
                             in addEdge' exprs'' $
                                Op dims Sum $
                                scalarDiffs ++ (catMaybes oneDiffedAtATime)
                        ScaleV ->
                            let (exprs' :: Internal, [x, y, dX, dY] :: [Internal -> ( Internal
                                                                                    , Node)]) =
                                    case inputs of
                                        (x:y:[]) ->
                                            let (exprs'' :: Internal, [dX, dY]) =
                                                    List.mapAccumR
                                                        (partDiff' (dVar, dRel))
                                                        exprs
                                                        [x, y]
                                             in ( exprs''
                                                , map sourceNode [x, y, dX, dY])
                                        _ ->
                                            error $
                                            "partDiff ScaleV needs 2 inputs " ++
                                            (take 200 $ show inputs)
                                dims = getDimE exprs node
                                scaleOrProd =
                                    if Dim0 == dims
                                        then (*)
                                        else scale :: Construct -> Construct -> Construct
                             in bigE $
                                ((dX `scaleOrProd` y) + (x `scaleOrProd` dY))
                                    exprs'
                        PFT _dir _idx -> d1Input
                        FT _dir -> d1Input
                        Project _ss -> d1Input
                        Inject _ss -> d1Input
                        Extract _elem ->
                            case inputs of
                                [single] ->
                                    case I.lookup single exprs of
                                        op ->
                                            error $
                                            "partDiff encountered unhandled Extract " ++
                                            show op
                                _ ->
                                    error $
                                    "partDiff encountered unhandled Extract " ++
                                    show inputs
                -- JLMP - Extract takes a node out of tuple (Compound), so the derivative is just
                      -- the derivative of the Extracted node
                      -- fix this and Compound if we use them
                        Compound _size ->
                            error "partDiff unimplemented    Compound          "

{-
$$\frac{d}{dx} log(f(x)) = \frac{f'(x)}{f(x)}$$
-}
{-
$$\frac{d}{dx} sin(f(x)) = f'(x)cos(f(x))$$
-}
{-
$$\frac{d}{dx} cos(f(x)) = f'(x)sin(f(x))$$
-}
{-
$$\frac{d}{dx} tan(f(x)) = f'(x)\frac{1}{(cos(x))^2}$$
-}
{-
$$\frac{d}{dx} acos(f(x)) = f'(x)\frac{-1}{\sqrt{1-(f(x))^2}}$$
-} -}
{-
$$\frac{d}{dx} asin(f(x)) = f'(x)\frac{1}{\sqrt{1-(f(x))^2}}$$
-}
{-
$$\frac{d}{dx} atan(f(x)) = f'(x)\frac{1}{(f(x))^2+1}$$
-}
{-
$$\frac{d}{dx} cosh(f(x)) = f'(x)sinh(f(x))$$
-}
{-
$$\frac{d}{dx} sinh(f(x)) = f'(x)cosh(f(x))$$
-}
{-
$$\frac{d}{dx} tanh(f(x)) = f'(x)\frac{1}{cosh^2(f(x))}$$
-}
{-
$$\frac{d}{dx} acosh(f(x)) = f'(x)\frac{1}{\sqrt{(f(x))^2+1}}$$
-}
{-
$$\frac{d}{dx} asinh(f(x)) = f'(x)\frac{1}{\sqrt{(f(x))^2-1}}$$
-}
{-
$$\frac{d}{dx} atanh(f(x)) = f'(x)\frac{1}{1-(f(x))^2}$$
-}
{-

-}
mergeConstsBy ::
       (Double -> Double -> Double) -> (Internal, [Node]) -> (Internal, [Node])
mergeConstsBy op (exprs, nodes@(n1:_)) = (exprs', combined ++ nonconsts)
  where
    consts =
        concatMap
            (\n ->
                 case I.lookup n exprs of
                     Just (Const _ x) -> [x]
                     _ -> [])
            nodes
    nonconsts =
        filter
            (\n ->
                 case I.lookup n exprs of
                     Just (Const _ _) -> False
                     _ -> True)
            nodes
    (exprs', combined) =
        if null consts
            then (exprs, [])
            else (\(a, b) -> (a, [b])) $
                 addEdge exprs (Const (getDimE exprs n1) $ foldr1 op consts)
mergeConstsBy _ (exprs, []) = (exprs, [])

-- list of singleton and rest of list
pickOne (b:bs) cs = (b, cs ++ bs) : (pickOne bs (b : cs))
pickOne [] _cs = []

-- map over one element of a list at a time, and return the whole list in the same order
mapOneAtATime fun = mapOne' fun []

mapOne' fun fstBack (b:bs) =
    (reverse fstBack ++ ((fun b) : bs)) : (mapOne' fun (b : fstBack) bs)
mapOne' _fim _fstBack [] = []

testMapOne =
    [[11, 2, 3], [1, 12, 3], [1, 2, 13]] == mapOneAtATime (+ 10) [1 .. 3]

-- map over one element of a list at a time, and  an accumulator
mapAccumOneAtATime :: (s -> b -> (s, b)) -> s -> [b] -> (s, [[b]])
mapAccumOneAtATime fun state = mapAccumOne fun state []

mapAccumOne fun s fstBack (b:bs) =
    let (s', b') = fun s b
        (s'', rest) = mapAccumOne fun s' (b : fstBack) bs
     in (s'', (reverse fstBack ++ (b' : bs)) : rest)
mapAccumOne _ s _fstBack [] = (s, [])

testMapAccumOne =
    (10000, [[11, 2, 3], [1, 102, 3], [1, 2, 1003]]) ==
    mapAccumOneAtATime (\s a -> (s * 10, a + s)) 10 [1 .. 3]

{-


To calculate the gradient, we need to normalize the expression of the differential of a function to put all
of the differentials of vector variables on their own side of dot products.
To do this we need to apply adjointness relationships to "lift" d(X) terms in expressions up to the top level.
The only place vectors can be turned into scalars, is in the dot product,
we only need to look at those nodes, but we have to drill down to the bottom to see if there are any dX terms.
If there are, we need to recursively apply adjointess.




Generic expression transformer.

Collect a group of identical nodes into a scaled node, but don't scale by 1
-}
groupedToScaled :: Internal -> [(Double, Node)] -> (Internal, Node)
groupedToScaled _ [] = error "newForGroup shouldn't get nullary operations"
groupedToScaled exprs scaledGroup =
    case sum scalings of
        0 -> addEdge exprs (Const dims 0)
        1 -> (exprs, node)
        s ->
            let (e, n) = addEdge exprs (Const Dim0 s)
             in addEdge
                    e
                    (Op dims
                         (case dims of
                              Dim0 -> Prod
                              _ -> ScaleV)
                         [n, node])
  where
    (scalings, node:_rest) = unzip scaledGroup
    dims = getDimE exprs node

{-

FIXME:  if a component of the gradient is zero we should not use it in the calculation!  Are we?

 $(dX\cdot W_1) + (dX\cdot W_2) + ...$ terms into $dX\cdot(W_1+W_2+...)$
-}
gatherDiffs :: ((Internal, Node) -> (Internal, [(Node, Node)]))
gatherDiffs (exprsNoOne, node) =
    if isZero $ I.lookup node exprsNoOne
        then error $
             "HD.gatherDiffs found zero gradient which isn't supported " ++
             pretty' (exprsNoOne, node)
        else aux exprsNoOne
                 (case I.lookup node exprsNoOne of
                      Just (Op Dim0 Sum inputs) -> inputs
                      _ -> [node])

aux exprsNoOne inputs =
    let (exprs, one) = addEdge exprsNoOne $ Const Dim0 1
        scaledDiffs = concatMap extractDiff inputs
        sortedDiffs = List.sort scaledDiffs
        grouped = List.groupBy (\(x, _, _) (y, _, _) -> x == y) sortedDiffs
        --vector cases
        extractDiff n
            | Just (Op Dim0 Dot [left, right]) <- I.lookup n exprs
            , Just (DVar _ _name) <- I.lookup left exprs =
                [(left, Nothing, right)]
        extractDiff n
            | Just (Op Dim0 Prod [dot, scale]) <- I.lookup n exprs
            , Just (Op Dim0 Dot [left, right]) <- I.lookup dot exprs
            , Just (DVar _ _name) <- I.lookup left exprs =
                [(left, Just scale, right)]
        -- scalar cases
        extractDiff n
            | Just (Op Dim0 Prod factors) <- I.lookup n exprs
            , ([dv], rest) <- List.partition (nodeIsDifferential exprs) factors =
                case rest of
                    [] -> [(dv, Nothing, one)]
                    [scale] -> [(dv, Nothing, scale)]
                    _ ->
                        error
                            "HD.gatherDiffs found product with differential and more than 2 factors, needs foldr"
        extractDiff n
            | Just (DVar _ _name) <- I.lookup n exprs = [(n, Nothing, one)]
        -- this shouldn't occur because ScaleV should only be applied to vectors
        extractDiff n
            | Just (Op Dim0 ScaleV [scale, dot]) <- I.lookup n exprs
            , Just (Op Dim0 Dot [left, right]) <- I.lookup dot exprs
            , Just (DVar _ _name) <- I.lookup left exprs =
                [(left, Just scale, right)]
        extractDiff n =
            error $
            "Error: extractDiff " ++
            show n ++
            (unlines $
             (pretty' (exprs, n)) : "inputs" : map (pretty' . (exprs, )) inputs)
        unzipDot ::
               Internal
            -> [(Node, Maybe Node, Node)]
            -> (Internal, (Node, Node))
        unzipDot e0 dotNodes@((dX, _, _):_) = (e2, (dX, innerSum))
            -- multiply right side of dot by the outer scalar
          where
            (e1, terms) = List.mapAccumR maybeScale e0 dotNodes
            -- add up the terms and simplify
            (e2, innerSum) =
                simplify' $ addEdge e1 (Op (getDimE e0 dX) Sum terms)
            maybeScale e (_dX, Nothing, right) = (e, right)
            maybeScale e (_dx, Just scale, right) =
                addEdge e $ Op (getDimE e0 dX) ScaleV [scale, right]
        unzipDot _ [] = error "unzipDot []"
        (e0, nodePairs) = List.mapAccumR unzipDot exprs grouped
     in (e0, nodePairs)

{-

-}
gatherDiffs1 f (Scalar (Expression n e)) =
    let (e1, diffGrads) = gatherDiffs (e, n)
        showDiffs e (nodeD, nodeG) =
            case (getDimE e nodeD, getDimE e nodeG) of
                (Dim1 _, Dim1 _) ->
                    show
                        ( (OneD $ Expression nodeD e)
                        , (f $ OneD $ Expression nodeG e))
                (Dim2 _, Dim2 _) ->
                    show
                        ( (TwoD $ Expression nodeD e)
                        , (simplify $ TwoD $ Expression nodeG e))
                (Dim3 _, Dim3 _) ->
                    show
                        ( (ThreeD $ Expression nodeD e)
                        , (simplify $ ThreeD $ Expression nodeG e))
                x -> "showDiffs not implemented" ++ show x
     in map (showDiffs e1) diffGrads

--         ([dimD,dimD2,dd3,dd4],[dimG,dimG2,dg3,dg4]) -> show ((FourD (dimD,dimD2,dd3,dd4) $ Expression nodeD e),(simplify $ FourD (dimG,dimG2,dg3,dg4) $ Expression nodeG e))
{-



*** should we have a special wrapper around Expression to contain the gradient and the original function?

*** how do we handle data, which is a parameter, when we have partial data, ie, a subspace of k-space?
  -- only named variables can appear

Find gradient of a function.
-}
funGrad :: Scalar -> [ByteString] -> FGrad
funGrad fun vars -- trace (show $ map (flip I.lookup eDiffs . fst) listDGrad) $
 = FGrad exprFinal compoundN funN params varsDiffs
  where
    Scalar (Expression nDiff eDiff) = diff vars fun
    (eDiffs, listDGradUnordered) =
        gatherDiffs $
        simplify'
            ( eDiff -- trace (("before extract:"++) $ pretty' $ simplify (eDiff,nDiff))
            , nDiff)
    listDGrad =
        concatMap
            (\v ->
                 filter
                     ((isD v) . flip I.lookup eDiffs . fst)
                     listDGradUnordered)
            vars
    isD v (Just (DVar _ name)) = name == v
    isD _ _ =
        error $
        "HD.funGrad non-dvar" ++
        show
            ( listDGradUnordered
            , map (flip I.lookup eDiffs . fst) listDGradUnordered)
    Scalar fE = fun
    (eGrad, gradN) =
        addEdge eDiffs $
        Op
            (CDims (map (getDimE exprComp) (map snd listDGrad)))
            (Compound $ length listDGrad) $
        map snd listDGrad --I'm not sure is exprComp is the right place to find the nodes, but it should work.
    (Expression _ exprComp, (nf, ng)) = merge fE (Expression gradN eGrad)
    (exprFinal, compoundN) =
        simplify' $
        addEdge exprComp $
        Op (CDims (map (getDimE exprComp) [nf, ng])) (Compound 2) [nf, ng] --I'm not sure is exprComp is the right place to find the nodes, but it should work.
    (params, nodeVarsUnsorted :: [(Node, ByteString)]) =
        List.partition (not . flip elem vars . snd) $
        map (\(name, n) -> (n, name)) $ variables exprFinal compoundN
    -- make sure variables are in the same order we were given them
    nodeVars = concatMap (\v -> filter ((v ==) . snd) nodeVarsUnsorted) vars
    (funN, listGradN) =
        case I.lookup compoundN exprFinal of
            Just (Op _ (Compound 2) [funN, listGradN]) -> (funN, listGradN)
            _ -> error "funGrad impossible 1"
    listGradSimp =
        case I.lookup listGradN exprFinal of
            Just (Op _ (Compound _) listGradSimp) -> listGradSimp
            _ -> error "finGrad impossible 2"
    varsDiffs = zipWith (\(vN, n) ddv -> ((vN, ddv), n)) nodeVars listGradSimp

{-
CKA:  Why not write scalar products as Dot products.  It is a natural generalization, and might simplify pattern matching.

Check for extractable gradients.  Either there is only one gradient component, given by a
single dot product with a differential, or given by a sum, in which summands either contain
no differentials, or are dot products with differentials on the left.
-}
isExtractable :: Internal -> Node -> Bool
isExtractable exprs node =
    isDotDiff exprs node ||
    case I.lookup node exprs of
        Just (Op _ Sum inputs) ->
            all
                (\n -> isDotDiff exprs n || (not $ containsDifferential exprs n))
                inputs
        _ -> False

isDotDiff exprs n =
    case I.lookup n exprs of
        Just (Op _ Dot [left, _right]) ->
            case I.lookup left exprs of
                Just (DVar _ _) -> True
                _ -> False
        Just (Op _ Prod [const, dot]) ->
            case map (flip I.lookup exprs) [const, dot] of
                [Just (Const Dim0 _), Just (Op _ Dot [left, _right])] ->
                    case I.lookup left exprs of
                        Just (DVar _ _) -> True
                        _ -> False
                _ -> False
        _ -> False

{-
Sort nodes by hashes, except that differentials come first.
-}
sortNodes exprs = List.sortBy cmpNodes
  where
    cmpNodes l r =
        case map (flip I.lookup exprs) [l, r] of
            [Just (DVar _ _), Just (DVar _ _)] -> compare l r
            [Just (DVar _ _), Just _] -> LT
            [Just _, Just (DVar _ _)] -> GT
            _ -> compare l r
