{-
(c) 2010-2012 Christopher Kumar Anand, Jessica LM Pavlin

Calculating Derivatives.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module HashedFactor
    ( factor'
    , commonTop
    , linOpDepth
    , floatNeg
    , floatNeg'
    , newFloatNeg
    , newFloatNeg'
    , factorNeg
    , pushNegIntoConst
    , normalizeSCZs
    , normalizeSCZ
    ) where

import HashedConstruct
import HashedExpression
import HashedInstances (pretty')
import HashedSimplify (mkSCZ, simpRewrite, simplify')

--import HashedMatch (o)
import qualified HashedMatch as M

--import Data.ByteString (ByteString)
--import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap as I

--import Data.Map (Map)
--import qualified Data.Map as Map
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Debug.Trace

{-


-}
tf description en =
    if skipDebug
        then en
        else case hasCycles en of
                 [] -> trace description $ en `seq` trace (pretty' en) en
                 x ->
                     error $
                     "HS.simplify found cycle at " ++
                     description ++ " " ++ show x ++ " " ++ show en

{-

New recursive negative pulling.
Do depth-first pulling.  If there is no Neg, then nothing will happen.
|fn| returns (newExpressions,(trueIfNegIsBeingFloated,newNode)).
-}
newFloatNeg' (exprs, node) = newFloatNeg exprs node

newFloatNeg exprs node =
    let (e1, (looseNeg, n1)) = fn exprs node
     in if looseNeg
            then error "newFloatNeg floated Neg up to top"
            else (e1, n1)

fn :: Internal -> Node -> (Internal, (Bool, Node))
fn exprs node
    | Just (Op _dims Neg [arg]) <- I.lookup node exprs
    , (expr1, (isNeg, node1)) <- fn exprs arg = (expr1, (not isNeg, node1))
    | Just (Op dims (Compound l) args) <- I.lookup node exprs
    , (expr1, negNodes) <- L.mapAccumR fn exprs args =
        case L.nub $ map fst negNodes of
            [loose] ->
                let (e2, n2) =
                        addEdge expr1 $ Op dims (Compound l) $ map snd negNodes
                 in (e2, (loose, n2))
            _ ->
                error $
                "HF.newFloatNeg differing signs for compound " ++ show negNodes
    | Just (Op dims Sum args) <- I.lookup node exprs
    , (expr1, negNodes) <- L.mapAccumR fn exprs args =
        case L.nub $ map fst negNodes of
            [loose] ->
                let (e2, n2) = addEdge expr1 $ Op dims Sum $ map snd negNodes
                 in (e2, (loose, n2))
            _ ->
                let Scalar (Expression nN nE) =
                        sum $
                        zipWith3
                            (\idx a nn ->
                                 (if fst nn
                                      then id
                                      else negate) $
                                 relElem0 idx a)
                            [0 ..]
                            args
                            negNodes
                    (sE, sN) = simplify' (nE, nN)
                    relElem0 idx arg =
                        relElem idx ZeroMargin $ zeroOffsets $ getDimE exprs arg
                    (newE, newN) =
                        addEdge expr1 $
                        mkSCZ dims (Expression sN sE) $ map snd negNodes
                 in mt ("fn.sum" ++ show negNodes) (newE, (False, newN))
    | Just (Op dims (SCZ (Expression sn se)) args) <- I.lookup node exprs
    , (expr1, negNodes) <- L.mapAccumR fn exprs args =
        let (sE, sN) = simplify' $ rewrite se sn
            (e2, n2) =
                addEdge expr1 $ mkSCZ dims (Expression sN sE) $ map snd negNodes
            xchSign idx =
                if idx < 0 || idx >= length negNodes
                    then error $ "HF.fn scz idx " ++ show (idx, negNodes)
                    else fst $ negNodes !! idx
            rewrite e0 head
                | Just (Op dims op args) <- I.lookup head e0 =
                    let (e1, newAs) = L.mapAccumR rewrite e0 args
                     in addEdge e1 $ Op dims op newAs
                | Just (RelElem idx _bnd _off) <- I.lookup head e0 =
                    if xchSign idx
                        then addEdge e0 $ Op Dim0 Neg [head]
                        else (e0, head)
                | otherwise = (e0, head)
         in mt ("fn.scz" ++ show negNodes) (e2, (False, n2))
    | Just (Op dims op args) <- I.lookup node exprs
    , (expr1, negNodes) <- L.mapAccumR fn exprs args
    , canPull op || not (any fst negNodes) =
        case L.nub $ map fst negNodes of
            [loose] ->
                let (e2, n2) = addEdge expr1 $ Op dims op $ map snd negNodes
                 in (e2, (loose, n2))
            _ ->
                error $
                "HF.newFloatNeg differing signs for " ++ show (op, negNodes)
    | Just (Var _ _) <- I.lookup node exprs = (exprs, (False, node))
    | Just (DVar _ _) <- I.lookup node exprs = (exprs, (False, node))
    | Just (Const _ _) <- I.lookup node exprs = (exprs, (False, node))
    | Just RelElem {} <- I.lookup node exprs =
        error $ "HF.fn found RelElem " ++ take 200 (show exprs)
    | otherwise = error $ "HF.fn unhandled " ++ pretty' (exprs, node)

{-


-}
floatNeg' (exprs, node) = floatNeg exprs node

floatNeg exprs node
    | Just (Op dims (Compound l) args) <- I.lookup node exprs =
        let (exprTry, tryPull) = L.mapAccumR floatNeg exprs args
         in mt ("~~compound~~~" ++ (show $ zip args tryPull)) $
            if tryPull == args
                then mt "no" (exprs, node)
                else addEdge exprTry $ Op dims (Compound l) tryPull
    | Just (Op dims Sum args) <- I.lookup node exprs =
        let (exprTry, tryPull) = L.mapAccumR pullNeg exprs args
            Scalar (Expression nN nE) =
                sum $
                zipWith3
                    (\idx a b ->
                         (if a == b
                              then id
                              else negate) $
                         relElem0 idx a)
                    [0 ..]
                    args
                    tryPull
            (sE, sN) = simplify' (nE, nN)
            relElem0 idx arg =
                relElem idx ZeroMargin $ zeroOffsets $ getDimE exprs arg
         in mt ("~~sum~~~" ++ (show $ zip args tryPull)) $
            if tryPull == args
                then mt "no" (exprs, node)
                else addEdge exprTry $ mkSCZ dims (Expression sN sE) tryPull
    | Just (Op dims (SCZ (Expression sn se)) args) <- I.lookup node exprs =
        let (fltdE, fltdArgs) = L.mapAccumR floatNeg exprs args
            (exprTry, tryPull) = L.mapAccumR pullNeg fltdE fltdArgs
            xchSign 0 (try:_) (arg:_) = try == arg
            xchSign idx (_:trys) (_:rest) = xchSign (idx - 1) trys rest
            xchSign a b c =
                error $
                "F.floatNeg.xchSign " ++
                show (a, b, c) ++ (pretty (exprs, node))
            (newE, newN) = rewrite se sn
            rewrite e0 head
                | Just (Op dims op args) <- I.lookup head e0 =
                    let (e1, newAs) = L.mapAccumR rewrite e0 args
                     in addEdge e1 $ Op dims op newAs
                | Just (RelElem idx _bnd _off) <- I.lookup head e0 =
                    if xchSign idx tryPull fltdArgs
                        then addEdge e0 $ Op Dim0 Neg [head]
                        else (e0, head)
                | True = (e0, head)
         in mt ("~~~scz~~~" ++ (show $ zip args tryPull)) $
            if tryPull == args
                then mt "no" (exprs, node)
                else addEdge exprTry $ mkSCZ dims (Expression newN newE) tryPull
    | Just (Op dims op args) <- I.lookup node exprs =
        let (newE, newArgs) = L.mapAccumR floatNeg exprs args
         in mt ("~~op~~" ++ (show $ zip args newArgs)) $
            if newArgs == args
                then (exprs, node)
                else addEdge newE $ Op dims op newArgs
    | otherwise = (exprs, node)

pullNeg exprs node
    | Just (Op _ Neg [arg]) <- I.lookup node exprs = mt "neg" (exprs, arg)
    | Just (Op dims op args) <- I.lookup node exprs
    , canPull op
    , (pulledE, pulledArgs) <- L.mapAccumR pullNeg exprs args =
        if L.or $ zipWith (==) args pulledArgs
            then mt ("not " ++ (show op)) (exprs, node) -- couldn't pull negate out of all args
            else mt ("pulled " ++ show (op, args, pulledArgs)) $
                 addEdge pulledE $ Op dims op pulledArgs
    | x <- I.lookup node exprs = mt ("@@@@@@@@" ++ show x) (exprs, node)

canPull RealPart = True
canPull ImagPart = True
canPull RealImag = True
canPull (Project _) = True
canPull (Inject _) = True
canPull (PFT _ _) = True
canPull (Transpose _) = True
canPull (Compound _) = True
canPull x = mt ("couldn't pull " ++ show x) False

{-


-}
factorNeg (exprs, node)
    | Just (Op Dim0 Sum args) <- mt "preSum" $ I.lookup node exprs
    , negArgs <- mapMaybe (M.negate exprs) args
    , length negArgs == length args =
        tf "factor Neg out of sum" $
        pushNegIntoConst $ factor' $ (sumE negArgs) exprs
    | Just (Op dims op args) <- I.lookup node exprs
    , (newExprs, newArgs) <- L.mapAccumR (curry factorNeg) exprs args
    , newArgs /= args = addEdge newExprs $ Op dims op newArgs
    | otherwise = (exprs, node)

{-

-}
pushNegIntoConst (exprs, node)
    | Just (Op Dim0 Neg [arg]) <- I.lookup node exprs
    , Just (Const Dim0 d) <- I.lookup arg exprs =
        addEdge exprs $ Const Dim0 (-d)
    | Just (Op Dim0 Neg [arg]) <- I.lookup node exprs
    , Just (Op Dim0 Prod args) <- I.lookup arg exprs
    , consts <- concatMap getConst $ map (flip I.lookup exprs) args
    , nonConsts <-
         map sourceNode $ filter (not . isConst . flip I.lookup exprs) args
    , not $ null consts =
        (prodE
             ((\e -> addEdge e $ Const Dim0 $ L.product $ (-1) : consts) :
              nonConsts))
            exprs
    | Just (Op dims op args) <- I.lookup node exprs
    , (newExprs, newArgs) <- L.mapAccumR (curry pushNegIntoConst) exprs args
    , newArgs /= args = addEdge newExprs $ Op dims op newArgs
    | otherwise = (exprs, node)

{-


-}
factor' (exprs, node)
    | Just (Op Dim0 Sum args) <- mt "preSum" $ I.lookup node exprs
    , any ((Nothing /=) . M.prod exprs) args =
        tf "factor prod out of sum" $
        let prodArgsOrSelf arg =
                case I.lookup arg exprs of
                    Just (Op _ Prod argArgs) -> argArgs
                    _ -> [arg]
            f [] = []
            f [[]] = []
            f argSets =
                case (ones, hasNot) of
                    ([], []) -> mostCommon' : had
                    _ ->
                        [ sumE $
                          notNull $ possibleConst ++ [hadProd] ++ possibleNot
                        ]
              where
                mostCommon' = sourceNode mostCommon
                mostCommon =
                    fst $
                    L.maximumBy (\x y -> compare (snd x) (snd y)) $
                    I.toList $
                    I.fromListWith (+) $ zip (concat argSets) (repeat 1)
                (ones, proper) = L.partition null argSets
                possibleConst =
                    case ones of
                        [] -> []
                        _ -> [fromRational $ fromIntegral $ length ones]
                (has, hasNot) = L.partition (mostCommon `L.elem`) proper
                possibleNot =
                    case f hasNot of
                        [] -> []
                        something -> [prodE something]
                had = f $ map (L.delete mostCommon) has
                hadProd =
                    case had of
                        [] -> mostCommon'
                        _ -> mostCommon' * prodE had
                notNull x =
                    if null x
                        then error $
                             "factor null" ++
                             show (ones, map (L.delete mostCommon) has, hasNot)
                        else x
         in tf "distribute prod inside sum" $
            prodE (f $ map prodArgsOrSelf args) exprs
    | Just (Op dims (SCZ (Expression sn se)) args) <-
         mt "preSCZ" $ I.lookup node exprs =
        let (sczE, sczN) = factor' (se, sn)
            offset =
                case dims of
                    Dim1 _ -> [0]
                    Dim2 _ -> [0, 0]
                    Dim3 _ -> [0, 0, 0]
                    _ -> error "factor SCZ dims"
            replacements =
                zipWith
                    (\(oldN, cs) newIdx -> (oldN, (cs, newIdx)))
                    (L.nub $ L.sort $ commonSummands sczN)
                    [(length args) ..]
            rewrite newE head
                | Just (_cs, newIdx) <- lookup head replacements =
                    tf "." $ addEdge newE (RelElem newIdx ZeroMargin offset)
                | Just (Op dims op args) <- I.lookup head sczE =
                    let (e1, newAs) = L.mapAccumR rewrite newE args
                     in tf ("rewrite op" ++ show op) $
                        addEdge e1 $ Op dims op newAs
                | Just x <- I.lookup head sczE = tf (show x) $ addEdge newE x
                | True = error "factor rewrite can't happen"
            (rewrittenE, rewrittenN) = rewrite I.empty sczN
            (newExprs, argsToAdd) =
                mt ("replacements " ++ show replacements) $
                L.mapAccumR addCommon exprs replacements
            usedIdxs = map fst $ L.nub $ L.sort $ relElems rewrittenE rewrittenN
            usedArgs =
                map snd $
                filter ((`elem` usedIdxs) . fst) $
                zip [0 ..] $ args ++ map snd argsToAdd
            (newSCZe, newSCZn) =
                let remap = I.fromList $ zip usedIdxs [0 ..]
                    rewrite newE head
                        | Just (RelElem idx _bdy offset) <-
                             I.lookup head rewrittenE =
                            case I.lookup idx remap of
                                Just newIdx ->
                                    tf "." $
                                    addEdge
                                        newE
                                        (RelElem newIdx ZeroMargin offset)
                                _ ->
                                    error $
                                    "factor newSCZ found idx we shouln't have " ++
                                    show (idx, remap)
                        | Just (Op dims op as) <- I.lookup head rewrittenE =
                            let (e1, nargs) = L.mapAccumR rewrite newE as
                             in tf (show op) $ addEdge e1 $ Op dims op nargs
                        | Just x <- I.lookup head rewrittenE =
                            tf (show x) $ addEdge newE x
                        | True = error "factor rewrite can't happen2"
                 in tf ("rewrite2" ++ show usedIdxs) $
                    rewrite I.empty rewrittenN
            addCommon es (_oldN, (cs, newIdx)) =
                case commonTop es cs of
                    Just (ec, en) -> (ec, (newIdx, en))
                    _ ->
                        error $
                        ("HF.factor.addCommon " ++) $
                        (show (commonSummands sczN) ++) $
                        ("\n" ++) $
                        (pretty (sczE, sczN) ++) $
                        (show (sczE, sczN) ++) $
                        ("\n" ++) $
                        (show es ++) $
                        ("\n" ++) $
                        (show cs ++) $
                        ("\n" ++) $ unlines $ map (curry pretty' es) cs
            commonSummands node
                | Just (Op _dims Sum summands) <- I.lookup node sczE
                , all (nodeIsRelElem sczE) summands =
                    let idxNs = relElemZeroOffsets sczE node
                        argMap = I.fromList $ zip [0 ..] args
                        toSum =
                            map
                                (\(i, _) ->
                                     I.findWithDefault
                                         (error "factor' commonSummands idx")
                                         i
                                         argMap)
                                idxNs
                     in [(node, toSum)]
                | Just (Op _ _op opArgs) <- I.lookup node sczE =
                    mt "=" $ concatMap commonSummands opArgs
                | otherwise = []
         in tf (unlines
                    [ "SCZ"
                    , pretty (rewrittenE, rewrittenN)
                    , pretty (sczE, sczN)
                    , show replacements
                    , show $ L.nub $ L.sort $ relElems rewrittenE rewrittenN
                    ]) $
            addEdge newExprs $
            mkSCZ
                dims
                (Expression newSCZn newSCZe)
                (mt (show $ usedArgs) usedArgs)
    | Just (Op dims op args) <- mt "preOp" $ I.lookup node exprs
    , (exprs1, newArgs) <- L.mapAccumR (curry factor') exprs args
    , args /= newArgs = tf "Op" $ addEdge exprs1 $ Op dims op newArgs
    | otherwise = tf "True" (exprs, node)

{-

FIXME : fails with

let ss = SSCrop [(32,95),(16,47)] [128,64]
let m0 =  meas (2,64,64,32) 0
let m1 =  meas (2,64,64,32) 1
factor $ czZip (+) (projSS ss (ft m1)) (projSS ss (ft m0))

factor $ czZip (+) (projSS ss (m1)) (projSS ss (m0))

-- FIXME add these as regression tests
let m0 =  meas (2,64,64,32) 0
let m1 =  meas (2,64,64,32) 1
factor $ czZip (+) ((ft m1)) ((ft m0)) -- succeeds
--
let ss = SSCrop [(32,95),(16,47)] [128,64]
let m1 = var2d (128,64) "x"
let m0 = var2d (128,64) "y"
factor $ czZip (+) (projSS ss (m1)) (projSS ss (m0))


FIXME : can we do better with constants?
FIXME : this doesn't optimize for fmas


FIXME:  This comment copied with code from HS:  if we ever want to use this, make sure it doesn't push sums into nonlinear SCZs and other nonlinear operations
-}
commonTop :: Internal -> [Node] -> Maybe (Internal, Node)
commonTop exprs summands =
    mt "preCommonTop" $
    let (exprs1, newSummands) =
            L.mapAccumR pushSum exprs $
            L.groupBy sameOp $ L.sort $ map (getOp . dS) summands
        pushSum ::
               Internal
            -> [(Maybe (OpId, Node), (Bool, Either Node Double, Node), Node)]
            -> (Internal, Node)
        pushSum _ [] = error "pushSum"
        pushSum e [(_, _, origNode)] = (e, origNode)
        pushSum e twoOrMore@((Just (op, _), _, _):_) =
            let (e1, newSum) =
                    case commonTop e $
                         map
                             (snd . ($ e))
                             (zipWith makeNeg twoOrMore $ map scaleBy twoOrMore) of
                        Just x -> x
                        _ ->
                            simplify' $
                            (sumE $
                             zipWith makeNeg twoOrMore $ map scaleBy twoOrMore)
                                e
                makeNeg (_, (True, _, _), _) c = negate c
                makeNeg (_, (False, _, _), _) c = c
                scaleBy ::
                       ( Maybe (OpId, Node)
                       , (Bool, Either Node Double, Node)
                       , Node)
                    -> (Internal -> (Internal, Node))
                scaleBy (Just (_, arg), (_, Left sn, _), _) =
                    sourceNode sn `scale` sourceNode arg
                scaleBy (Just (_, arg), (_, Right 1, _), _) = sourceNode arg
                scaleBy (Just (_, arg), (_, Right d, _), _) =
                    (mkConst Dim0 d) `scale` sourceNode arg
                scaleBy _ = error "commonTop.scaleBy impossible"
             in addEdgeD e1 op [newSum]
        -- FIXME Op (getDimE e1 newSum) op [newSum] is dangerous, because dimensions change
        pushSum _ _ = error "commonTop pushSum"
        sameOp (Just (op1, _), _, _) (Just (op2, _), _, _) = op1 == op2
        sameOp _ _ = False
    -- find the nodes which come from linear operations
        getOp :: ((a, b, Node), c) -> (Maybe (OpId, Node), (a, b, Node), c)
        getOp ((a, b, n), c)
            | Just (Op _ op [arg]) <- I.lookup n exprs
            , linearOp op = (Just (op, arg), (a, b, n), c)
            | otherwise = (Nothing, (a, b, n), c)
        dS :: Node -> ((Bool, Either Node Double, Node), Node)
        dS n = disassociateScale False n n
        disassociateScale ::
               Bool -> Node -> Node -> ((Bool, Either Node Double, Node), Node)
        disassociateScale isNeg origFactor factor =
            case I.lookup factor exprs of
                Just (Op _dims Neg [newFactor]) ->
                    disassociateScale (not isNeg) origFactor newFactor
          -- from earlier rules we know we cannot get zeros, and we only have one const @ end
                Just (Op _dims ScaleV [a, b]) ->
                    case I.lookup a exprs of
                        Just (Const Dim0 m) -> ((isNeg, Right m, b), origFactor)
                        _ -> ((isNeg, Left a, factor), origFactor)
                _ -> ((isNeg, Right 1, factor), origFactor)
     in if length summands /= length newSummands
            then mt ("Common " ++ show (newSummands, summands)) $
                 Just
                     (case newSummands of
                          [x] -> (exprs1, x)
                          _ -> (sumE $ map sourceNode newSummands) exprs1)
            else mt (unlines $
                     ("nonCommon " ++
                      show (newSummands, summands, map (getOp . dS) summands)) :
                     (map (pretty' . (exprs1, )) newSummands))
                     Nothing

{-

simplifySCZ (exprs,node)
  -- * remove unused inputs from SCZs expressions
  -- - SCZ( f r0 r2 )[x,y,z] -> SCZ( f r0 r1 )[x,z]
  | Just (Op dims (SCZ outD (Expression sczN sczE)) inputs) <- I.lookup node exprs
  , inputIdxs <- L.nub $ L.sort $ map fst $ relElems sczE sczN
  , length inputIdxs < length inputs
  = tf "pre-remove unused inputs" $
    let  remap = I.fromList $ zip inputIdxs [0..]
         -- map relElems to compressed subset, otherwise don't remap
         reMapVar (RelElem idx b o)
           | Just newIdx <- I.lookup (-idx-1) remap
           = RelElem newIdx b o
         reMapVar x = x

         (sczE',sczN') = tf "pre2-unused inputs" $ simpRewrite reMapVar $ tt "pre1-unused inputs" (sczE,sczN)
         inputs' = onlyUsed inputs
         onlyUsed = map snd . filter (\ (a,_b) -> a `elem` inputIdxs) . zip [0..]
    in tf "remove unused inputs" $ addEdge exprs $ case inputs' of
                        [] -> Const dims 0
                        _ -> let (sczNN,sczEE) = simplify' (sczE',sczN')
                             in sczEE `seq` Op dims (mkSCZ outD (Expression sczEE sczNN)) inputs'


  | True
  = tf "True" $ (exprs,node)

-}
linOpDepth exprs = lOD 0
  where
    lOD d n
        | Just (Op _ op args) <- I.lookup n exprs
        , linearOp op = L.maximum $ map (lOD (d + 1)) args
        | True = d

{-

Rewrite SCZs into a standard form (so we don't generate redundant code).
Since we can reorder the arguments of the SCZ, we can rename the |RelElem|s.
The normal form will the be the one with the smallest hash value,
and if values are the same, then the one with the smaller expression map.
-}
normalizeSCZ :: ExpressionEdge -> (Expression, [Node])
normalizeSCZ (Op _dims (SCZ (Expression sczN sczE)) args)
         -- map relElems to compressed subset, otherwise don't remap
 =
    let reMapVar remap (RelElem idx b o)
            | Just newIdx <- L.lookup (-idx - 1) remap = RelElem newIdx b o
        reMapVar _ x = x
        inputIdxs = L.nub $ L.sort $ map fst $ relElems sczE sczN
        idxInputPairs =
            filter (\(a, _b) -> a `elem` inputIdxs) $ zip [0 ..] args
        ((newN, newE), newArgs):_ =
            L.sort
                [ ( (\(e, n) -> (n, e)) $
                    recreate $
                    factor' $
                    simplify' $
                    simpRewrite
                        (reMapVar $ zip (map fst iip) [0 ..])
                        (sczE, sczN)
                  , map snd iip)
                | iip <- L.permutations idxInputPairs
                ]
     in (Expression newN newE, newArgs)
normalizeSCZ x = error $ "HF.normalizeSCZ not implemented: " ++ show x

{-


-}
normalizeSCZs (exprs0, node)
  -- * remove unused inputs from SCZs expressions and normalize
  -- - SCZ( f r0 r2 )[x,y,z] -> SCZ( f r1 r0 )[z,x]
    | Just (Op dims (SCZ (Expression sczN sczE)) oldInputs) <-
         I.lookup node exprs0
    , (exprs, inputs) <- L.mapAccumR (curry $ normalizeSCZs) exprs0 oldInputs
    , (newSCZE, newInputs) <-
         normalizeSCZ (mkSCZ dims (Expression sczN sczE) inputs) =
        trace
            ("normalsizeSCZ " ++ show (zip oldInputs inputs) ++ show newInputs) $
        addEdge exprs $ mkSCZ dims newSCZE newInputs
  -- * if no simplification was applied to this node, then simplify args
  -- - f x, f \in [+,*] -> f (simplify x)
    | Just (Op dims op args) <- I.lookup node exprs0
    -- and sort the args for |Sum|s and |Prod|s
     =
        trace ("normalize op " ++ show op) $
        let (e, simpArgs) =
                (if op `elem` [Sum, Prod]
                     then nodeSort
                     else id) $
                L.mapAccumR (curry $ normalizeSCZs) exprs0 args
         in addEdge e $ Op dims op simpArgs
normalizeSCZs (exprs, node) = (exprs, node) {-


-- find a linear combination of RelElems, and look for a common top of _linear_ operations
   on all of them and push the linear combination down into those linear operations
%\begin{code}
factorSCZ (exprs,node)
  -- * do the adjoint of common subexpression elimination for SCZs
  -- - Prod(x,Sum(y+z)) -> Sum(x*y,x*z)
  | Just (Op _dims (SCZ (Expression sczN sczE)) args) <- I.lookup node exprs
  ,
  ,
  = tf "distribute prod inside sum" $
    let  (sczE',sczN') = factorPoly (sczE,sczN)
         prodArgsOrSelf arg = case I.lookup arg exprs of
                                Just (Op _ Prod argArgs) -> argArgs
                                _ -> [arg]

         -- find sums of nodes
         sums arg
           | Just (Op _dims Sum args) <- I.lookup node sczE'
           = [(arg, L.partition nodeIsRelElem args)]


           | Just (Op _dims op args) <- I.lookup node sczE'
           = concatMap sums args

           | True
           = []

         sumsOfRelElems = sums sczN

         -- apply adjoint CSE to arguments which appear as sums of



    in tf "distribute prod inside sum" $ prodE (f $ map prodArgsOrSelf args) exprs
%\end{code}

containsLinearCombRelElems exprs head
  | Just (Op _dims Sum args) <- I.lookup head exprs
  | Just (exprs',(linCombo,notLinCombo))
      <- containsLinearCombRelElems (containsLinearCombRelElems exprs) args

Test SCZ rewriting needed after |mkAccumulators|.
Use two SCZs which are the same up to permuting arguments.
FIXME:  move to tests
sczOp1@(Op _ (SCZ _ sczOp1E) sczOp1Args) = Op (Dim2 (64,64)) (mkSCZ {convDims = Dim2 (64,64), convExtentconvExpr = Expression 411319616 (I.fromList [(-1067038951,Op Dim0 Sum [-996545584,-1050201013,-1036241877,-1008759828,-1062415257,-1021410295]),(-1062415257,Op Dim0 Prod [479002315,479002673,1305018176]),(-1050201013,Op Dim0 Prod [479001778,479002852,1305018176]),(-1036241877,Op Dim0 Prod [479001957,479003031,1305018176]),(-1021410295,Op Dim0 Prod [479002494,479003210,1305018176]),(-1008759828,Op Dim0 Prod [479002136,479003389,1305018176]),(-996545584,Op Dim0 Prod [479001599,479003568,1305018176]),(411319616,Op (Dim2 (64,64)) Sum [-1036241877,-1008759828]),(479001599,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002315,RelElem {reArray = 4, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002494,RelElem {reArray = 5, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002673,RelElem {reArray = 6, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002852,RelElem {reArray = 7, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003031,RelElem {reArray = 8, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003210,RelElem {reArray = 9, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003389,RelElem {reArray = 10, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003568,RelElem {reArray = 11, reBoundary = ZeroMargin, reIdx = [0,0]}),(1305018176,Const {unConstDims = Dim0, unConst = 2.0})])}) [-865672852,-865632261,781956429,781997020,1781665163,1781705754,-41332422,-781117466,1382551098,-1024330906,-537904026,670609338]
sczOp2@(Op _ (SCZ _ sczOp2E) sczOp2Args) = Op (Dim2 (64,64)) (mkSCZ {convDims = Dim2 (64,64), convExtentconvExpr = Expression 411319616 (I.fromList [(-1067038951,Op Dim0 Sum [-996545584,-1050201013,-1036241877,-1008759828,-1062415257,-1021410295]),(-1062415257,Op Dim0 Prod [479002315,479002673,1305018176]),(-1050201013,Op Dim0 Prod [479001778,479002852,1305018176]),(-1036241877,Op Dim0 Prod [479001957,479003031,1305018176]),(-1021410295,Op Dim0 Prod [479002494,479003210,1305018176]),(-1008759828,Op Dim0 Prod [479002136,479003389,1305018176]),(-996545584,Op Dim0 Prod [479001599,479003568,1305018176]),(411319616,Op (Dim2 (64,64)) Sum [-1036241877,-1008759828]),(479001599,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 4, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002315,RelElem {reArray = 5, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002494,RelElem {reArray = 6, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002673,RelElem {reArray = 7, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002852,RelElem {reArray = 8, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003031,RelElem {reArray = 9, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003210,RelElem {reArray = 10, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003389,RelElem {reArray = 11, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003568,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(1305018176,Const {unConstDims = Dim0, unConst = 2.0})])}) [670609338,-865672852,-865632261,781956429,781997020,1781665163,1781705754,-41332422,-781117466,1382551098,-1024330906,-537904026]
t_normalizeSCZ = normalizeSCZ sczOp1 == normalizeSCZ sczOp2

sczOps = [Op (Dim2 (64,64)) (mkSCZ {convDims = Dim2 (64,64), convExtentconvExpr = Expression (-2021108393) (I.fromList [(-2021108393,Op Dim0 Sum [1157171367,-716182117]),(-716182117,Op Dim0 Prod [479001778,479002136,1850237956]),(479001599,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(1157171367,Op Dim0 Prod [479001599,479001957]),(1850237956,Const {unConstDims = Dim0, unConst = -1.0})])}) [781956429,781997020,1088390919,408848608],Op (Dim2 (64,64)) (mkSCZ {convDims = Dim2 (64,64), convExtentconvExpr = Expression 195811545 (I.fromList [(195811545,Op Dim0 Sum [1170694280,1157607590]),(479001599,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(1157607590,Op Dim0 Prod [479001778,479001957]),(1170694280,Op Dim0 Prod [479001599,479002136])])}) [781956429,781997020,1088390919,408848608],Op (Dim2 (64,64)) (mkSCZ {convDims = Dim2 (64,64), convExtentconvExpr = Expression (-2021108393) (I.fromList [(-2021108393,Op Dim0 Sum [1157171367,-716182117]),(-716182117,Op Dim0 Prod [479001778,479002136,1850237956]),(479001599,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(1157171367,Op Dim0 Prod [479001599,479001957]),(1850237956,Const {unConstDims = Dim0, unConst = -1.0})])}) [-865672852,-865632261,1088390919,408848608],Op (Dim2 (64,64)) (mkSCZ {convDims = Dim2 (64,64), convExtentconvExpr = Expression 195811545 (I.fromList [(195811545,Op Dim0 Sum [1170694280,1157607590]),(479001599,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(1157607590,Op Dim0 Prod [479001778,479001957]),(1170694280,Op Dim0 Prod [479001599,479002136])])}) [-865672852,-865632261,1088390919,408848608],Op (Dim2 (64,64)) (mkSCZ {convDims = Dim2 (64,64), convExtentconvExpr = Expression (-2021108393) (I.fromList [(-2021108393,Op Dim0 Sum [1157171367,-716182117]),(-716182117,Op Dim0 Prod [479001778,479002136,1850237956]),(479001599,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(1157171367,Op Dim0 Prod [479001599,479001957]),(1850237956,Const {unConstDims = Dim0, unConst = -1.0})])}) [1781665163,1781705754,1088390919,408848608],Op (Dim2 (64,64)) (mkSCZ {convDims = Dim2 (64,64), convExtentconvExpr = Expression 195811545 (I.fromList [(195811545,Op Dim0 Sum [1170694280,1157607590]),(479001599,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(1157607590,Op Dim0 Prod [479001778,479001957]),(1170694280,Op Dim0 Prod [479001599,479002136])])}) [1781665163,1781705754,1088390919,408848608],Op (Dim2 (64,64)) (mkSCZ {convDims = Dim2 (64,64), convExtentconvExpr = Expression 1730330861 (I.fromList [(-1600683919,Op (Dim2 (64,64)) Sum [-1009196051,352210083]),(-1273885814,Op Dim0 Prod [-1160138082,479003389,1305018176]),(-1160138082,Op Dim0 Neg [479001957]),(-1159727635,Op Dim0 Neg [479002136]),(-1050637236,Op Dim0 Prod [479001599,479002852,1305018176]),(-1021846518,Op Dim0 Prod [479002315,479003210,1305018176]),(-1009196051,Op Dim0 Prod [479001957,479003389,1305018176]),(-797100059,Const {unConstDims = Dim0, unConst = -2.0}),(-391950002,Op Dim0 Sum [-1050637236,391906376,-1009196051,352210083,-1021846518,326036703]),(326036703,Op Dim0 Prod [479002494,479002673,-797100059]),(352210083,Op Dim0 Prod [479002136,479003031,-797100059]),(391906376,Op Dim0 Prod [479001778,479003568,-797100059]),(479001599,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002315,RelElem {reArray = 4, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002494,RelElem {reArray = 5, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002673,RelElem {reArray = 6, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002852,RelElem {reArray = 7, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003031,RelElem {reArray = 8, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003210,RelElem {reArray = 9, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003389,RelElem {reArray = 10, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003568,RelElem {reArray = 11, reBoundary = ZeroMargin, reIdx = [0,0]}),(1087343436,Op Dim0 Prod [-1159727635,479003031,-797100059]),(1305018176,Const {unConstDims = Dim0, unConst = 2.0}),(1730330861,Op (Dim2 (64,64)) Sum [-1273885814,1087343436])])}) [-865672852,-865632261,781956429,781997020,1781665163,1781705754,-1061907030,-1634246506,783695530,1615437334,-588963050,2008377898],Op (Dim2 (64,64)) (mkSCZ {convDims = Dim2 (64,64), convExtentconvExpr = Expression (-573158724) (I.fromList [(-1234151008,Op Dim0 Prod [-1158906741,479002673,-797100059]),(-1159317188,Op Dim0 Neg [479002315]),(-1158906741,Op Dim0 Neg [479002494]),(-1156033612,Op Dim0 Neg [479003747]),(-1050637236,Op Dim0 Prod [479001599,479002852,1305018176]),(-1021846518,Op Dim0 Prod [479002315,479003210,1305018176]),(-1009196051,Op Dim0 Prod [479001957,479003389,1305018176]),(-797100059,Const {unConstDims = Dim0, unConst = -2.0}),(-573158724,Op (Dim2 (64,64)) Sum [713109951,-1234151008,-1156033612]),(-391950002,Op Dim0 Sum [-1050637236,391906376,-1009196051,352210083,-1021846518,326036703]),(326036703,Op Dim0 Prod [479002494,479002673,-797100059]),(352210083,Op Dim0 Prod [479002136,479003031,-797100059]),(391906376,Op Dim0 Prod [479001778,479003568,-797100059]),(479001599,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002315,RelElem {reArray = 4, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002494,RelElem {reArray = 5, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002673,RelElem {reArray = 6, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002852,RelElem {reArray = 7, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003031,RelElem {reArray = 8, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003210,RelElem {reArray = 9, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003389,RelElem {reArray = 10, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003568,RelElem {reArray = 11, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003747,RelElem {reArray = 12, reBoundary = ZeroMargin, reIdx = [0,0]}),(713109951,Op Dim0 Prod [-1159317188,479003210,1305018176]),(1305018176,Const {unConstDims = Dim0, unConst = 2.0}),(1744983741,Op (Dim2 (64,64)) Sum [-1021846518,326036703,479003747])])}) [-865672852,-865632261,781956429,781997020,1781665163,1781705754,-1061907030,-1634246506,783695530,1615437334,-588963050,2008377898,953692176],Op (Dim2 (64,64)) (mkSCZ {convDims = Dim2 (64,64), convExtentconvExpr = Expression (-552632900) (I.fromList [(-1300931640,Op Dim0 Prod [-1160138082,479003031,1305018176]),(-1160138082,Op Dim0 Neg [479001957]),(-1159727635,Op Dim0 Neg [479002136]),(-1067038951,Op Dim0 Sum [-996545584,-1050201013,-1036241877,-1008759828,-1062415257,-1021410295]),(-1062415257,Op Dim0 Prod [479002315,479002673,1305018176]),(-1050201013,Op Dim0 Prod [479001778,479002852,1305018176]),(-1036241877,Op Dim0 Prod [479001957,479003031,1305018176]),(-1021410295,Op Dim0 Prod [479002494,479003210,1305018176]),(-1008759828,Op Dim0 Prod [479002136,479003389,1305018176]),(-996545584,Op Dim0 Prod [479001599,479003568,1305018176]),(-552632900,Op (Dim2 (64,64)) Sum [-1300931640,-273626475]),(-273626475,Op Dim0 Prod [-1159727635,479003389,1305018176]),(411319616,Op (Dim2 (64,64)) Sum [-1036241877,-1008759828]),(479001599,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002315,RelElem {reArray = 4, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002494,RelElem {reArray = 5, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002673,RelElem {reArray = 6, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002852,RelElem {reArray = 7, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003031,RelElem {reArray = 8, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003210,RelElem {reArray = 9, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003389,RelElem {reArray = 10, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003568,RelElem {reArray = 11, reBoundary = ZeroMargin, reIdx = [0,0]}),(1305018176,Const {unConstDims = Dim0, unConst = 2.0})])}) [-865672852,-865632261,781956429,781997020,1781665163,1781705754,-1061907030,-1634246506,783695530,1615437334,-588963050,2008377898],Op (Dim2 (64,64)) (mkSCZ {convDims = Dim2 (64,64), convExtentconvExpr = Expression (-1624720891) (I.fromList [(-1624720891,Op (Dim2 (64,64)) Sum [672541212,1713369290,-1156033612]),(-1159317188,Op Dim0 Neg [479002315]),(-1158906741,Op Dim0 Neg [479002494]),(-1156033612,Op Dim0 Neg [479003747]),(-1067038951,Op Dim0 Sum [-996545584,-1050201013,-1036241877,-1008759828,-1062415257,-1021410295]),(-1062415257,Op Dim0 Prod [479002315,479002673,1305018176]),(-1050201013,Op Dim0 Prod [479001778,479002852,1305018176]),(-1036241877,Op Dim0 Prod [479001957,479003031,1305018176]),(-1021410295,Op Dim0 Prod [479002494,479003210,1305018176]),(-1008759828,Op Dim0 Prod [479002136,479003389,1305018176]),(-996545584,Op Dim0 Prod [479001599,479003568,1305018176]),(479001599,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002315,RelElem {reArray = 4, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002494,RelElem {reArray = 5, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002673,RelElem {reArray = 6, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002852,RelElem {reArray = 7, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003031,RelElem {reArray = 8, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003210,RelElem {reArray = 9, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003389,RelElem {reArray = 10, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003568,RelElem {reArray = 11, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003747,RelElem {reArray = 12, reBoundary = ZeroMargin, reIdx = [0,0]}),(672541212,Op Dim0 Prod [-1159317188,479002673,1305018176]),(693421574,Op (Dim2 (64,64)) Sum [-1062415257,-1021410295,479003747]),(1305018176,Const {unConstDims = Dim0, unConst = 2.0}),(1713369290,Op Dim0 Prod [-1158906741,479003210,1305018176])])}) [-865672852,-865632261,781956429,781997020,1781665163,1781705754,-1061907030,-1634246506,783695530,1615437334,-588963050,2008377898,356256920],Op (Dim2 (64,64)) (mkSCZ {convDims = Dim2 (64,64), convExtentconvExpr = Expression (-181821846) (I.fromList [(-1160958976,Op Dim0 Neg [479001599]),(-1160548529,Op Dim0 Neg [479001778]),(-1156033612,Op Dim0 Neg [479003747]),(-1067038951,Op Dim0 Sum [-996545584,-1050201013,-1036241877,-1008759828,-1062415257,-1021410295]),(-1062415257,Op Dim0 Prod [479002315,479002673,1305018176]),(-1050201013,Op Dim0 Prod [479001778,479002852,1305018176]),(-1036241877,Op Dim0 Prod [479001957,479003031,1305018176]),(-1021410295,Op Dim0 Prod [479002494,479003210,1305018176]),(-1008759828,Op Dim0 Prod [479002136,479003389,1305018176]),(-996545584,Op Dim0 Prod [479001599,479003568,1305018176]),(-181821846,Op (Dim2 (64,64)) Sum [1034085717,1980253404,-1156033612]),(54664299,Op (Dim2 (64,64)) Sum [-996545584,-1050201013,479003747]),(479001599,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002315,RelElem {reArray = 4, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002494,RelElem {reArray = 5, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002673,RelElem {reArray = 6, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002852,RelElem {reArray = 7, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003031,RelElem {reArray = 8, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003210,RelElem {reArray = 9, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003389,RelElem {reArray = 10, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003568,RelElem {reArray = 11, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003747,RelElem {reArray = 12, reBoundary = ZeroMargin, reIdx = [0,0]}),(1034085717,Op Dim0 Prod [-1160958976,479003568,1305018176]),(1305018176,Const {unConstDims = Dim0, unConst = 2.0}),(1980253404,Op Dim0 Prod [-1160548529,479002852,1305018176])])}) [-865672852,-865632261,781956429,781997020,1781665163,1781705754,-1061907030,-1634246506,783695530,1615437334,-588963050,2008377898,929756224],Op (Dim2 (64,64)) (mkSCZ {convDims = Dim2 (64,64), convExtentconvExpr = Expression 899616887 (I.fromList [(-1160958976,Op Dim0 Neg [479001599]),(-1160548529,Op Dim0 Neg [479001778]),(-1156033612,Op Dim0 Neg [479003747]),(-1050637236,Op Dim0 Prod [479001599,479002852,1305018176]),(-1021846518,Op Dim0 Prod [479002315,479003210,1305018176]),(-1009196051,Op Dim0 Prod [479001957,479003389,1305018176]),(-872606503,Op Dim0 Prod [-1160548529,479003568,-797100059]),(-797100059,Const {unConstDims = Dim0, unConst = -2.0}),(-391950002,Op Dim0 Sum [-1050637236,391906376,-1009196051,352210083,-1021846518,326036703]),(326036703,Op Dim0 Prod [479002494,479002673,-797100059]),(352210083,Op Dim0 Prod [479002136,479003031,-797100059]),(391906376,Op Dim0 Prod [479001778,479003568,-797100059]),(479001599,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002315,RelElem {reArray = 4, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002494,RelElem {reArray = 5, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002673,RelElem {reArray = 6, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002852,RelElem {reArray = 7, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003031,RelElem {reArray = 8, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003210,RelElem {reArray = 9, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003389,RelElem {reArray = 10, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003568,RelElem {reArray = 11, reBoundary = ZeroMargin, reIdx = [0,0]}),(479003747,RelElem {reArray = 12, reBoundary = ZeroMargin, reIdx = [0,0]}),(899616887,Op (Dim2 (64,64)) Sum [979994065,-872606503,-1156033612]),(979994065,Op Dim0 Prod [-1160958976,479002852,1305018176]),(1136103032,Op (Dim2 (64,64)) Sum [-1050637236,391906376,479003747]),(1305018176,Const {unConstDims = Dim0, unConst = 2.0})])}) [-865672852,-865632261,781956429,781997020,1781665163,1781705754,-1061907030,-1634246506,783695530,1615437334,-588963050,2008377898,-1727488056]]
scz12Ops = map (\(i,op@(Op _ (mkSCZ _ e) _)) -> ( normalizeSCZ op,i,e)) $ zip [0..] sczOps
scz12OpsReducedTo6 = putStrLn $ unlines $ map (\((e,args),i,f) -> pretty e ++ show (args,i) ++ pretty f) $ L.nubBy (\((e,_),_,_) ((f,_),_,_) -> e==f) $ L.sortBy (\((e,_),_,_) ((f,_),_,_) -> compare e f) scz12Ops
-}
