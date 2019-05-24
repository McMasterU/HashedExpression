{-
(c) 2010-2014 Christopher Kumar Anand, Jessica LM Pavlin

Simplify Expressions.
We don't know if these rules terminate, and we don't know if they are confluent.
If they aren't we will waste a lot of time doing and undoing or computing inefficient expressions,
but correctness should not suffer.
But we also don't have a proof of correctness (equivalence of expressions).
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module HashedSimplify
    ( simplify'
    , simplify''
    , simpRewrite
    , simpRewrite''
    , xMkReal
    , xMkImag
    , mergeScaledConstsBy
    , HashedExpression.containsDifferential
    , isDeepZero
    , disassociateScale
    , constExpr
    , constExpr'
    , simplifyE
    , mkSCZ
    , simp1
    , applyOne
    , mkConst -- for testing
    ) where

import HashedExpression
import HashedInstances ()

--import HashedComplexInstances   --this might be helpful for some complex things which aren't working, but causes overlapping instances
import HashedConstruct
import HashedMatch (o)
import qualified HashedMatch as M
import WithHoles

--import HashedTransformable
--import Data.ByteString (ByteString)
--import qualified Data.ByteString.Char8 as C
import Data.IntMap (IntMap)
import qualified Data.IntMap as I

import Control.DeepSeq

--import Data.Map (Map)
--import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe (catMaybes, fromJust)
import Debug.Trace

{-

-}
tt description en =
    if skipDebug
        then en
        else case hasCycles en of
                 [] -> trace description $ en `seq` trace (pretty en) en
                 x ->
                     error $
                     "HS.simplify found cycle at " ++
                     description ++ " " ++ show x ++ " " ++ show en

ttall description en =
    case hasCycles en of
        [] -> trace description $ en `seq` trace (pretty en) en
        x ->
            error $
            "HS.simplify found cycle at " ++
            description ++ " " ++ show x ++ " " ++ show en

{-

We can define the right version of mkSCZ now that we have simplify.
-}
mkSCZ :: Dims -> Expression -> [Node] -> ExpressionEdge
mkSCZ dims expr args =
    let (Expression n e) = expr
        (e', n') = simplify' (e, n)
     in mkSCZ' dims (Expression n' e') args

{-

Simplify rules which can be handled by WithHoles:
\begin{itemize}
\item These rules only work on simple cases.  For example $x+0$, but not $x+y+0$
\item Some rules simplify when making an instance of the expression, but these rules are still necessary.  For example $e^{x-x}$ needs a rule for $e^0$.
\end{itemize}
-}
simp1 :: [(GuardedPattern, WithHoles)]
simp1 =
    [ x *. (y *. z) |.~~> (x * y) *. z
    , 1 * x |.~~> x --added these two TB 28/05/2015
    , x * 1 |.~~> x
    , 0 * x |.~~> 0
    , x * 0 |.~~> 0
    , 0 *. x |.~~> 0
    , x *. 0 |.~~> 0
    , 1 *. x |.~~> x
    , x + 0 |.~~> x -- added these two TB 01/06/2015
    , 0 + x |.~~> x
    , xRe (x +: y) |.~~> x
    , xIm (x +: y) |.~~> y
    , ft (invFt z) |.~~> z --FIXME  make sure FT scaling is consistent
    , invFt (ft z) |.~~> z
    , ft (0 +: 0) |.~~> (0 +: 0)
    , invFt (0 +: 0) |.~~> (0 +: 0)
    , (x +: y) + (u +: v) |.~~> (x + u) +: (y + v)
           -- FIXME  this won't match inside bigger sum, make part of Sum normalization
    , s *. (x +: y) |.~~> (s *. x) +: (s *. y) -- does not work for ScalarC, only vectorC; it's also in HashedComplexInstances
    , (x +: y) * (z +: w) |.~~> (x * z - y * w) +: (x * w + y * z)
    , exp 0 |.~~> 1 --exp(0) simplifies with the instance, but this rule is necessary for exp(x-x)
        --,0 / x |.~~> 0   FIXME we can only do this if we can prove x /= 0
    , x <.> 0 |.~~> 0
    , 0 <.> x |.~~> 0
    , (s *. x) <.> y |.~~> s * (x <.> y) -- TB,CD,RF: *. --> * (FIX) 27/05/2015.
    , x <.> (s *. y) |.~~> s * (x <.> y) -- TB,CD,RF: *. --> * (FIX) 27/05/2015.
    , x <.> y |.
      (WithHoles.containsDifferential y &&. WithHoles.containsDifferential x) ~~>
      error "HS.simp1 dx <.> dx"
    , x <.> y |. WithHoles.containsDifferential y ~~> y <.> x
    , x * (y + z) |.~~> (x * y + x * z)
    , (y + z) * x |.~~> (x * y + x * z)
    , x *. (y + z) |.~~> (x *. y + x *. z)
    , (x <.> (y + z)) |.~~> ((x <.> y) + (x <.> z))
    , ((y + z) <.> x) |.~~> ((x <.> y) + (x <.> z))
    ]

{-

-}
simplifyE dbg (Expression n e) =
    let (e', n') = simplify' (e, n)
     in mt (dbg ++ " " ++ pretty (e, n) ++ pretty (e', n')) $ Expression n' e'

simplify' :: (Internal, Node) -> (Internal, Node)
simplify' (e, n) = e `deepseq` sub 1000 n $ simplify'' (e, n)
  where
    sub 0 _oldN (e, n) =
        error $
        "simplify' ran out of iterations without reaching fixed point " ++
        pretty (Expression n e)
    sub iter oldN (e, n) =
        if n == oldN {- mt ("sub "++show iter) $ -}
            then (e, n) {- mt ("simplify'ed "++pretty (e,n))-}
            else sub (iter - 1) n $ simplify'' (e, n)

simpRewrite reWrite (e, n) --   simpRewrite' reWrite $ simpRewrite' negRelIdx (e,n)
 =
    let a = tt "negRelIdx" $ simpRewrite' negRelIdx $ tt "inputRelIdx" (e, n)
        b = tt "posRelIdx" $ simpRewrite' reWrite a
     in b

negRelIdx (RelElem idx b o)
    | idx < 0 = RelElem idx b o
    | idx >= 0 = RelElem (-idx - 1) b o
negRelIdx x = x

simpRewrite' reWrite (e, n) = sub 1000 n $ simpRewrite'' reWrite (e, n)
  where
    sub 0 _oldN (e, n) =
        error $
        "simpRewrite' ran out of iterations without reaching fixed point " ++
        pretty (Expression n e)
    sub iter oldN (e, n) =
        if n == oldN
            then tt ("simpRewrite'ed " ++ show (n) ++ "\n:::") (e, n) {-,e-}
            else let (e', n') = simpRewrite'' reWrite (e, n)
                  in tt ("sub recurse " ++ show (n', n, e) ++ "\n::") $
                     sub (iter - 1) n (e', n')

simplify'' (e, n) = (simpRewrite'' id) (e, n)

{-

Only reachable nodes are rewritten.
-}
simpRewrite'' reWrite (exprs, node)
  -- check for edge which should be rewritten
    | Just edge <- I.lookup node exprs
    , new <- reWrite edge
    , new /= edge =
        tt ("reWrite " ++ show new ++ "<-" ++ show edge) $ addEdge exprs new
  -- apply simple rewrites
    | Just ei <- applyOne (exprs, node) simp1 = mt "simp1 has been used" ei
  -- * sum with one summand collapses
  -- - Sum(x) -> x
    | Just [n] <- M.sum exprs node = tt "one sum" $ n exprs
  -- * product with one factor collapses
  -- - Prod(x) -> x
    | Just [n] <- M.prod exprs node = tt "one prod" $ n exprs
  {--}
  -- * if a product contains any products, collapse them into one product
  -- - Prod(x,Prod(y,z)) -> Prod(x,y,z)
    | Just (Op Dim0 Prod inputs) <- I.lookup node exprs
    , List.or $ map ((Nothing /=) . M.prod exprs) inputs =
        let collapsed = concatMap (collapse exprs Prod) inputs
            (exprs', sorted) = nodeSort (exprs, collapsed)
         in tt "collapse prods" $ addEdge exprs' $ Op Dim0 Prod sorted
  {--}
  -- * if a product contains a zero, collapse it
  -- - Prod(x,0,y) -> 0
    | Just (Op Dim0 Prod inputs) <- I.lookup node exprs
    , 0 <
          length
              (filter ((Just (Const Dim0 0) ==) . flip I.lookup exprs) inputs) =
        tt "prod ..0.." $ addEdge exprs (Const Dim0 0)
  -- FIXME: duplicate?
  -- * remove product containing a zero
  -- - Prod(x,y,0) -> 0
    | Just (Op Dim0 Prod inputs) <- I.lookup node exprs
    , [] /= filter (isDeepZero exprs) inputs =
        tt "prod ..0.." $ 0 exprs -- addEdge exprs $ Const Dim0 0
  -- * if a product contains two or more constants, collapse them into one constant
  -- - Prod(s1,s2,x) -> let s3 = s1 * s2 in s3 * x
    | Just (Op Dim0 Prod inputs) <- I.lookup node exprs
    , 1 < length (filter ((Nothing /=) . M.const exprs) inputs) =
        tt "pre-2 consts in prod" $
        let (consts, nonconsts) =
                List.partition ((Nothing /=) . M.const exprs) inputs
            newC =
                mkConst Dim0 $
                product $
                map
                    ((\x ->
                          case x of
                              (Just (c, _)) -> c
                              _ -> error "spm") .
                     M.const exprs)
                    consts
         in tt "2 consts in prod" $
            (prodE (newC : (map sourceNode nonconsts))) exprs
  {--}
  -- * if a product contains a constant 1 eliminate it
  -- - Prod(1,x,y) -> Prod(x,y)
    | Just (Op Dim0 Prod inputs) <- I.lookup node exprs
    , 0 <
          length
              (filter ((Just (Const Dim0 1) ==) . flip I.lookup exprs) inputs) =
        let (ones, nonones) =
                List.partition
                    ((Just (Const Dim0 1) ==) . flip I.lookup exprs)
                    inputs
         in tt ("prod consts " ++
                concatMap (pretty . (exprs, )) ones ++
                " not " ++ concatMap (pretty . (exprs, )) nonones) $
            (prodE (map sourceNode nonones)) exprs
  --
  -- * distribute product inside sum
  -- - Prod(x,Sum(y,z)) -> Sum(x*y,x*z)
    | Just (Op _dims Prod args) <- I.lookup node exprs
    , List.or $ map ((Nothing /=) . M.sum exprs) args =
        tt "pre-distribute prod" $
        let isSum arg =
                case I.lookup arg exprs of
                    Just (Op _ Sum _) -> True
                    _ -> False
            getArgs arg =
                case I.lookup arg exprs of
                    Just (Op _ Sum sArgs) -> sArgs
                    _ -> error "simplify' distribute product"
            (sums, notSums) = List.partition isSum args
            lists = cartProd $ map getArgs sums
         in tt "distribute prod inside sum" $
            (sumE
                 [ prodE (map sourceNode $ nodeSort' exprs $ l ++ notSums)
                 | l <- lists
                 ])
                exprs
  --
  -- * sum with no summands
  -- - Sum() -> error
    | Just [] <- M.sum exprs node = error $ "simplify found empty sum"
  {--}
  --this is also in simp1
  -- * complex sum becomes combination of real sums
  -- - (x +: y) + (z +: w) -> ((x + z) +: (y + w))
    | Just (Op _ Sum inputs) <- I.lookup node exprs
    , reImInputs <- map (M.reIm exprs) inputs
    , 1 < (length $ filter (/= Nothing) reImInputs) =
        tt "pre-complex sum" $
        let inputPair = zip reImInputs inputs {-mt (unlines $ map (curry pretty exprs) inputs) $-}
            (fromReImConstructor, others) =
                List.partition ((/= Nothing) . fst) inputPair
            (reals, imags) =
                unzip $
                map
                    (\(x, _) ->
                         case x of
                             Just u -> u
                             _ -> error "simp reim")
                    fromReImConstructor
            [reSum, imSum] {-mt (show $ length reals)
                 $-}
             = map sumE [reals, imags]
         in tt (("C sum" ++) $ show $ map snd others) $
            (sumE ((reSum +: imSum) : (map (sourceNode . snd) others))) exprs
  -- * move Re Im inside projections and injections and sums
  -- - Re ( Proj ( x ) ) -> Proj ( Re ( x ) )
    | Just (ss, x) <- (M.xRe `o` M.proj) exprs node =
        tt ("Re . proj") $ (projSS ss $ xRe x) exprs
  -- - Im ( Proj ( x ) ) -> Proj ( Im ( x ) )
    | Just (ss, x) <- (M.xIm `o` M.proj) exprs node = (projSS ss $ xIm x) exprs
  -- - Re ( Inject ( x ) ) -> Inject ( Re ( x ) )
    | Just (ss, x) <- (M.xRe `o` M.inject) exprs node =
        (injectSS ss $ xRe x) exprs
  -- - Im ( Inject ( x ) ) -> Inject ( Im ( x ) )
    | Just (ss, x) <- (M.xIm `o` M.inject) exprs node =
        (injectSS ss $ xIm x) exprs
  -- - Re ( ( x +: y ) + ( z +: w ) ) -> Re ( x +: y ) + Re ( z +: w )
    | Just inputs <- (M.xRe `o` M.sum) exprs node =
        (sumE $ map xRe inputs) exprs
  -- - Im ( ( x +: y ) + ( z +: w ) ) -> Im ( x +: y ) + Im ( z +: w )
    | Just inputs <- (M.xIm `o` M.sum) exprs node =
        (sumE $ map xIm inputs) exprs
  --
  -- * pull scale outside any linear operation
  -- - f \in [Neg,RealPart,ImagPart,FT _, PFT _ _]
  -- - f (s *. x) -> s *. f x
    | Just (Op dims op [arg]) <- I.lookup node exprs
    , linearOp op
    , Just (Op _ ScaleV [s, v]) <- I.lookup arg exprs =
        let (e1, opV) = addEdge exprs (Op dims op [v])
         in tt "scale outside linear" $ addEdge e1 $ Op dims ScaleV [s, opV]
  -- * move scale inside sum
  -- - s *. Sum(x,y,z) -> Sum(s *. x, s *. y, s *. z)
    | Just (s, n) <- M.scale exprs node
    , Just inputs <- M.sum exprs (snd $ n exprs) =
        tt "scale inside sum" $ (sumE $ map (scale s) inputs) exprs
  --
  -- this rule is both in HashedComplexInstances and in simp
  -- * move scale inside +:
  -- - s *. ( x +: y ) -> ( (s *. x) +: (s *. y ) )
    | Just (s, n) <- M.scale exprs node
    , Just (re, im) <- M.reIm exprs (snd $ n exprs) =
        ((s `scale` re) +: (s `scale` im)) exprs
  --}
  -- * if a sum contains any sums, collapse them into one sum
  -- - Sum(x,Sum(y,z)) -> Sum(x,y,z)
    | Just (Op dims Sum inputs) <- I.lookup node exprs
    , List.or $ map ((Nothing /=) . M.sum exprs) inputs =
        tt "pre-collapse sum" $
        let collapsed = concatMap (collapse exprs Sum) inputs
            (exprs', sorted) = nodeSort $ (exprs, collapsed)
         in tt "collapse sum" $ addEdge exprs' $ Op dims Sum sorted
 --
  -- * if a sum contains two or more constants, collapse them into one constant
  -- - Sum(s1,s2,x) -> let s3 = s1 + s2 in Sum(s3,x)
    | Just (Op _dims Sum inputs) <- I.lookup node exprs
    , 1 < length (filter ((Nothing /=) . M.const exprs) inputs) =
        tt "pre-2 consts in sum" $
        let (consts, nonconsts) =
                List.partition ((Nothing /=) . M.const exprs) inputs
            newC =
                mkConst Dim0 $
                sum $
                map
                    ((\x ->
                          case x of
                              (Just (c, _)) -> c
                              _ -> error "spm2") .
                     M.const exprs)
                    consts
         in (sumE (newC : (map sourceNode nonconsts))) exprs
  {--}
  -- * remove zeros from sum  (note this doesn't change sorting of args)
  -- - Sum(0,x,y) -> Sum(x,y)
    | Just (Op dims Sum inputs) <- I.lookup node exprs
    , nonZeros <- filter (not . isDeepZero exprs) inputs
    , [] == nonZeros || nonZeros /= inputs =
        case nonZeros of
            [] -> tt "no nonzeros in sum" $ addEdge exprs $ Const dims 0
            filtered ->
                tt "remove zeros from sum" $
                addEdge exprs $ Op dims Sum filtered
  -- * turn complex products into real products
  -- - (x +: y) * (z +: w) -> (x*z - y*w) +: (x*w - y*z)
    | Just (Op _dims Prod inputs) <- I.lookup node exprs
    , List.or $ map (nodeIsComplex exprs) inputs
         -- sort the nodes accoring to real/complex
     =
        let (cplx, real) = List.partition (nodeIsComplex exprs) inputs
            realParts = map (flip extractOrMakeReal) cplx
            imagParts = map (flip extractOrMakeImag) cplx
         -- form complex products
            (realProd, imagProd) = complexMult realParts imagParts
         -- form real product
            reals = prodE (map sourceNode real)
         in tt "decompose complex prod into real" $
            (if null real
                 then realProd +: imagProd
                 else (reals * realProd) +: (reals * imagProd))
                exprs
  -- * pull sum out of left dot
  -- - (x + y) <.> z -> Sum(x <.> z, y <.> z)
    | Just (left, right) <- M.dot exprs node
    , Just inputsL <- M.sum exprs (snd $ left exprs) =
        tt "sum `dot` ..." $ (sumE $ [dot l right | l <- inputsL]) exprs
  -- * pull sum out of right dot
  -- - x <.> ( y + z) -> Sum(x <.> y, x <.> z)
    | Just (left, right) <- M.dot exprs node
    , Just inputsR <- M.sum exprs (snd $ right exprs) =
        tt "... `dot` sum" $ (sumE $ [dot left r | r <- inputsR]) exprs
  --
  -- * combine identical terms in a sum
  -- - Sum(x,(-1) *. x,y) -> Sum(y)
  -- - Sum(2 *. x, (-1) *. x,y) -> Sum(x,y)
  -- - Sum(x,x,y) -> Sum(2 *. x,y)
    | Just (Op dims Sum summands) <- I.lookup node exprs
    , (e1, scaledNodes) <- List.mapAccumR (disassociateScale 1) exprs summands
    , grouped <- List.groupBy sndEqual $ List.sortBy compareSnd scaledNodes
    , List.or $ map ((> 1) . length) grouped =
        tt "pre-combine identical terms in a sum" $
        let recombine sameNodes@((_, theNode):_) =
                case sum $ map fst sameNodes of
                    0 -> mkConst dims 0
                    1 -> sourceNode theNode
                    d ->
                        case dims of
                            Dim0 -> (mkConst Dim0 d) * (sourceNode theNode)
                            _ -> (mkConst Dim0 d) `scale` (sourceNode theNode)
            recombine [] = error "simplify.recombine []"
         in tt "combine identical terms in a sum" $
            (sumE $ map recombine grouped) e1
  -- * reverse vectors in dot product if any differentials are on the right
  -- - x . (f dy ) -> ( f dy ) . x
    | Just (left, right) <- M.dot exprs node
    , uncurry HashedExpression.containsDifferential $ right exprs =
        tt "pre-dot reverse" $
        if uncurry HashedExpression.containsDifferential $ left exprs
            then error $ "dot both differentials " ++ show exprs
            else tt "dot reverse" $ (dot right left) exprs
  -- * differential only on left but not isolated, now pop/push operators to the right
  -- * but also simplify inside left, since this would otherwise be skipped
  -- - ( f dy ) . x -> dy . adj f x
    | Just (left, right) <- M.dot exprs node
    , uncurry HashedExpression.containsDifferential $ left exprs
  -- differential is not isolated
    , case I.lookup (snd $ left exprs) exprs of
         Just (DVar _ _) -> False
         _ -> True =
        tt "pushPop 1" $
        let (e1, left') = simplify' (left exprs)
            (e2, right') = simplify' (right e1)
         in tt "pushPop 2" $
            (sumO $ pushPop (sourceNode left') (sourceNode right')) e2
  -- * reverse arguments of dot to sort, if neither contains a differential
  -- - x . y -> y . x (if x > y, and niether contains differential)
    | Just (left, right) <- M.dot exprs node
    , not $ uncurry HashedExpression.containsDifferential $ right exprs
    , not $ uncurry HashedExpression.containsDifferential $ left exprs
    , left exprs > right exprs =
        tt "dot no diff reverse" $ (dot right left) exprs
  -- do adjoint of common-subexpression elimination for linear operators
  -- * simplify SCZ expression
  -- - SCZ (expr) [inputs] -> let expr' = simplify expr in SCZ (expr') [inputs]
    | Just (Op dims (SCZ (Expression sczN sczE)) inputs) <- I.lookup node exprs
    , Expression sczN1 sczE1 <- simplifyE "inSCZ" (Expression sczN sczE)
    , sczN1 /= sczN -- simplification had an effect
     =
        let (sczE2, sczN2) = recreate (sczE1, sczN1)
         in ttall ("oSCZ " ++ pretty (sczE2, sczN2)) $
            addEdge exprs $ mkSCZ dims (Expression sczN2 sczE2) inputs
  -- * move constant scaling of SCZ into SCZ expression to enable SCZ fusion,
  -- * and reduce computation downstream
  -- CKA:  It makes sense to move references to scalar values form the outer expression
  --       inside the SCZ too (e.g. dot products arising in CG and LBFGS), but this is
  --       probably not supported by our mechanisms, and we need SCZs to have more
  --       complete lambda powers
    | Just (Op _dims1 ScaleV [scaleN, sczN]) <- I.lookup node exprs
    , Just (Op dims (SCZ sczEx) inputs) <- I.lookup sczN exprs
    , Just (Const Dim0 c) <- I.lookup scaleN exprs =
        let Scalar (Expression sczN1 sczE1) = (fromDbl c) * (Scalar sczEx)
         in tt ("constant -> SCZ " ++ pretty (sczE1, sczN1)) $
            addEdge exprs $ mkSCZ dims (Expression sczN1 sczE1) inputs
  -- * eliminate SCZ with constant expressions
  -- - SCZ (c) [_] -> c
    | Just (Op dims (SCZ sczE) _) <- I.lookup node exprs
    , Just c <- constExpr sczE =
        tt ("mkConst " ++ show c) $ (mkConst dims c) exprs
  -- * eliminate SCZ that are the identity
  -- - SCZ[x](y) -> y
    | Just (Op _dims (SCZ (Expression sczN sczE)) [n]) <- I.lookup node exprs
    , Just (RelElem _i _b o) <- I.lookup sczN sczE
    , null $ filter (0 /=) o = tt "SCZ [input]" $ (exprs, n)
  -- * collapse SCZs with one unshifted input
  -- - SCZ( f r1 r2 )[x,y] -> SCZ( f r0 r1 )[x,y]
    | Just (Op dims (SCZ (Expression sczN sczE)) [input]) <- I.lookup node exprs
    , (_, newNode) <- addEdge sczE (RelElem 0 ZeroMargin $ relZero dims)
                                 -- get zero offset of correct dimension by (0*)
    , newNode == sczN = tt "collapse SCZ" $ (exprs, input)
  -- * remove unused inputs from SCZs expressions
  -- - SCZ( f r0 r2 )[x,y,z] -> SCZ( f r0 r1 )[x,z]
    | Just (Op dims (SCZ (Expression sczN sczE)) inputs) <- I.lookup node exprs
    , inputIdxs <- List.nub $ List.sort $ map fst $ relElems sczE sczN
    , length inputIdxs < length inputs =
        tt "pre-remove unused inputs" $
        let remap = I.fromList $ zip inputIdxs [0 ..]
         -- map relElems to compressed subset, otherwise don't remap
            reMapVar (RelElem idx b o)
                | Just newIdx <- I.lookup (-idx - 1) remap = RelElem newIdx b o
            reMapVar x = x
            (sczE', sczN') =
                tt "pre2-unused inputs" $
                simpRewrite reMapVar $ tt "pre1-unused inputs" (sczE, sczN)
            inputs' = onlyUsed inputs
            onlyUsed =
                map snd . filter (\(a, _b) -> a `elem` inputIdxs) . zip [0 ..]
         in tt "remove unused inputs" $
            addEdge exprs $
            case inputs' of
                [] -> Const dims 0
                _ ->
                    let (sczNN, sczEE) = simplify' (sczE', sczN')
                     in sczEE `seq` mkSCZ dims (Expression sczEE sczNN) inputs'
  -- * merge SCZ together with direct children SCZs, if the outer RelElem has zero offset
  -- - SCZ(f0 r0 r1)[SCZ(f2 r0 r1)[x,y],z] -> SCZ(f0 (f2 r0 r1) r2)[x,y,z]
    | Just (Op dims (SCZ (Expression sczN sczE)) inputs) <- I.lookup node exprs
  -- list of  (idx,argNode) for which all RelElems have zero offsets
    , offsets <- relElemZeroOffsets sczE sczN
  -- list of all RelElems (idx,Just ) if replaceable (idx,Nothing) otherwise
    , justReplaceable <-
         let removeNonZeroOffsets (idxSCZNode, Just (sczE, args)) =
                 case lookup idxSCZNode offsets of
                     Just _n -> (idxSCZNode, Just (sczE, args))
                     Nothing -> (idxSCZNode, Nothing)
             removeNonZeroOffsets x = x
          in map removeNonZeroOffsets $
             zip [0 ..] $ map (isSCZNode exprs) inputs
  -- just the replaceable ones
    , subSCZs <- filter ((/= Nothing) . snd) justReplaceable
    , not $ null subSCZs =
        tt "pre-SCZ [...sczs...]" $
        let oldIdxsOfReplaceableRelElems = map fst subSCZs
            rewrittenSubSCZs =
                map (\(sczEN, args) -> simpRewrite (reRemap args) sczEN) $
                map fromJust $ (Just ((sczE, sczN), inputs)) : (map snd subSCZs)
            (mergeE, newTopN:newSubNs) = mergeL' rewrittenSubSCZs
            maxUsed = length inputs + length keptInputs
         -- replace references to old subSCZs with new Nodes
            rewriteArgs arg = I.findWithDefault arg arg assocOldNtoNewN
             --   first find the node numbers of the RelElems
              where
                assocOldNtoNewN =
                    I.fromListWith
                        (\a b ->
                             if a == b
                                 then a
                                 else error $ "HS.rewriteArgs " ++ show (a, b)) $
                    concatMap findNegRelElem $ depthFirst mergeE newTopN
             -- map of oldRE indices to new node numbers
                assocNegOldIdxNewN =
                    zip
                        (map (\idx -> idx + maxUsed)
                             oldIdxsOfReplaceableRelElems)
                        newSubNs
             -- find the RelElems we will be replacing so we know which node numbers to look for
                findNegRelElem (node, RelElem idx _b _o) =
                    case lookup idx assocNegOldIdxNewN of
                        Just newN -> [(node, newN)]
                        Nothing -> []
                findNegRelElem _ = []
            (newSCZE, newSCZN) = simpRewrite' subsSubSCZs (mergeE, newTopN)
            subsSubSCZs (Op dims op args) = Op dims op $ map rewriteArgs args
            subsSubSCZs x = x
         -- map an input to its new index, or to a negative number
         -- if the old RelElem will be replaced by an newly included subexpression
            reRemap args (RelElem idx b o) =
                if idx < 0
                    then RelElem (inputToNewIdx (args !! (-idx - 1))) b o
                    else RelElem idx b o
            reRemap _ x = x
            inputToNewIdx oldNode =
                case lookup oldNode (replacedInputs ++ (zip keptInputs [0 ..])) of
                    Just idx -> idx
                    _ -> error "...sczs... node not found"
         -- the inputs which are not folded RelElems in the top-level and all lower inputs
            keptInputs =
                List.nub $
                List.sort $ concat $ zipWith kI justReplaceable inputs
              where
                kI (_idx, Nothing) arg = [arg]
                kI (_idx, Just (_, args)) _ = args
         -- the inputs which are replaced by the SCZ Expressions referenced by RelElems in the top-level
            replacedInputs = concat $ zipWith kI justReplaceable inputs
              where
                kI (idx, Just (_, _args)) arg = [(arg, idx + maxUsed)]
                kI _ _ = []
         -- *** unchecked assumption that inputs are all the same size as output
         in tt "SCZ [...sczs...]" $
            addEdge exprs $ mkSCZ dims (Expression newSCZN newSCZE) keptInputs
  -- * push sums into SCZ's so they are exposed to further simplifications
  -- - Sum(x,SCZ(f r0 r1)[y,z]) -> SCZ(r0 + f r1 r2)[x,y,z]
    | Just (Op dims Sum summands) <- I.lookup node exprs
    , summandSCZ <- map (isSCZNode exprs) summands
    , summandSCZDiff <-
         filter ((/= Nothing) . fst) $
         zip summandSCZ $
         map (HashedExpression.containsDifferential exprs) summands
    , not $ null $ drop 1 $ summandSCZDiff -- there are two SCZs
    , null $ drop 1 $ filter snd summandSCZDiff -- but at most 1 has a differential
     =
        tt "sum of SCZs" $
        -- get single list of all SCZ inputs
        let gatheredInputs =
                List.nub $ List.sort $ concatMap justArgs summandSCZ
            allArgs = zip gatheredInputs [0 ..]
        -- make map from old reIdxs to new reIdxs
            oldIdxToNew args idx =
                case lookup (args !! idx) allArgs of
                    Just new -> new
                    Nothing -> error "HS..Sum SCZ"
            reRemap args (RelElem idx b o) =
                if idx < 0
                    then RelElem (oldIdxToNew args (-idx - 1)) b o
                    else RelElem idx b o
            reRemap _ x = x
        -- remap relIdxs in separate SCZs, and merge them
            (mergedE, newNs) =
                mergeL'
                    [ simpRewrite (reRemap args) en
                    | (en, args) <- catMaybes summandSCZ
                    ]
        -- add sum node
            (newE, newN) = simplify' $ addEdge mergedE $ Op Dim0 Sum newNs
        -- create new SCZ node
        -- *** unchecked assumption that inputs are all the same size as output
            bigSCZ@(bigSCZE, bigSCZN) =
                addEdge exprs $ mkSCZ dims (Expression newN newE) gatheredInputs
         in if null $ filter (== Nothing) summandSCZ
       -- if there are no summands other than SCZs
                then bigSCZ
       -- if there are other summands, create a new Sum node
                else let nonSCZs =
                             filter ((== Nothing) . isSCZNode exprs) summands
                         (_, allSummands) =
                             nodeSort (bigSCZE, bigSCZN : nonSCZs)
                      in addEdge bigSCZE $ Op dims Sum allSummands
  -- * simplify SCZ if some inputs are constants
  -- - SCZ(f r0 r2)[x,c] -> SCZ(f r0 c)[x]
    | Just (Op dims (SCZ (Expression sczN sczE)) inputs) <- I.lookup node exprs
    , consts <- zip [0 ..] $ map (constExpr' exprs) inputs
    , not $ null $ filter ((/= Nothing) . snd) consts =
        tt "pre-SCZ [...consts...]" $
        let constIdxs = filter ((/= Nothing) . snd) consts
            inputIdxs = map fst $ filter ((== Nothing) . snd) consts
            idxRemap =
                I.fromList $
                (zip inputIdxs (map Left [0 ..])) ++
                (map (\(a, b) -> (a, Right b)) constIdxs)
         -- remap RelElems to input constants or remapped RelElems
            remap (RelElem idx b o)
                | Just (Right (Just c)) <- I.lookup (-idx - 1) idxRemap =
                    Const Dim0 c
                | Just (Left newIdx) <- I.lookup (-idx - 1) idxRemap =
                    RelElem newIdx b o
            remap x = x
            (sczE', sczN') = simpRewrite remap (sczE, sczN) {- mt ("SCZ 1" ++ pretty (sczE,sczN) ++" - " ++ show idxRemap) $ -}
            inputs' {- mt (pretty (Expression sczN' sczE') ++" const in scz "++ pretty (Expression sczN sczE))
           $ -}
             = onlyUsed inputs
            onlyUsed =
                map snd . filter (\(a, _b) -> a `elem` inputIdxs) . zip [0 ..]
            sczSimp@(Expression sczSimpN sczSimpE) =
                simplifyE "sczConst" $ Expression sczN' sczE'
            result =
                if null inputIdxs
                    then case I.lookup sczSimpN sczSimpE of
                             Just (Const Dim0 c) -> addEdge exprs $ Const Dim0 c
                             _ ->
                                 error $
                                 "SCZ with no used nonconstant inputs is not constant " ++
                                 pretty (exprs, node)
                    else sczE' `seq` addEdge exprs $ mkSCZ dims sczSimp inputs'
         in tt "SCZ [...const...]" $ result
  -- * if no simplification was applied to this node, then simplify args
  -- - f x, f \in [+,*] -> f (simplify x)
    | Just (Op dims op args) <- I.lookup node exprs
    -- and sort the args for |Sum|s and |Prod|s
     =
        tt ("pre-simpRewrite recurse " ++ show op) $
        let (e, simpArgs) =
                (if op `elem` [Sum, Prod]
                     then nodeSort
                     else id) $
                List.mapAccumR (curry $ simpRewrite' reWrite) exprs args
         in tt ("simpRewrite recurse " ++ pretty (exprs, node) ++ " ") $
            addEdge e $ Op dims op simpArgs
--
--
{-  | Just (Op _dims Sum summands) <- I.lookup node exprs
  , Just (e,n) <- commonTop exprs summands
  = tt "commonTop " $ (e,n)
  -}
{- -}
simpRewrite'' _ x = tt "simpRewrite'' fallthrough" x

{-

-}
constExpr (Expression n e) = constExpr' e n

constExpr' e n =
    let (e', n') = tt "simp for constExpr" $ simplify' (e, n)
     in case I.lookup n' e' of
            Just (Const _ c) -> Just c
            _ -> Nothing

{-


-}
{-

-}
isDeepZero exprs node =
    case I.lookup node exprs of
        Just (Const _ 0) -> True
        Just (Op _ ScaleV [s, v]) -> isDeepZero exprs s || isDeepZero exprs v
        Just (Op _ Prod vs) -> List.or $ map (isDeepZero exprs) vs
        Just (Op _ RealImag [re, im]) ->
            isDeepZero exprs re && isDeepZero exprs im
        Just (Op _ op [v]) -> linearOp op && isDeepZero exprs v
        _ -> False

{-

-}
compareSnd (_, x) (_, y) = compare x y

sndEqual (_, x) (_, y) = x == y

{-

-}
disassociateScale :: Double -> Internal -> Node -> (Internal, (Double, Node))
disassociateScale multiplier exprs factor =
    let ttt :: Int -> (Internal, (Double, Node)) -> (Internal, (Double, Node))
        ttt idx (e, (d, n)) =
            mt
                ("disasociateScale " ++
                 show idx ++ " " ++ show d ++ " " ++ pretty (e, n))
                (e, (d, n))
     in case I.lookup factor exprs
      -- from earlier rules we know we cannot get zeros, and we only have one const @ end;
              of
            Just (Op _dims Prod factors@(_:_)) ->
                let ([constN], rest) = splitAt 1 $ reverse factors
                 in case I.lookup constN exprs of
                        Just (Const Dim0 m) ->
                            case rest of
                                [] -> (exprs, (multiplier, factor))
                                [r] -> (exprs, (m * multiplier, r))
                                _ ->
                                    let (e, restN) =
                                            addEdge exprs $
                                            Op Dim0 Prod $ reverse rest
                                     in ttt 3 (e, (m * multiplier, restN))
                        _ -> ttt 2 (exprs, (multiplier, factor))
            Just (Op _dims ScaleV [a, b]) ->
                case I.lookup a exprs of
                    Just (Const Dim0 m) -> (exprs, (m * multiplier, b))
                    _ -> (exprs, (multiplier, factor))
            _ -> ttt 1 (exprs, (multiplier, factor))

{-

-}
pushPop :: Construct -> Construct -> Internal -> (Internal, [Node])
pushPop = pushPop' -- mt "pushPop" . pushPop'

pushPop' left right exprs
  -- if we see an ft on the left, put the adjoint on the right
    | Just (re, im) <- uncurry (M.xRe `o` (M.fwdFt `o` M.reIm)) $ left exprs =
        mt "pp8" $
        let (e1, lLst) = pushPop re (xRe $ invFt $ iRe right) exprs
            (e2, rLst) = pushPop im (xIm $ invFt $ iRe right) e1
         in (e2, lLst ++ rLst)
    | Just (re, im) <- uncurry (M.xRe `o` (M.invFt `o` M.reIm)) $ left exprs =
        mt "pp7" $
        let (e1, lLst) = pushPop re (xRe $ ft $ iRe right) exprs
            (e2, rLst) = pushPop im (xIm $ ft $ iRe right) e1
         in (e2, lLst ++ rLst)
    | Just (re, im) <- uncurry (M.xIm `o` (M.fwdFt `o` M.reIm)) $ left exprs =
        mt "pp6" $
        let (e1, lLst) = pushPop re (xRe $ invFt $ iIm right) exprs
            (e2, rLst) = pushPop im (xIm $ invFt $ iIm right) e1
         in (e2, lLst ++ rLst)
    | Just (re, im) <- uncurry (M.xIm `o` (M.invFt `o` M.reIm)) $ left exprs =
        mt "pp5" $
        let (e1, lLst) = pushPop re (xRe $ ft $ iIm right) exprs
            (e2, rLst) = pushPop im (xIm $ ft $ iIm right) e1
         in (e2, lLst ++ rLst)
    | Just (re, im) <- uncurry (M.xRe `o` (M.fwdCFt `o` M.reIm)) $ left exprs =
        mt "ppc8" $
        let (e1, lLst) = pushPop re (xRe $ invColumnPFT $ iRe right) exprs
            (e2, rLst) = pushPop im (xIm $ invColumnPFT $ iRe right) e1
         in (e2, lLst ++ rLst)
    | Just (re, im) <- uncurry (M.xRe `o` (M.invCFt `o` M.reIm)) $ left exprs =
        mt "ppc7" $
        let (e1, lLst) = pushPop re (xRe $ columnPFT $ iRe right) exprs
            (e2, rLst) = pushPop im (xIm $ columnPFT $ iRe right) e1
         in (e2, lLst ++ rLst)
    | Just (re, im) <- uncurry (M.xIm `o` (M.fwdCFt `o` M.reIm)) $ left exprs =
        mt "ppc6" $
        let (e1, lLst) = pushPop re (xRe $ invColumnPFT $ iIm right) exprs
            (e2, rLst) = pushPop im (xIm $ invColumnPFT $ iIm right) e1
         in (e2, lLst ++ rLst)
    | Just (re, im) <- uncurry (M.xIm `o` (M.invCFt `o` M.reIm)) $ left exprs =
        mt "ppc5" $
        let (e1, lLst) = pushPop re (xRe $ columnPFT $ iIm right) exprs
            (e2, rLst) = pushPop im (xIm $ columnPFT $ iIm right) e1
         in (e2, lLst ++ rLst)
    | Just (ss, n) <- uncurry M.proj $ left exprs =
        mt "pp4" $ pushPop n (injectSS ss right) exprs
    | Just (ss, n) <- uncurry M.inject $ left exprs =
        mt "pp3" $ pushPop n (projSS ss right) exprs
  -- if we see a negMask on the left and a subMask on the right, then the left projection
  -- is redundant, although we have to preserve the sign
    | Just (maskL, newLeft) <- uncurry M.negMask $ left exprs
    , Just (maskR, _newRight) <- uncurry M.subMask $ right exprs
    , (snd $ maskL exprs) == (snd $ maskR exprs) =
        mt "pp3a" $ pushPop newLeft (negate right) exprs
  -- if we see a SCZ on the left with a differential, we pull the differential out
  -- and return it on the left, and reformulate the SCZ and return it on the right
    | Just (Op dims (SCZ (Expression sczN sczE)) inputs) <-
         uncurry (flip I.lookup) $ left exprs
  -- we assume the differential is in the last slot, because that is how we create it
    , (diff:restRev) <- reverse inputs
  -- check that it is a differential
    , HashedExpression.containsDifferential exprs diff
  -- check that the SCZ is linear in this slot
    , isLinearIn
         sczE
         (\n ->
              case I.lookup n sczE of
                  Just (RelElem i _ _) -> i == (length inputs - 1)
                  _ -> False)
         sczN
     -- *** for now all the dims are the same, but this doesn't allow folding
     =
        let (e1, right') = right exprs
      -- negate the relative offsets in references to the last array variable
            reflectedExpr =
                eMap (reMapRE $ reflectN $ dimLength dims - 1) $
                Expression sczN sczE
            adjoint =
                \e ->
                    reflectedExpr `seq`
                    addEdge
                        e
                        (mkSCZ
                             dims
                             (simplifyE "sczPP" reflectedExpr)
                             ((reverse restRev) ++ [right']))
         in mt "ppSCZ" $ pushPop (sourceNode diff) adjoint e1
{- CKA/JLMP reread
  this is broken, it assumes that the scz expression is linear in all variables, for no reason
  the important property is the linearity in the last variable (dx) alone
  when we write the dot product as a sum, and the dx has the form zn[0,...],
  so we can just redraw the parentheses in the sum, to move the rest of the scz from the LHS to the RHS
  ... not quite right, because in the simple convolution case, we know that we need to end up
  with the transposed operator acting on the RHS
  ... if x appears in an input as zj[1,2] then when we calculate the differential, we will get
    zn[1,2] corresponding to dx, these indices need to be reflected before being applied to the RHS
  *** make sure chainRule in HashedDerivative is working
-}
pushPop' left right exprs =
    let (e, n) = (dot left right) exprs
     in e `seq` (e, [n])

{-
Check that a function is linear in a subexpression (includes case of no dependence),
we identify the subexpression with a function, since multiple RelElems can point to the same input
-}
isLinearIn :: Internal -> (Node -> Bool) -> Node -> Bool
isLinearIn exprs isSubexpr head
    | isSubexpr head = True
    | Just (Op _dims op inputs) <- I.lookup head exprs
    , op == Sum || linearOp op =
        List.and $ map (isLinearIn exprs isSubexpr) inputs
    | Just (Op _dims op inputs) <- I.lookup head exprs
    , op == Prod || op == Dot || op == ScaleV =
        (List.and $ map (isLinearIn exprs isSubexpr) inputs) &&
        (length (filter (== False) $ map (isConstIn exprs isSubexpr) inputs) < 2)
    | Just (Op _dims op inputs) <- I.lookup head exprs
    , op == Div = List.and $ map (isLinearIn exprs isSubexpr) inputs
  -- since the outer expression has passed the SCZ simplifications
  -- we know that all inputs are used, so we only need to check
  -- that all inputs are linear, at most one is nonconstant, and the
  -- nonconstant one (if it exists) is used linearly in the SCZ expression
    | Just (Op _dims op inputs) <- I.lookup head exprs
    , SCZ (Expression sczN sczE) <- op
    , List.and $ map (isLinearIn exprs isSubexpr) inputs
    , nonConsts <-
         filter ((False ==) . isConstIn exprs isSubexpr . snd) $
         zip [0 ..] inputs =
        case nonConsts of
            [] -> True
            [(idx, _)] ->
                List.and $
                map (isLinearIn sczE (== sczN)) $ reNodeWithIdx idx sczE sczN
            _ -> False
  -- (linear in) / (const in)
    | Just (Op _dims Div [num, denom]) <- I.lookup head exprs =
        isLinearIn exprs isSubexpr num && isConstIn exprs isSubexpr denom
  -- all other operators are nonlinear, so it is only linear if it is constant in
    | Just (Op _ _ inputs) <- I.lookup head exprs =
        List.and $ map (isConstIn exprs isSubexpr) inputs
  -- otherwise this is another variable or a constant
    | True = True

{-
Check that a function is linear in second node (includes case of no dependence)
-}
isConstIn :: Internal -> (Node -> Bool) -> Node -> Bool
isConstIn exprs isSubexpr head
    | isSubexpr head = False
    | Just (Op _dims _op inputs) <- I.lookup head exprs =
        List.and $ map (isConstIn exprs isSubexpr) inputs
    | True = True

{-
find nodes which are RelElems
-}
reNodeWithIdx idx e n
    | Just (RelElem i _ _) <- I.lookup n e
    , i == idx = [n]
    | Just (Op _ _ inputs) <- I.lookup n e =
        concatMap (reNodeWithIdx idx e) inputs
    | True = []

{-
remap array indices in relative elements
-}
reMapRE ::
       ((Int, Boundary, [Int]) -> (Int, Boundary, [Int]))
    -> (IntMap Int)
    -> ExpressionEdge
    -> ExpressionEdge
reMapRE remapArrays remapNodes ee =
    case ee of
        (RelElem idx b offsets) ->
            let (idx', b', offsets') = remapArrays (idx, b, offsets)
             in RelElem idx' b' offsets'
        Op d id nodes ->
            let mapNode node =
                    case I.lookup node remapNodes of
                        Just x -> x
                        _ ->
                            error $
                            "reMapRE impossible " ++ show (node, remapNodes, ee)
             in Op d id $ map mapNode nodes
        x -> x

{-

Reverse the last slot
-}
reflectN n (a, b, offsets) =
    if n == a
        then (a, b, map negate offsets)
        else (a, b, offsets)

{-

Map map of expression edges over a whole expression.
To do this we need to keep track of a map between old node numbers and new node numbers.
-}
eMap ::
       (IntMap Int -> ExpressionEdge -> ExpressionEdge)
    -> Expression
    -> Expression
eMap fun (Expression head exprs) = Expression newHead finalExprs
  where
    df = depthFirst exprs head
    (reMapper, finalExprs) =
        List.foldl' (flip $ foldEdge fun) (I.empty, I.empty) df
    newHead =
        case I.lookup head reMapper of
            Just x -> x
            Nothing -> error $ "eMap impossible" ++ show (head, reMapper, exprs)
    foldEdge ::
           (IntMap Int -> ExpressionEdge -> ExpressionEdge) {-remap nodes-}
        -> (Node, ExpressionEdge)
        -> (IntMap Int, Internal)
        -> (IntMap Int, Internal)
    foldEdge mapEE (node, ee) (nodeRemap, exprs) --error $ show (node,ee,nodeRemap,exprs,df)
     = (newNodeRemap, newExprs)
        -- map the expression edge, including node remapping
      where
        newEE = mapEE nodeRemap ee
        -- add this edge to the graph we are building
        (newExprs, newNode) = addEdge exprs newEE
        -- add remapping for node we just created
        newNodeRemap = I.insert node newNode nodeRemap

{-

-}
{-


-}
complexMult (r1:[]) (im1:[]) = (r1, im1)
complexMult (r1:r2:reals) (i1:i2:imags) =
    complexMult ((r1 * r2 - i1 * i2) : reals) ((r1 * i2 + i1 * r2) : imags)
complexMult r i = error $ show "complexMult " ++ show (r, i)

{-


Extract real/imag part, or project real/imag part from complex node
-}
xMkReal :: (Internal, [Node]) -> (Internal, [Node])
xMkReal (exprs, nodes) = List.mapAccumR extractOrMakeReal exprs nodes

extractOrMakeReal :: Internal -> Node -> (Internal, Node)
extractOrMakeReal es node
    | Just (reN, _) <- M.reIm es node = reN es
extractOrMakeReal es n = addEdge es $ Op (getDimE es n) RealPart [n]

xMkImag :: (Internal, [Node]) -> (Internal, [Node])
xMkImag (exprs, nodes) = List.mapAccumR extractOrMakeImag exprs nodes

extractOrMakeImag es node
    | Just (_, imN) <- M.reIm es node = imN es
extractOrMakeImag es n = addEdge es $ Op (getDimE es n) ImagPart [n]

{-

-}
mergeScaledConstsBy ::
       (Double -> Double -> Double)
    -> (Internal, [(Double, Node)])
    -> (Internal, [(Double, Node)])
mergeScaledConstsBy op (exprs, scaledNodes) = (exprs', combined ++ nonconsts)
  where
    consts =
        concatMap
            (\(m, n) ->
                 case I.lookup n exprs of
                     Just (Const Dim0 x) -> [m * x]
                     _ -> [])
            scaledNodes
    nonconsts =
        filter
            (\(_m, n) ->
                 case I.lookup n exprs of
                     Just (Const Dim0 _) -> False
                     _ -> True)
            scaledNodes
    (exprs', combined) =
        if null consts
            then (exprs, [])
            else (expr'', [(1, combinedNode)])
    (expr'', combinedNode) = addEdge exprs (Const Dim0 $ foldr1 op consts)

{-
Recursively combine all |op|s which are terms in this |op|
-}
collapse :: Internal -> OpId -> Node -> [Node]
collapse exprs op term =
    case I.lookup term exprs of
        Just (Op _dims op1 newTerms) ->
            if op == op1
                then concatMap (collapse exprs op) newTerms
                else [term]
        _ -> [term]

{-

-}
cartProd (l1:ls) = [i : j | i <- l1, j <- cartProd ls]
cartProd [] = [[]] {-
-}
