{-
-}
module HashedDot where

import Polynomials

import HashedExpression
import HashedInstances ()

import Control.Monad (liftM)
import qualified Data.ByteString.Char8 as C

--import HashedSimplify
import qualified Data.IntMap as I
import qualified Data.IntSet as Set
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Ratio

import Debug.Trace

threeDC2dot name (ThreeDC (Expression n e)) =
    writeFile (addDot name) $ snd $ expr2dot' name (e, n)

threeD2dot name (ThreeD (Expression n e)) =
    writeFile (addDot name) $ snd $ expr2dot' name (e, n)

scalar2dot name (Scalar (Expression n e)) =
    writeFile (addDot name) $ snd $ expr2dot' name (e, n)

scalar2polys name (Scalar (Expression n e)) = fst $ expr2dot' name (e, n)

addDot fileName =
    case reverse fileName of
        't':'o':'d':'.':_ -> fileName
        _ -> fileName ++ ".dot"

expr2dot' name (e, n) =
    ( sczs
    , unlines $
      [ "digraph " ++ name ++ " {"
      , " nslimit=\"100\";"
      , " mclimit=\"100\";"
      , " ranksep=\"0.4\";"
      , " node [shape=\"none\",width=\"1\",height=\"1\",fontsize=\"30\"];"
      , " edge [len=\"1.9\"];"
      ] ++
      dotDefs ++ ["}"])
  where
    nodes = Set.toList $ sczInputs e n
    nodesWithVars = zip nodes $ map (maybeVarNode e) nodes
    ((_, sczs), dotDefs) =
        node2dot
            (I.fromList $ zip [0 ..] $ map snd nodeSubs)
            e
            (Set.empty, [])
            n
    ((_, _notes), nodeSubs) = L.mapAccumL mkShort (0, []) nodesWithVars
    mkShort ::
           (Node, [[Char]])
        -> (Node, Maybe C.ByteString)
        -> ((Node, [[Char]]), (Node, [Char]))
    mkShort idxn (n, Just varName) = (idxn, (n, C.unpack varName))
    mkShort (idx, notes) (n, Nothing) =
        ( (n, ("<" ++ show idx ++ "> = " ++ (pretty (e, n))) : notes)
        , (n, "<" ++ show idx ++ ">"))

sczInputs e n =
    case I.lookup n e of
        Just (Op _ (SCZ _) inputs) ->
            Set.unions ((Set.fromList inputs) : (map (sczInputs e) inputs))
        Just (Op _ _ inputs) -> Set.unions $ map (sczInputs e) inputs
        _ -> Set.empty

{-

-}
nn n = "\"" ++ show n ++ "\""

nnl n label = nn n ++ " [label=\"" ++ label ++ "\"];"

en n1 n2 = nn n1 ++ " -> " ++ nn n2 ++ ";"

node2dot nodeSubs e (ns, sczs) n =
    trace (show ns) $
    if Set.member n ns
        then ((ns, []), [])
        else let nns = Set.insert n ns
              in case I.lookup n e of
                     Nothing -> ((nns, []), [nnl n (show n ++ " not found")])
                     Just (Op _ op inputs) ->
                         let uniqueInputs =
                                 trace (show op) $ L.nub $ L.sort inputs
                             ((nsNew, sczs'), ls) =
                                 L.mapAccumR
                                     (node2dot nodeSubs e)
                                     (nns, sczs)
                                     inputs
                          in ( (nsNew, (extractSczs op inputs) ++ sczs')
                             , (nnl n (showOp op nodeSubs)) :
                               ((concat ls ++ map (en n) uniqueInputs)))
                     Just (Var _ name) -> ((nns, []), [nnl n (C.unpack name)])
                     Just (DVar _ name) ->
                         ((nns, []), [nnl n ("d(" ++ C.unpack name ++ ")")])
                     Just (RelElem i _ idx) ->
                         ((nns, []), [nnl n ("RE" ++ show i ++ show idx)])
                     Just (Const _ d) -> ((nns, []), [nnl n (show d)])

extractSczs (SCZ (Expression subN subE)) inputs =
    error $ show [((subE, subN), inputs)]
extractSczs op inputs = trace (show (op, inputs)) []

showOp (SCZ (Expression n e)) nodeSubs = prettySubsRE e nodeSubs n
showOp (FT True) _ = "FT"
showOp (FT False) _ = "InvFT"
showOp (MapND e _input) _ = pretty e
showOp x _ = show x

{-

Rewriting SCZ search from scratch
-}
scalarSCZSearch (Scalar (Expression n e)) =
    let sczNodes = L.nub $ L.sort $ sczSearch e n
        allInputs = L.nub $ L.sort $ concatMap (sczsToIndexedInputs e) sczNodes
        fwdRevLookup =
            ( I.fromList $ zip [0 ..] allInputs
            , M.fromList $ zip allInputs [0 ..])
        polyEnv = PolyEnv fwdRevLookup (Expression n e)
        sczToPoly n =
            case I.lookup n e of
                Just (Op _ (SCZ (Expression sczN sczE)) inputs) ->
                    [(n, extractPoly polyEnv inputs sczE sczN)]
                x -> error $ "sczsToIndexedInputs found " ++ show (x, n, e)
     in concatMap sczToPoly sczNodes

sczSearch e n =
    case I.lookup n e of
        Just (Op _ (SCZ _) _) -> [n]
        Just (Op _ _ inputs) -> concatMap (sczSearch e) inputs
        _ -> []

sczsToIndexedInputs e n =
    case I.lookup n e of
        Just (Op _ (SCZ (Expression sczN sczE)) inputs) ->
            let re2Node = I.fromList $ zip [0 ..] inputs
                relElems sn =
                    case I.lookup sn sczE of
                        Just (RelElem num bnd idxs) ->
                            case I.lookup num re2Node of
                                Just node -> [(node, bnd, idxs)]
                                _ -> error "sczsToIndexedInputs lookup re2Node"
                        Just (Op _ _ opinputs) -> concatMap relElems opinputs
                        _ -> []
             in relElems sczN
        x -> error $ "sczsToIndexedInputs found " ++ show (x, n, e)

{-

Print search results
-}
printSCZPolys scalar@(Scalar (Expression n e)) =
    (printPolys $ map (\(n, Just p) -> (show n, p)) polys) ++
    (unlines $ map printSCZ nonpolys) ++ (unlines $ tvars)
  where
    maybeList = scalarSCZSearch scalar
    (polys, nonpolys) = span isPoly maybeList
    isPoly (_, Nothing) = False
    isPoly _ = True
    tvars =
        case polys of
            (_n, Just (Poly (PolyEnv (fwdLookup, _) _) _)):_ ->
                concatMap pTvar $ I.toList fwdLookup
            _ -> []
    pTvar (idx, (inNode, bndy, idxs)) =
        case I.lookup n e of
            Just _ ->
                [ "t" ++
                  show idx ++
                  " = " ++
                  pretty (Expression inNode e) ++
                  " with " ++ show bndy ++ " offset " ++ show idxs
                ]
            _ -> []
    printSCZ (n, _) =
        show n ++
        " = " ++
        (case I.lookup n e of
             Just (Op _ (SCZ sczExpr) _) -> pretty sczExpr
             _ -> "!!!! unknown node !!!!")

{-

Convert SCZ to Poly
Extract a polynomial from an expression, or return Nothing if it is not a polynomial.
The polynomial is of the form of a list of monomials [(Map(varName->power),coeff)]
-}
extractPoly pe@(PolyEnv (_, nbidx2varIdx) _envExpr) inputs e n =
    case I.lookup n e of
        Just (Op _ Sum summands) ->
            case mapMaybe extractMonomial summands of
                [] -> Nothing
                listOfMonos -> Just $ Poly pe listOfMonos
        _ -> fmap ((Poly pe) . (: [])) $ extractMonomial n
  where
    num2Node = I.fromList $ zip [0 ..] inputs
    varLkup num bnd idxs =
        case I.lookup num num2Node of
            Just node ->
                case M.lookup (node, bnd, idxs) nbidx2varIdx of
                    Just varIdx -> varIdx
                    x ->
                        error $
                        "extractPoly.varLkup2 " ++
                        show (x, num, node, bnd, idxs)
            x -> error $ "extractPoly.varLkup1 " ++ show (x, num, bnd, idxs)
    --extractMonomial :: Node -> Maybe Monomial
    extractMonomial n =
        case I.lookup n e of
            Just (Op _ Prod terms) -> monoFromTerms (I.empty, 1) terms
            _ -> monoFromTerms (I.empty, 1) [n]
      where
        monoFromTerms (vars, c) (term:terms) =
            case I.lookup term e of
                Just (RelElem num bnd idxs) ->
                    monoFromTerms
                        (I.insertWith (+) (varLkup num bnd idxs) 1 vars, c)
                        terms
                Just (Const _ d) ->
                    monoFromTerms (vars, c * (approxRational d 0.0001)) terms
                _ -> Nothing
        monoFromTerms (vars, c) [] =
            Just
                ( c
                , ( I.foldr (+) 0 vars -- total degree of all variables
                  , vars))
