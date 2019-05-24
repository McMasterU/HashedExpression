{-
-}
module Hashed2Dot where

import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap as I
import qualified Data.IntSet as Set
import qualified Data.List as L
import HashedExpression

threeDC2dot fileName (ThreeDC (Expression n e)) =
    writeFile fileName $ expr2dot' (e, n)

threeD2dot fileName (ThreeD (Expression n e)) =
    writeFile fileName $ expr2dot' (e, n)

scalar2dot fileName (Scalar (Expression n e)) =
    writeFile fileName $ expr2dot' (e, n)

expr2dot' (e, n) =
    unlines $
    [ "digraph fmaSPU {" -- }
    , " nslimit=\"100\";"
    , " mclimit=\"100\";"
    , " ranksep=\"0.4\";"
    , " node [shape=\"none\",width=\"1\",height=\"1\",fontsize=\"30\"];"
    , " edge [len=\"1.9\"];"
    ] ++
    snd (node2dot e Set.empty n) ++ -- {
    ["}"]

{-

-}
nn n = "\"" ++ show n ++ "\""

nnl n label = nn n ++ " [label=\"" ++ label ++ "\"];"

en n1 n2 = nn n1 ++ " -> " ++ nn n2 ++ ";"

node2dot e ns n =
    if Set.member n ns
        then (ns, [])
        else let nns = Set.insert n ns
              in case I.lookup n e of
                     Nothing -> (nns, [nnl n (show n ++ " not found")])
                     Just (Op _ op inputs) ->
                         let uniqueInputs = L.nub $ L.sort inputs
                             (nsNew, ls) =
                                 L.mapAccumR (node2dot e) nns uniqueInputs
                          in ( nsNew
                             , (nnl n (showOp op)) :
                               ((concat ls ++ map (en n) uniqueInputs)))
                     Just (Var _ name) -> (nns, [nnl n (C.unpack name)])
                     Just (DVar _ name) ->
                         (nns, [nnl n ("d(" ++ C.unpack name ++ ")")])
                     Just (RelElem i _ idx) ->
                         (nns, [nnl n ("RE" ++ show i ++ show idx)])
                     Just (Const _ d) -> (nns, [nnl n (show d)])

showOp (SCZ e) = pretty e
showOp (MapND e input) = pretty e
showOp x = show x
{-
data ExpressionEdge  = Op  Dims                          -- dims of output
                           OpId                          -- the operation
                           [Node]                        -- dims and nodes of inputs
                     | Var Dims ByteString               -- variable X
                     | DVar Dims ByteString              -- differential variable dX
                     | RelElem  {reArray::Int            -- array number
                                ,reBoundary::Boundary    -- how to treat boundary elements
                                ,reIdx::[Int]            -- offsets in array indices
                                }                        --   length reIdx == length outDims
                     | Const {unConstDims::Dims, unConst ::Double}

-}
