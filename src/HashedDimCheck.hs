{-
\documentclass[10pt]{amsart}
\usepackage{listingsutf8}    %for making code contexts, as well as a few other things
\usepackage{comment}         %for block comments \begin{comment}...\end{comment}
\usepackage{geometry}
\usepackage{algpseudocode}
\usepackage{algorithm}
\usepackage{titletoc}        %for making a list of the sections; not used yet
\lstnewenvironment{code}     %using the listing environment for code instead of verbatim so that long lines of codes break nicely
  {
    \lstset{
        language=Haskell,
        basicstyle=\small\ttfamily,
        breaklines=true,
        literate={<.>}{{$\bullet$}}1  %special characters need to be specified here; see tex.stackexchange.com/questions/24528 for more information
        }
  }{}

\begin{document}

\begin{comment}
-}
module HashedDimCheck where

import HashedDerivative
import HashedExpression
import HashedInstances
import HashedSimplify
import HashedTransformable

-- import HashedMRI
-- import R2Star
import Control.DeepSeq
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap as I
import qualified Data.List as L
import Data.Maybe (fromJust)
import Test.QuickCheck

{-
\end{comment}

\title{HashedDimCheck}
\maketitle

\section{Defining exprDimCheck}

In this module, we go through an expression and find the dimension of each node, based both on the nodes above and the nodes below.  Then we compare this dimension to the existing dimension, replace DimUnknowns and raise exceptions for mismatches.  % These errors could be confusing if the mismatched node was something inferred from something else.  The error is still necessary, but it might be difficult to find the problem.  Maybe print out the entire expression, with the error so that the user can see what is happening.

Here are the functions.  exprDimCheck applies intDimCheck to the internal of the expression, and returns the new expression.  This dimension check does not depend on the top node of an expression.  There are also a bunch of nice functions for individual types of expressions.
-}
--FIXME it should check that there are no DimUnknowns left.
scalarDimCheck :: Scalar -> Scalar
scalarDimCheck (Scalar expr) = Scalar (exprDimCheck expr)

oneDDimCheck :: OneD -> OneD
oneDDimCheck (OneD expr) = OneD (exprDimCheck expr)

oneDCDimCheck :: OneDC -> OneDC
oneDCDimCheck (OneDC expr) = OneDC (exprDimCheck expr)

twoDDimCheck :: TwoD -> TwoD
twoDDimCheck (TwoD expr) = TwoD (exprDimCheck expr)

twoDCDimCheck :: TwoDC -> TwoDC
twoDCDimCheck (TwoDC expr) = TwoDC (exprDimCheck expr)

exprDimCheck :: Expression -> Expression
exprDimCheck (Expression top internal) = Expression top (intDimCheck internal)

{-

intDimCheck iterates through intDimCheck' until there are no more changes to be made; it reaches a fixed point.

It is guaranteed to reach a fixed point because the maximum number of iterations is the number of nodes in the internal.  This happens if the original internal has all unknown dimensions and each iteration changes one node.  A node can only be changed once, from a DimUnknown to a known dimension.  After this, if an implied dimension conflicts with the actual dimension, there is an error in the expression.

Wait! check out what happens in a compound node if the dimension is known, but one of the inner dimensions is not.  Get back to this later.


-}
intDimCheck :: Internal -> Internal
intDimCheck internal =
    internal `deepseq` sub 1000 internal $ intDimCheck' internal
  where
    sub 0 _oldIn internal =
        error $
        "intDimCheck' ran out of iterations without reaching fixed point " ++
        show internal
    sub iter oldIn internal =
        mt ("sub " ++ unwords [show iter, take 100 (show internal)]) $
        if internal == oldIn
            then mt ("intDimCheck done " ++ show internal) internal
            else sub (iter - 1) internal $ intDimCheck' internal

{-

intDimCheck' uses upInternal and downInternal to replace the internal with an updated one.   upInternal replaces the internal by searching above a node for dimensions; downInternal replaces the internal by searching below a node for dimensions.  After the search, the dimension based on the surrounding nodes is compared to the existing dimension.
-}
intDimCheck' :: Internal -> Internal
intDimCheck' internal =
    upInternal (I.keys internal) $ downInternal (I.keys internal) internal

{-

downInternal replaces an internal using downEdge which returns an updated ExpressionEdge based on the node and internal
-}
downInternal :: [Node] -> Internal -> Internal
downInternal [] internal = internal
downInternal (node:nodes) internal = newInternal
  where
    newInternal = I.insert node newEdge $ downInternal nodes internal
    newEdge = downEdge node internal

{-

downEdge replaces an edge with updated dims based on inputs, if the dimension based on the inputs conflicts with the existing dimension, it raises an exception.
-}
downEdge :: Node -> Internal -> ExpressionEdge
downEdge node internal =
    mt ("HDC.downEdge replacing " ++ show currEdge ++ " with " ++ show newEdge)
        newEdge
  where
    newEdge =
        case currEdge of
            Op (CDims dims) (Compound int) inputs ->
                Op
                    (CDims (zipWith inputDims (map (: []) inputs) dims))
                    (Compound int)
                    inputs
            Op DimUnknown (Compound int) inputs ->
                Op
                    (CDims $ map ((`inputDims` DimUnknown) . (: [])) inputs)
                    (Compound int)
                    inputs
            Op oldEDim (Extract int) [input] ->
                case getDimE internal input of
                    DimUnknown -> Op oldEDim (Extract int) [input]
            --this seems to work; but it's really ugly FIXME
                    CDims inDims ->
                        let newEDim =
                                if ((L.length inDims) > int)
                                    then mt (show inDims ++ show int)
                                             (inDims !! int)
                                    else error
                                             ("Extract " ++
                                              show int ++
                                              " applied to Compound " ++
                                              show inDims ++
                                              "; the index is probably out of range.")
                         in if (newEDim == oldEDim) || (newEDim == DimUnknown)
                                then Op oldEDim (Extract int) [input]
                                else if oldEDim == DimUnknown
                                         then Op newEDim (Extract int) [input]
                                         else error
                                                  ("HDC.downEdge found: " ++
                                                   show currEdge ++
                                                   " has child " ++
                                                   show
                                                       (fromJust
                                                            (I.lookup
                                                                 input
                                                                 internal)))
                    badCDims ->
                        error
                            ("HDC.downEdge found node below Extract (should be a Compound) with " ++
                             show badCDims)
            Op dim opId inputs ->
                case opId of
                    Compound _ ->
                        error
                            ("HDC.downEdge found Compound node with " ++
                             show dim)
                    Extract _ ->
                        error
                            ("HDC.downEdge found Extract node with " ++
                             show inputs)
                    ScaleV -> Op (newDim [inputs !! 1] dim) ScaleV inputs
                    x
                        | elem
                             x
                             [ Dot
                             , Prod
                             , Div
                             , Sqrt
                             , Sin
                             , Cos
                             , Tan
                             , Exp
                             , Log
                             , Sinh
                             , Cosh
                             , Tanh
                             , Asin
                             , Acos
                             , Atan
                             , Asinh
                             , Acosh
                             , Atanh
                             , Abs
                             , Signum
                             ] ->
                            if dim == Dim0 || dim == DimUnknown
                                then Op Dim0 opId inputs
                                else error (show opId ++ " has " ++ show dim)
                    x
                        | elem
                             x
                             [ Sum
                             , Neg
                             , RealPart
                             , ImagPart
                             , RealImag
                             , SubMask
                             , NegMask
                             ] -> Op (newDim inputs dim) opId inputs
                    FT _ -> Op (newDim inputs dim) opId inputs
                    PFT _ _ -> Op (newDim inputs dim) opId inputs
                    MapND _ _ -> Op (newDim inputs dim) opId inputs
                    Transpose swap ->
                        Op
                            (revTrDims swap (newDim inputs (revTrDims swap dim)))
                            opId
                            inputs
                    SCZ expr ->
                        Op (newDim inputs dim) (SCZ (exprDimCheck expr)) inputs
                    Project ss -> Op (projInp inputs dim ss) opId inputs
                    Inject ss -> Op (injInp inputs dim ss) opId inputs
                    _ ->
                        error
                            ("HDC.downEdge does not (yet) know about " ++
                             show opId)
            _ -> currEdge
    projInp :: [Node] -> Dims -> Subspace -> Dims
    projInp inputs olDim ss = projDim (inputDims inputs (injectDim olDim ss)) ss
    injInp inputs olDim ss = injectDim (inputDims inputs (projDim olDim ss)) ss
    newDim = inputDims
    inputDims :: [Node] -> Dims -> Dims
    inputDims inputs olDim =
        case L.filter
                 (/= DimUnknown)
                 (L.nub (downDims node internal inputs [olDim])) of
            [] -> DimUnknown
            [dim] -> dim
            dims ->
                error
                    ("HDC.downEdge found: " ++
                     show currEdge ++
                     " has child(ren) " ++
                     show (map fromJust (map (`I.lookup` internal) inputs)) ++
                     ", which does not match.")
    currEdge = fromJust (I.lookup node internal)

{-
\begin{description}
  \item [Compound] Pattern match Compound separately so that the dimensions are easier to access.
-}
{-
  \item [Extract] Pattern match Extract separately so that the input is easier to access.  There should only be one input.
-}
-- this is the dimension of the compound node below the extract
-- the compound node below did not tell anything, oldEDim is good
{-
  \item [Simpler Stuff] It's easier to just match the opIds
-}
{-
  \item [Compound,Extract] The compound did not properly match a dim above; the extract did not properly match the input above. Error.
-}
{-
  \item [ScaleV] the same dimension as the second input
-}
{-
  \item [Dot,Prod,Div,Sqrt,Sin,Cos,Tan,Exp etc.] all Dim0
-}
{-
  \item [Sum,Neg,RealPart,ImagPart,RealImag,FT,PFT,MapND] the same dimensions as the inputs; the ops which take arguments must be separated for pattern matching
-}
{-
  \item [Transpose] use functions from HashedExpression to find out what the swapped dimensions are; then apply newDim
-}
{-
  \item [Sparse Convolution Zip] same dimensions as the inputs, make sure to also check the innner expression
-}
{-
  \item [Project,Inject] the dimensions are based on inputs, but different, so use projInp and injInp
-}
{-
  \item [Anything Else] something without inputs; we can't check anything now.
\end{description}
-}
{-
Finding the dimensions of a projection or injection based on the inputs. olDim is the original dimension of the proj node; we are comparing to the input of the proj node, so take the injDim.  Opposite for injInp
-}
{-
The dimension of the node based directly on the inputs
-}
{-
inputDims uses downDims, which returns a list of dims that are based on the inputs; nub and filter for unknowns.  More than one dimension is implied, there is a conflict.
-}
{-

A function to find the dimension of a node based on its inputs; it returns a list of implied dims.
-}
downDims :: Node -> Internal -> [Node] -> [Dims] -> [Dims]
downDims node internal [] dims = dims
downDims node internal (input:inputs) dims =
    mt ("applying downDims " ++
        show node ++ take 100 (show internal) ++ show inputs)
        (newDim ++ (downDims node internal inputs dims))
  where
    newDim =
        case currChild of
            Var DimUnknown _ -> []
            DVar DimUnknown _ -> []
            Const DimUnknown _ -> []
            Op DimUnknown _ _ -> []
            RelElem reA reB reI -> [Dim0] --see HE.getDim 1830
            Var dim _ -> [dim]
            DVar dim _ -> [dim]
            Const dim _ -> [dim]
            Op dim _ _ -> [dim]
    currChild = fromJust (I.lookup input internal)

{-
We can't tell the dimensions based on this child, so add the empty list to the list of possible dims
-}
{-
We can tell the dimensions based on this child, so add a list containing the new dimension to the list of possible dimensions.
-}
{-


a function to update the dimensions of an internal based on the parents of a node.
-}
upInternal :: [Node] -> Internal -> Internal
upInternal [] internal = internal
upInternal (node:nodes) internal = newInternal
  where
    newInternal = I.insert node newEdge (upInternal nodes internal)
    newEdge = upEdge node internal

{-

a function replace an edge based on its parents
-}
upEdge :: Node -> Internal -> ExpressionEdge
upEdge node internal = newEdge
  where
    newEdge =
        case currEdge --map a function which takes a dim and returns a dim
              of
            Op DimUnknown (Compound int) inputs --may have to figure this out directly, rather than using newDim
             ->
                Op
                    (CDims
                         (newCDims
                              node
                              internal
                              currParents
                              (replicate int DimUnknown)))
                    (Compound int)
                    inputs
            Op (CDims dims) (Compound int) inputs ->
                Op
                    (CDims (newCDims node internal currParents dims))
                    (Compound int)
                    inputs
            Op dim opId inputs -> Op (newDim currParents dim) opId inputs
            Var dim name -> Var (newDim currParents dim) name
            DVar dim name -> DVar (newDim currParents dim) name
            Const dim num -> Const (newDim currParents dim) num
            _ -> currEdge
    currParents :: [Node]
    currParents = getParents internal node
    currEdge = fromJust (I.lookup node internal)
    newDim :: [Node] -> Dims -> Dims
    newDim parents olDim =
        case parentDims parents [olDim] of
            [] -> DimUnknown
            [dim] -> dim
            dims ->
                error
                    ("HDC.upEdge found " ++
                     show currEdge ++
                     " has parent(s) " ++
                     show (map fromJust (map (`I.lookup` internal) parents)) ++
                     ", which does not match.")
    newCDims :: Node -> Internal -> [Node] -> [Dims] -> [Dims]
    newCDims node internal [] dims = dims
    newCDims node internal (par:pars) dims =
        case fromJust (I.lookup par internal) of
            Op DimUnknown (Extract int) _ -> newCDims node internal pars dims --I don't think this condition should ever be matched because of downInternal
            Op dim (Extract int) _ --say what index are we taking? FIXME
             ->
                if length dims > int --check !! for safety
                    then if (dims !! int == dim) || (dims !! int == DimUnknown) --check at these match properly
                             then newCDims
                                      node
                                      internal
                                      pars
                                      (map ((!!) dims) [0 .. int - 1] ++
                                       [dim] ++
                                       map
                                           ((!!) dims)
                                           [int + 1 .. (length dims - 1)] --take of a list
                                       )
                             else error
                                      ("HDC.upEdge found " ++
                                       show (fromJust (I.lookup par internal)) ++
                                       " with child " ++
                                       show (fromJust (I.lookup node internal)))
                    else error
                             ("For some reason, we have a problem." ++
                              show int ++ show dims)
            Op DimUnknown (Compound int) _ -> dims
            Op cDims (Compound int) _ -> newCDims node internal pars dims
            other ->
                error
                    ("HDC.upEdge found " ++
                     show other ++
                     " with child " ++ show (fromJust (I.lookup node internal)))
    parentDims parents olDim =
        L.filter (/= DimUnknown) (L.nub (upDims node internal parents olDim))

--        Op (CDims (zipWith newDim (map (\x -> [x]) currParents) (repeat DimUnknown)) (Compound int) inputs
{-
If the parent is a compound, then one of the dims of the child compound must match the parent node dim.  I can't tell which position the parent dim goes in though, so we just leave this.
-}
{-

upDims finds the dimensions of a node based on the parents of the node
-}
upDims :: Node -> Internal -> [Node] -> [Dims] -> [Dims]
{-

If there are no parents, then we still do not know the dimension
-}
upDims node internal [] dims = dims
upDims node internal (par:pars) dims =
    upDims node internal pars (dims ++ newDim)
  where
    newDim =
        case currParent of
            Op DimUnknown opId inputs -> []
            Op dim ScaleV inputs -- case node of (inputs !! 0) -> ; (inputs !! 1) -> ; does this work? then we can raise errors if there are too many inputs to the scaleV
             ->
                if mt "!! here 310" (inputs !! 0) == node
                    then [Dim0]
                    else [dim]
            Op dim (Transpose swap) _ -> [transposeDims swap dim]
            Op (CDims inDims) (Compound _) inputs ->
                mt
                    "looking up dim"
                    [inDims !! (fromJust (L.elemIndex node inputs))]
            Op dim (Extract int) [input] ->
                error
                    "HDC.upDims found Extract; Extract should not make it through upDims"
            Op dim (Extract int) inputs ->
                error ("HDC.upDims found Extract with children " ++ show inputs)
            Op dim opId inputs ->
                case opId of
                    Dot ->
                        if length sibDims <= 1
                            then sibDims
                            else error
                                     ("Dot product with input dims " ++
                                      show sibDims)
                    x
                        | elem
                             x
                             [ Sum
                             , Neg
                             , RealPart
                             , ImagPart
                             , RealImag
                             , SubMask
                             , NegMask
                             ] -> [dim]
                    FT _ -> [dim]
                    PFT _ _ -> [dim]
                    MapND _ _ -> [dim]
                    SCZ _ -> [dim]
                    x
                        | elem
                             x
                             [ Prod
                             , Div
                             , Sqrt
                             , Sin
                             , Cos
                             , Tan
                             , Exp
                             , Log
                             , Sinh
                             , Cosh
                             , Tanh
                             , Asin
                             , Acos
                             , Atan
                             , Asinh
                             , Acosh
                             , Atanh
                             , Abs
                             , Signum
                             ] -> [Dim0]
                    Project ss -> [injectDim dim ss]
                    Inject ss -> [projDim dim ss]
                    _ ->
                        error
                            ("HDC.upDims does not (yet) know about this Op: " ++
                             show opId)
            _ ->
                error
                    ("HDC.upDim does not (yet) know about this parent: " ++
                     show currParent)
    currParent = fromJust (I.lookup par internal)
    sibDims' = map (getDimE internal) (getSiblings internal node)
    sibDims = L.nub (filter (/= DimUnknown) sibDims')
    currCDims = cDims (getDimE internal node)

{-
\begin{description}
  \item [DimUnknown parent] does not tell anything
-}
{-
  \item [ScaleV] If the current node is the scalar, then Dim0, if it is the vector, then the same dim as the parent
-}
{-
  \item [Transpose] The dimensions are a swap of the parents; use transposeDims from HashedExpression
-}
{-
  \item [Compound] lookup the index of the input node in the list of inputs of the compound node.  Then use this index to find out which dimension from the compound node is the new one.
-}
{-
  \item [Extract] the current node is a compound, we should have dealt with this already
-}
{-
We cannot tell the dimension from the parent, but we need to check the siblings.
-}
{-
These functions have the same dimensions as their parents.
-}
{-
These should all have Dim0
-}
{-

-}
{-
For project and inject, we use functions from HashedExpression to determine what the dimensions should be.
-}
{-
\end{description}
-}
{-

a function to find children of a node
-}
getChildren :: Internal -> Node -> [Node]
getChildren internal node =
    case fromJust (I.lookup node internal) of
        Op _ _ inputs -> inputs
        _ -> []

{-

A function to return the parents of a node, for use in upInternal
-}
getParents :: Internal -> Node -> [Node]
getParents internal node = I.keys (I.filter (isParent node) internal)

isParent :: Node -> ExpressionEdge -> Bool
isParent node edge =
    case edge of
        Op _ _ inputs -> L.elem node inputs
        _ -> False

{-

A function to find the siblings of a node; useful for finding dimensions of the inputs of a dot product.  Caution! only finds siblings with a dot parent.
-}
getSiblings :: Internal -> Node -> [Node]
getSiblings internal node =
    concatMap
        (getChildren internal)
        (I.keys (I.filter (isDotParent node) internal))

isDotParent :: Node -> ExpressionEdge -> Bool
isDotParent node edge =
    case edge of
        Op _ Dot inputs -> node `elem` inputs
        _ -> False

{-


This function could potentially be useful somewhere
-}
putDims :: ExpressionEdge -> Dims -> ExpressionEdge
putDims edge newDim =
    case edge of
        Op _ opId inputs -> Op newDim opId inputs
        Var _ name -> Var newDim name
        DVar _ name -> DVar newDim name
        RelElem reArray reBoundary reIdx -> RelElem reArray reBoundary reIdx --getDimE says RelElem has Dim0, but it's not stated in the constructor
        Const _ num -> Const newDim num

{-


Functions for testing
-}
exprUpCheck (Expression top internal) =
    Expression top (upInternal (topSort internal top) internal)

exprDownCheck (Expression top internal) =
    Expression top (downInternal (topSort internal top) internal)

{-

A bunch of variables for testing.  The arrays are not square so that we can test transpose properly.
-}
[x, y, z, u, v, w] = map var ["x", "y", "z", "u", "v", "w"]

[x1, y1, z1, u1, v1, w1] = map (var1d 4) ["X1", "Y1", "Z1", "U1", "V1", "W1"]

[x2, y2, z2, u2, v2, w2] =
    map (var2d (3, 4)) ["X2", "Y2", "Z2", "U2", "V2", "W2"]

[x3, y3, z3, u3, v3, w3] =
    map (var3d (2, 3, 4)) ["X3", "Y3", "Z3", "U3", "V3", "W3"]

{-


-}
inputCheck0_0 =
    getParents
        (I.fromList
             [(120, Var Dim0 (p "x")), (8185171, Op DimUnknown Sum [120, 120])])
        120

inputCheck0_1 = getParents

{-

Testing some specific pattern matches in upDims
-}
upDims0_1 =
    upDims
        10
        (I.fromList
             [ (10, Op (CDims [DimUnknown]) (Compound 1) [5])
             , (20, Op DimUnknown (Extract 0) [10])
             , (5, Var Dim0 (p "x1"))
             ])
        [20]
        []

{-
Good, so the parent matches the child, as expected
-}
upDims0_2 =
    upDims
        10
        (I.fromList
             [ (10, Op (CDims [Dim0]) (Compound 1) [5])
             , (20, Op Dim0 (Extract 0) [10])
             , (5, Var Dim0 (p "x1"))
             ])
        [20]
        []

{-

Here is the problem; HDC.upDims extract node dim Dim1 4 matches compound child dim Dim1 4
-}
upDims0_3 =
    upDims
        10
        (I.fromList
             [ (10, Op (CDims [Dim1 4]) (Compound 1) [5])
             , (20, Op Dim0 (Extract 0) [10])
             , (5, Var Dim0 (p "x1"))
             ])
        [20]
        []

{-

this is bad; HDC.upDims extract node dim DimUnknown matches compound child dim DimUnknown
-}
upDims0_4 =
    upDims
        10
        (I.fromList
             [ (10, Op (CDims [DimUnknown]) (Compound 1) [5])
             , (20, Op Dim0 (Extract 0) [10])
             , (5, Var Dim0 (p "x1"))
             ])
        [20]
        []

{-

Let's try to build a simpler function which has the same problem.

--\begin{code}
data MyType = TypeCons Int String

foo :: MyType -> Int -> String
foo x y =
  case x of
    TypeCons z "hi"  ->
      case z of "hi"

    TypeCons z "hey" -> "hey"
    TypeCons _ _     -> "nothing"
--\end{code}

\section{Testing exprDimCheck}

Now a few test cases.  The first check is a test returning a boolean; the second return a list containing the difference between the original and result intmap.  The third returns the pretty version so that we can see what we are doing.

Test them all:
-}
regressionDimCheck =
    sequence
        [ putStrLn "dimCheck0_0"
        , print dimCheck0_0
        , putStrLn "dimCheck0_1"
        , print dimCheck0_1
        , putStrLn "dimCheck0_2"
        , print dimCheck0_2
        , putStrLn "dimCheck0_3"
        , print dimCheck0_3
        , putStrLn "dimCheck0_4"
        , print dimCheck0_4
        , putStrLn "dimCheck1_0"
        , print dimCheck1_0
        , putStrLn "dimCheck1_1"
        , print dimCheck1_1
        , putStrLn "dimCheck1_2"
        , quickCheck dimCheck1_2
        , putStrLn "dimCheck2_0"
        , print dimCheck2_0
        , putStrLn "dimCheck2_1"
        , quickCheck dimCheck2_1
        , putStrLn "dimCheck2_2"
        , print dimCheck2_2
        , putStrLn "dimCheck2_3"
        , print dimCheck2_3
        , putStrLn "dimCheck3_0"
        , print dimCheck3_0
        , putStrLn "dimCheck3_1"
        , quickCheck dimCheck3_1
        , putStrLn "dimCheck3_2"
        , quickCheck dimCheck3_2
        , putStrLn "dimCheck3_3"
        , print dimCheck3_3
        , putStrLn "dimCheck3_4"
        , print dimCheck3_4
        , putStrLn "dimCheck3_5"
        , print dimCheck3_5
        , putStrLn "dimCheck3_6"
        , print dimCheck3_6
        , putStrLn "dimCheck3_7"
        , print dimCheck3_7
        , putStrLn "dimCheck3_8"
        , print dimCheck3_8
        , putStrLn "dimCheck3_9"
        , quickCheck dimCheck3_9
        , putStrLn "dimCheck4_0"
        , print dimCheck4_0
        , putStrLn "dimCheck4_1"
        , print dimCheck4_1
                               -- putStrLn "dimCheck5_0", print dimCheck5_0,
                               -- putStrLn "dimCheck5_1", print dimCheck5_1,
                               -- putStrLn "dimCheck5_2", quickCheck dimCheck5_2,
        , putStrLn "dimCheck6_0"
        , print dimCheck6_0
        , putStrLn "dimCheck7_0"
        , print dimCheck7_0
        , putStrLn "dimCheck7_1"
        , print dimCheck7_1
        , putStrLn "dimCheck7_2"
        , print dimCheck7_2
        , putStrLn "dimCheck7_3"
        , print dimCheck7_3
        , putStrLn "dimCheck7_4"
        , print dimCheck7_4
        , putStrLn "dimCheck7_5"
        , print dimCheck7_5
                               -- putStrLn "dimCheck8_0", print dimCheck8_0
        ]

{-

\subsection{Sums}


$x+x$ where sum is unknown and x is dim0
-}
dimCheck0_0 =
    exprDimCheck
        (Expression
             8185171
             (I.fromList
                  [ (120, Var Dim0 (p "x"))
                  , (8185171, Op DimUnknown Sum [120, 120])
                  ])) ==
    Expression
        8185171
        (I.fromList [(120, Var Dim0 (p "x")), (8185171, Op Dim0 Sum [120, 120])])

dimCheck0_0_0 =
    [ Expression
          8185171
          (I.fromList
               [ (120, Var Dim0 (p "x"))
               , (8185171, Op DimUnknown Sum [120, 120])
               ])
    , exprDimCheck
          (Expression
               8185171
               (I.fromList
                    [ (120, Var Dim0 (p "x"))
                    , (8185171, Op DimUnknown Sum [120, 120])
                    ]))
    , Expression
          8185171
          (I.fromList
               [(120, Var Dim0 (p "x")), (8185171, Op Dim0 Sum [120, 120])])
    ]

{-

$x+y$ where sum and y are unknown and x is dim0; it currently fixes the sum, but not the y
-}
dimCheck0_1 =
    exprDimCheck
        (Expression
             8185171
             (I.fromList
                  [ (121, Var DimUnknown (p "y"))
                  , (120, Var Dim0 (p "x"))
                  , (8185171, Op DimUnknown Sum [120, 121])
                  ])) ==
    Expression
        8185171
        (I.fromList
             [ (121, Var Dim0 (p "y"))
             , (120, Var Dim0 (p "x"))
             , (8185171, Op Dim0 Sum [120, 121])
             ])

dimCheck0_1_0 =
    [ (Expression
           8185171
           (I.fromList
                [ (121, Var DimUnknown (p "y"))
                , (120, Var Dim0 (p "x"))
                , (8185171, Op DimUnknown Sum [120, 121])
                ]))
    , exprDimCheck
          (Expression
               8185171
               (I.fromList
                    [ (121, Var DimUnknown (p "y"))
                    , (120, Var Dim0 (p "x"))
                    , (8185171, Op DimUnknown Sum [120, 121])
                    ]))
    , Expression
          8185171
          (I.fromList
               [ (121, Var Dim0 (p "y"))
               , (120, Var Dim0 (p "x"))
               , (8185171, Op Dim0 Sum [120, 121])
               ])
    ]

dimCheck0_1_1 =
    [ pretty
          (Expression
               8185171
               (I.fromList
                    [ (121, Var DimUnknown (p "y"))
                    , (120, Var Dim0 (p "x"))
                    , (8185171, Op DimUnknown Sum [120, 121])
                    ]))
    , pretty
          (exprDimCheck
               (Expression
                    8185171
                    (I.fromList
                         [ (121, Var DimUnknown (p "y"))
                         , (120, Var Dim0 (p "x"))
                         , (8185171, Op DimUnknown Sum [120, 121])
                         ])))
    , pretty
          (Expression
               8185171
               (I.fromList
                    [ (121, Var Dim0 (p "y"))
                    , (120, Var Dim0 (p "x"))
                    , (8185171, Op Dim0 Sum [120, 121])
                    ]))
    ]

{-

$x+y+z$ where $x+y$, x and y are unknown and x is dim0
-}
dimCheck0_2 =
    exprDimCheck
        (Expression
             258093934
             (I.fromList
                  [ (120, Var DimUnknown (p "x"))
                  , (121, Var DimUnknown (p "y"))
                  , (122, Var Dim0 (p "z"))
                  , (8251232, Op DimUnknown Sum [120, 121])
                  , (258093934, Op DimUnknown Sum [120, 121, 122])
                  , (17591436965, Op DimUnknown Sum [8251232, 122])
                  ])) ==
    (Expression
         258093934
         (I.fromList
              [ (120, Var Dim0 (p "x"))
              , (121, Var Dim0 (p "y"))
              , (122, Var Dim0 (p "z"))
              , (8251232, Op Dim0 Sum [120, 121])
              , (258093934, Op Dim0 Sum [120, 121, 122])
              , (17591436965, Op Dim0 Sum [8251232, 122])
              ]))

dimCheck0_2_0 =
    [ exprDimCheck
          (Expression
               258093934
               (I.fromList
                    [ (120, Var DimUnknown (p "x"))
                    , (121, Var DimUnknown (p "y"))
                    , (122, Var Dim0 (p "z"))
                    , (8251232, Op DimUnknown Sum [120, 121])
                    , (258093934, Op DimUnknown Sum [120, 121, 122])
                    , (17591436965, Op DimUnknown Sum [8251232, 122])
                    ]))
    , (Expression
           258093934
           (I.fromList
                [ (120, Var Dim0 (p "x"))
                , (121, Var Dim0 (p "y"))
                , (122, Var Dim0 (p "z"))
                , (8251232, Op Dim0 Sum [120, 121])
                , (258093934, Op Dim0 Sum [120, 121, 122])
                , (17591436965, Op Dim0 Sum [8251232, 122])
                ]))
    ]

{-

-}
dimCheck0_3 =
    exprDimCheck
        (Expression
             258093934
             (I.fromList
                  [ (120, Var DimUnknown (p "x"))
                  , (121, Var DimUnknown (p "y"))
                  , (122, Var Dim0 (p "z"))
                  , (258093934, Op DimUnknown Sum [120, 121, 122])
                  ])) ==
    Expression
        258093934
        (I.fromList
             [ (120, Var Dim0 (p "x"))
             , (121, Var Dim0 (p "y"))
             , (122, Var Dim0 (p "z"))
             , (258093934, Op Dim0 Sum [120, 121, 122])
             ])

{-


$x+x$ where sum is unknown and x is dim0
-}
dimCheck0_4 =
    exprDimCheck
        (Expression
             8185171
             (I.fromList
                  [ (120, Var DimUnknown (p "x"))
                  , (8185171, Op Dim0 Sum [120, 120])
                  ])) ==
    Expression
        8185171
        (I.fromList [(120, Var Dim0 (p "x")), (8185171, Op Dim0 Sum [120, 120])])

dimCheck0_4_0 =
    [ Expression
          8185171
          (I.fromList
               [ (120, Var DimUnknown (p "x"))
               , (8185171, Op Dim0 Sum [120, 120])
               ])
    , exprDimCheck
          (Expression
               8185171
               (I.fromList
                    [ (120, Var DimUnknown (p "x"))
                    , (8185171, Op Dim0 Sum [120, 120])
                    ]))
    , Expression
          8185171
          (I.fromList
               [(120, Var Dim0 (p "x")), (8185171, Op Dim0 Sum [120, 120])])
    ]

{-

\subsection{Scaling} The first input must be Dim0, the second must not be Dim0

x*.y1 with x and y1 unknown and *. known
-}
dimCheck1_0 =
    exprDimCheck
        (Expression
             206131415887
             (I.fromList
                  [ (120, Var DimUnknown (p "x"))
                  , (1989048, Var DimUnknown (p "Y1"))
                  , (206131415887, Op (Dim1 4) ScaleV [120, 1989048])
                  ])) ==
    Expression
        206131415887
        (I.fromList
             [ (120, Var Dim0 (p "x"))
             , (1989048, Var (Dim1 4) (p "Y1"))
             , (206131415887, Op (Dim1 4) ScaleV [120, 1989048])
             ])

{-

x*y1 with x and *. unknown and y1 known
-}
dimCheck1_1 =
    exprDimCheck
        (Expression
             206131415887
             (I.fromList
                  [ (120, Var DimUnknown (p "x"))
                  , (1989048, Var (Dim1 4) (p "Y1"))
                  , (206131415887, Op DimUnknown ScaleV [120, 1989048])
                  ])) ==
    Expression
        206131415887
        (I.fromList
             [ (120, Var Dim0 (p "x"))
             , (1989048, Var (Dim1 4) (p "Y1"))
             , (206131415887, Op (Dim1 4) ScaleV [120, 1989048])
             ])

{-
x*y1 with x as (Dim1 1); should raise an exception
-}
dimCheck1_2 =
    exprDimCheck
        (Expression
             206131415887
             (I.fromList
                  [ (120, Var (Dim1 1) (p "x"))
                  , (1989048, Var (Dim1 4) (p "Y1"))
                  , (206131415887, Op DimUnknown ScaleV [120, 1989048])
                  ])) ==
    Expression
        206131415887
        (I.fromList
             [ (120, Var Dim0 (p "x"))
             , (1989048, Var (Dim1 4) (p "Y1"))
             , (206131415887, Op (Dim1 4) ScaleV [120, 1989048])
             ])

{-

\subsection{Transpose} Transpose only works on twoDC and threeDC; not sure why it doesn't have an instance for twoD FIXME

tranpose (x2+:y2); everything known, but just checking that this does not raise an exception
-}
dimCheck2_0 =
    exprDimCheck
        (Expression
             4000889345517
             (I.fromList
                  [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
                  , (2029639, Var (Dim2 (3, 4)) (p "Y2"))
                  , (14873194592, Op (Dim2 (3, 4)) RealImag [2029638, 2029639])
                  , ( 4000889345517
                    , Op (Dim2 (4, 3)) (Transpose SCR) [14873194592])
                  ])) ==
    Expression
        4000889345517
        (I.fromList
             [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
             , (2029639, Var (Dim2 (3, 4)) (p "Y2"))
             , (14873194592, Op (Dim2 (3, 4)) RealImag [2029638, 2029639])
             , (4000889345517, Op (Dim2 (4, 3)) (Transpose SCR) [14873194592])
             ])

{-

transpose (x2+:y2) with the input and output both being (3,4) This should break it.
-}
dimCheck2_1 =
    exprDimCheck
        (Expression
             4000889345517
             (I.fromList
                  [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
                  , (2029639, Var (Dim2 (3, 4)) (p "Y2"))
                  , (14873194592, Op (Dim2 (4, 3)) RealImag [2029638, 2029639])
                  , ( 4000889345517
                    , Op (Dim2 (4, 3)) (Transpose SCR) [14873194592])
                  ])) ==
    Expression
        4000889345517
        (I.fromList
             [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
             , (2029639, Var (Dim2 (3, 4)) (p "Y2"))
             , (14873194592, Op (Dim2 (4, 3)) RealImag [2029638, 2029639])
             , (4000889345517, Op (Dim2 (4, 3)) (Transpose SCR) [14873194592])
             ])

{-

transpose (x2+:y2) with everything unknown, except the transpose
-}
dimCheck2_2 =
    exprDimCheck
        (Expression
             4000889345517
             (I.fromList
                  [ (2029638, Var DimUnknown (p "X2"))
                  , (2029639, Var DimUnknown (p "Y2"))
                  , (14873194592, Op DimUnknown RealImag [2029638, 2029639])
                  , ( 4000889345517
                    , Op (Dim2 (4, 3)) (Transpose SCR) [14873194592])
                  ])) ==
    Expression
        4000889345517
        (I.fromList
             [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
             , (2029639, Var (Dim2 (3, 4)) (p "Y2"))
             , (14873194592, Op (Dim2 (3, 4)) RealImag [2029638, 2029639])
             , (4000889345517, Op (Dim2 (4, 3)) (Transpose SCR) [14873194592])
             ])

dimCheck2_2_0 =
    [ exprDimCheck
          (Expression
               4000889345517
               (I.fromList
                    [ (2029638, Var DimUnknown (p "X2"))
                    , (2029639, Var DimUnknown (p "Y2"))
                    , (14873194592, Op DimUnknown RealImag [2029638, 2029639])
                    , ( 4000889345517
                      , Op (Dim2 (4, 3)) (Transpose SCR) [14873194592])
                    ]))
    , Expression
          4000889345517
          (I.fromList
               [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
               , (2029639, Var (Dim2 (3, 4)) (p "Y2"))
               , (14873194592, Op (Dim2 (3, 4)) RealImag [2029638, 2029639])
               , (4000889345517, Op (Dim2 (4, 3)) (Transpose SCR) [14873194592])
               ])
    ]

{-
transpose (x2+:y2) with everything unknown, except x2
-}
dimCheck2_3 =
    exprDimCheck
        (Expression
             4000889345517
             (I.fromList
                  [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
                  , (2029639, Var DimUnknown (p "Y2"))
                  , (14873194592, Op DimUnknown RealImag [2029638, 2029639])
                  , (4000889345517, Op DimUnknown (Transpose SCR) [14873194592])
                  ])) ==
    Expression
        4000889345517
        (I.fromList
             [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
             , (2029639, Var (Dim2 (3, 4)) (p "Y2"))
             , (14873194592, Op (Dim2 (3, 4)) RealImag [2029638, 2029639])
             , (4000889345517, Op (Dim2 (4, 3)) (Transpose SCR) [14873194592])
             ])

dimCheck2_3_0 =
    [ exprDimCheck
          (Expression
               4000889345517
               (I.fromList
                    [ (2029638, Var DimUnknown (p "X2"))
                    , (2029639, Var DimUnknown (p "Y2"))
                    , (14873194592, Op DimUnknown RealImag [2029638, 2029639])
                    , ( 4000889345517
                      , Op (Dim2 (4, 3)) (Transpose SCR) [14873194592])
                    ]))
    , Expression
          4000889345517
          (I.fromList
               [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
               , (2029639, Var (Dim2 (3, 4)) (p "Y2"))
               , (14873194592, Op (Dim2 (3, 4)) RealImag [2029638, 2029639])
               , (4000889345517, Op (Dim2 (4, 3)) (Transpose SCR) [14873194592])
               ])
    ]

{-

\subsection{Compound nodes} Use funGrad to make the compound nodes

Compound (x2,y2) with everything known, just checking that this works.
-}
dimCheck3_0 =
    exprDimCheck
        (Expression
             14873194592
             (I.fromList
                  [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
                  , (2029639, Var (Dim2 (3, 4)) (p "Y2"))
                  , ( 14873194592
                    , Op (CDims [Dim2 (3, 4), Dim2 (3, 4)])
                          (Compound 2)
                          [2029638, 2029639])
                  ])) ==
    Expression
        14873194592
        (I.fromList
             [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
             , (2029639, Var (Dim2 (3, 4)) (p "Y2"))
             , ( 14873194592
               , Op (CDims [Dim2 (3, 4), Dim2 (3, 4)])
                     (Compound 2)
                     [2029638, 2029639])
             ])

{-

Compound (x2,y2) with everything known, but the wrong dims on the compound
-}
dimCheck3_1 =
    exprDimCheck
        (Expression
             14873194592
             (I.fromList
                  [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
                  , (2029639, Var (Dim2 (3, 4)) (p "Y2"))
                  , ( 14873194592
                    , Op (Dim2 (3, 4)) (Compound 2) [2029638, 2029639])
                  ])) ==
    Expression
        14873194592
        (I.fromList
             [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
             , (2029639, Var (Dim2 (3, 4)) (p "Y2"))
             , ( 14873194592
               , Op (CDims [Dim2 (3, 4), Dim2 (3, 4)])
                     (Compound 2)
                     [2029638, 2029639])
             ])

{-

Compound (x2,y2) with everything known, but the wrong dims on the compound
-}
dimCheck3_2 =
    exprDimCheck
        (Expression
             14873194592
             (I.fromList
                  [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
                  , (2029639, Var (Dim2 (3, 4)) (p "Y2"))
                  , ( 14873194592
                    , Op (CDims [Dim2 (3, 4), Dim2 (4, 3)])
                          (Compound 2)
                          [2029638, 2029639])
                  ])) ==
    Expression
        14873194592
        (I.fromList
             [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
             , (2029639, Var (Dim2 (3, 4)) (p "Y2"))
             , ( 14873194592
               , Op (CDims [Dim2 (3, 4), Dim2 (3, 4)])
                     (Compound 2)
                     [2029638, 2029639])
             ])

{-

Just making sure that this expression is okay.  Since there I'm not sure which one is the head node, we'll just apply intDimCheck
-}
dimCheck3_3 =
    intDimCheck
        (I.fromList
             [ (-8550584515149074987, Op (Dim1 4) Sum [-2445111409338921930])
             , ( -3695392959663707949
               , Op Dim0 Sum [5897069695300716, 5897069695300716])
             , ( -2445111409338921930
               , Op (Dim1 4) ScaleV [1875395714461794112, 1989047])
             , ( -1669107634722594519
               , Op Dim0 Prod [1875395714461794112, 5897069695300716])
             , (1989047, Var (Dim1 4) (p "X1"))
             , (202850972435, Op Dim0 Dot [1989047, 1989047])
             , (1850289671610, DVar (Dim1 4) (p "X1"))
             , (5897069695300716, Op Dim0 Dot [1850289671610, 1989047])
             , (1875395714461753521, Const {unConstDims = Dim0, unConst = 1.0})
             , (1875395714461794112, Const {unConstDims = Dim0, unConst = 2.0})
             , ( 5449969129541664097
               , Op Dim0 Prod [5897069695300716, 1875395714461794112])
             , ( 6387268944568490653
               , Op (CDims [Dim0, CDims [Dim1 4]])
                     (Compound 2)
                     [202850972435, 7341716085073873767])
             , ( 7341716085073873767
               , Op (CDims [Dim1 4]) (Compound 1) [-2445111409338921930])
             ]) ==
    (I.fromList
         [ (-8550584515149074987, Op (Dim1 4) Sum [-2445111409338921930])
         , ( -3695392959663707949
           , Op Dim0 Sum [5897069695300716, 5897069695300716])
         , ( -2445111409338921930
           , Op (Dim1 4) ScaleV [1875395714461794112, 1989047])
         , ( -1669107634722594519
           , Op Dim0 Prod [1875395714461794112, 5897069695300716])
         , (1989047, Var (Dim1 4) (p "X1"))
         , (202850972435, Op Dim0 Dot [1989047, 1989047])
         , (1850289671610, DVar (Dim1 4) (p "X1"))
         , (5897069695300716, Op Dim0 Dot [1850289671610, 1989047])
         , (1875395714461753521, Const {unConstDims = Dim0, unConst = 1.0})
         , (1875395714461794112, Const {unConstDims = Dim0, unConst = 2.0})
         , ( 5449969129541664097
           , Op Dim0 Prod [5897069695300716, 1875395714461794112])
         , ( 6387268944568490653
           , Op (CDims [Dim0, CDims [Dim1 4]])
                 (Compound 2)
                 [202850972435, 7341716085073873767])
         , ( 7341716085073873767
           , Op (CDims [Dim1 4]) (Compound 1) [-2445111409338921930])
         ])

{-


Just making sure that this expression is okay.
-}
dimCheck3_4 =
    exprDimCheck
        (Expression
             (-8550584515149074987)
             (I.fromList
                  [ ( -8550584515149074987
                    , Op (Dim1 4) Sum [-2445111409338921930])
                  , ( -3695392959663707949
                    , Op Dim0 Sum [5897069695300716, 5897069695300716])
                  , ( -2445111409338921930
                    , Op (Dim1 4) ScaleV [1875395714461794112, 1989047])
                  , ( -1669107634722594519
                    , Op Dim0 Prod [1875395714461794112, 5897069695300716])
                  , (1989047, Var (Dim1 4) (p "X1"))
                  , (202850972435, Op Dim0 Dot [1989047, 1989047])
                  , (1850289671610, DVar (Dim1 4) (p "X1"))
                  , (5897069695300716, Op Dim0 Dot [1850289671610, 1989047])
                  , ( 1875395714461753521
                    , Const {unConstDims = Dim0, unConst = 1.0})
                  , ( 1875395714461794112
                    , Const {unConstDims = Dim0, unConst = 2.0})
                  , ( 5449969129541664097
                    , Op Dim0 Prod [5897069695300716, 1875395714461794112])
                  , ( 6387268944568490653
                    , Op (CDims [Dim0, CDims [Dim1 4]])
                          (Compound 2)
                          [202850972435, 7341716085073873767])
                  , ( 7341716085073873767
                    , Op (CDims [Dim1 4]) (Compound 1) [-2445111409338921930])
                  ])) ==
    Expression
        (-8550584515149074987)
        (I.fromList
             [ (-8550584515149074987, Op (Dim1 4) Sum [-2445111409338921930])
             , ( -3695392959663707949
               , Op Dim0 Sum [5897069695300716, 5897069695300716])
             , ( -2445111409338921930
               , Op (Dim1 4) ScaleV [1875395714461794112, 1989047])
             , ( -1669107634722594519
               , Op Dim0 Prod [1875395714461794112, 5897069695300716])
             , (1989047, Var (Dim1 4) (p "X1"))
             , (202850972435, Op Dim0 Dot [1989047, 1989047])
             , (1850289671610, DVar (Dim1 4) (p "X1"))
             , (5897069695300716, Op Dim0 Dot [1850289671610, 1989047])
             , (1875395714461753521, Const {unConstDims = Dim0, unConst = 1.0})
             , (1875395714461794112, Const {unConstDims = Dim0, unConst = 2.0})
             , ( 5449969129541664097
               , Op Dim0 Prod [5897069695300716, 1875395714461794112])
             , ( 6387268944568490653
               , Op (CDims [Dim0, CDims [Dim1 4]])
                     (Compound 2)
                     [202850972435, 7341716085073873767])
             , ( 7341716085073873767
               , Op (CDims [Dim1 4]) (Compound 1) [-2445111409338921930])
             ])

{-



Unknown compound nodes, everything else is known.
-}
dimCheck3_5 =
    exprDimCheck
        (Expression
             (-8550584515149074987)
             (I.fromList
                  [ ( -8550584515149074987
                    , Op (Dim1 4) Sum [-2445111409338921930])
                  , ( -3695392959663707949
                    , Op Dim0 Sum [5897069695300716, 5897069695300716])
                  , ( -2445111409338921930
                    , Op (Dim1 4) ScaleV [1875395714461794112, 1989047])
                  , ( -1669107634722594519
                    , Op Dim0 Prod [1875395714461794112, 5897069695300716])
                  , (1989047, Var (Dim1 4) (p "X1"))
                  , (202850972435, Op Dim0 Dot [1989047, 1989047])
                  , (1850289671610, DVar (Dim1 4) (p "X1"))
                  , (5897069695300716, Op Dim0 Dot [1850289671610, 1989047])
                  , ( 1875395714461753521
                    , Const {unConstDims = Dim0, unConst = 1.0})
                  , ( 1875395714461794112
                    , Const {unConstDims = Dim0, unConst = 2.0})
                  , ( 5449969129541664097
                    , Op Dim0 Prod [5897069695300716, 1875395714461794112])
                  , ( 6387268944568490653
                    , Op DimUnknown
                          (Compound 2)
                          [202850972435, 7341716085073873767])
                  , ( 7341716085073873767
                    , Op DimUnknown (Compound 1) [-2445111409338921930])
                  ])) ==
    Expression
        (-8550584515149074987)
        (I.fromList
             [ (-8550584515149074987, Op (Dim1 4) Sum [-2445111409338921930])
             , ( -3695392959663707949
               , Op Dim0 Sum [5897069695300716, 5897069695300716])
             , ( -2445111409338921930
               , Op (Dim1 4) ScaleV [1875395714461794112, 1989047])
             , ( -1669107634722594519
               , Op Dim0 Prod [1875395714461794112, 5897069695300716])
             , (1989047, Var (Dim1 4) (p "X1"))
             , (202850972435, Op Dim0 Dot [1989047, 1989047])
             , (1850289671610, DVar (Dim1 4) (p "X1"))
             , (5897069695300716, Op Dim0 Dot [1850289671610, 1989047])
             , (1875395714461753521, Const {unConstDims = Dim0, unConst = 1.0})
             , (1875395714461794112, Const {unConstDims = Dim0, unConst = 2.0})
             , ( 5449969129541664097
               , Op Dim0 Prod [5897069695300716, 1875395714461794112])
             , ( 6387268944568490653
               , Op (CDims [Dim0, CDims [Dim1 4]])
                     (Compound 2)
                     [202850972435, 7341716085073873767])
             , ( 7341716085073873767
               , Op (CDims [Dim1 4]) (Compound 1) [-2445111409338921930])
             ])

{-
Known compound nodes, everything else is unknown.
-}
dimCheck3_6 =
    exprDimCheck
        (Expression
             (-8550584515149074987)
             (I.fromList
                  [ ( -8550584515149074987
                    , Op DimUnknown Sum [-2445111409338921930])
                  , ( -3695392959663707949
                    , Op DimUnknown Sum [5897069695300716, 5897069695300716])
                  , ( -2445111409338921930
                    , Op DimUnknown ScaleV [1875395714461794112, 1989047])
                  , ( -1669107634722594519
                    , Op DimUnknown Prod [1875395714461794112, 5897069695300716])
                  , (1989047, Var DimUnknown (p "X1"))
                  , (202850972435, Op DimUnknown Dot [1989047, 1989047])
                  , (1850289671610, DVar DimUnknown (p "X1"))
                  , ( 5897069695300716
                    , Op DimUnknown Dot [1850289671610, 1989047])
                  , ( 1875395714461753521
                    , Const {unConstDims = DimUnknown, unConst = 1.0})
                  , ( 1875395714461794112
                    , Const {unConstDims = DimUnknown, unConst = 2.0})
                  , ( 5449969129541664097
                    , Op DimUnknown Prod [5897069695300716, 1875395714461794112])
                  , ( 6387268944568490653
                    , Op (CDims [Dim0, CDims [Dim1 4]])
                          (Compound 2)
                          [202850972435, 7341716085073873767])
                  , ( 7341716085073873767
                    , Op (CDims [Dim1 4]) (Compound 1) [-2445111409338921930])
                  ])) ==
    Expression
        (-8550584515149074987)
        (I.fromList
             [ (-8550584515149074987, Op (Dim1 4) Sum [-2445111409338921930])
             , ( -3695392959663707949
               , Op Dim0 Sum [5897069695300716, 5897069695300716])
             , ( -2445111409338921930
               , Op (Dim1 4) ScaleV [1875395714461794112, 1989047])
             , ( -1669107634722594519
               , Op Dim0 Prod [1875395714461794112, 5897069695300716])
             , (1989047, Var (Dim1 4) (p "X1"))
             , (202850972435, Op Dim0 Dot [1989047, 1989047])
             , (1850289671610, DVar (Dim1 4) (p "X1"))
             , (5897069695300716, Op Dim0 Dot [1850289671610, 1989047])
             , (1875395714461753521, Const {unConstDims = Dim0, unConst = 1.0})
             , (1875395714461794112, Const {unConstDims = Dim0, unConst = 2.0})
             , ( 5449969129541664097
               , Op Dim0 Prod [5897069695300716, 1875395714461794112])
             , ( 6387268944568490653
               , Op (CDims [Dim0, CDims [Dim1 4]])
                     (Compound 2)
                     [202850972435, 7341716085073873767])
             , ( 7341716085073873767
               , Op (CDims [Dim1 4]) (Compound 1) [-2445111409338921930])
             ])

{-

Assuming that the indexing of extract starts at 0.

Extract 0 [Compound 2 ("T1","T2")]; I think this should work
-}
dimCheck3_7 =
    exprDimCheck
        (Expression
             100
             (I.fromList
                  [ (100, Op DimUnknown (Extract 0) [50])
                  , (50, Op DimUnknown (Compound 2) [10, 20])
                  , (10, Var (Dim2 (15, 16)) (p "T1"))
                  , (20, DVar (Dim2 (22, 16)) (p "T2"))
                  ])) ==
    Expression
        100
        (I.fromList
             [ (100, Op (Dim2 (15, 16)) (Extract 0) [50])
             , ( 50
               , Op (CDims [Dim2 (15, 16), Dim2 (22, 16)]) (Compound 2) [10, 20])
             , (10, Var (Dim2 (15, 16)) (p "T1"))
             , (20, DVar (Dim2 (22, 16)) (p "T2"))
             ])

{-

-}
dimCheck3_8 =
    exprDimCheck
        (Expression
             100
             (I.fromList
                  [ (100, Op DimUnknown (Extract 1) [50])
                  , (50, Op DimUnknown (Compound 2) [10, 20])
                  , (10, Var (Dim2 (15, 16)) (p "T2"))
                  , (20, DVar (Dim2 (22, 16)) (p "T2"))
                  ])) ==
    Expression
        100
        (I.fromList
             [ (100, Op (Dim2 (15, 16)) (Extract 1) [50])
             , ( 50
               , Op (CDims [Dim2 (15, 16), Dim2 (22, 16)]) (Compound 2) [10, 20])
             , (10, Var (Dim2 (15, 16)) (p "T2"))
             , (20, DVar (Dim2 (22, 16)) (p "T2"))
             ])

dimCheck3_8_0 =
    [ exprDimCheck
          (Expression
               100
               (I.fromList
                    [ (100, Op DimUnknown (Extract 1) [50])
                    , (50, Op DimUnknown (Compound 2) [10, 20])
                    , (10, Var (Dim2 (15, 16)) (p "T2"))
                    , (20, DVar (Dim2 (22, 16)) (p "T2"))
                    ]))
    , Expression
          100
          (I.fromList
               [ (100, Op (Dim2 (15, 16)) (Extract 1) [50])
               , ( 50
                 , Op (CDims [Dim2 (15, 16), Dim2 (22, 16)])
                       (Compound 2)
                       [10, 20])
               , (10, Var (Dim2 (15, 16)) (p "T2"))
               , (20, DVar (Dim2 (22, 16)) (p "T2"))
               ])
    ]

{-

Extract (Compound (T1,T2))
-}
dimCheck3_9 =
    exprDimCheck
        (Expression
             100
             (I.fromList
                  [ (100, Op DimUnknown (Extract 2) [50])
                  , (50, Op DimUnknown (Compound 2) [10, 20])
                  , (10, Var (Dim2 (15, 16)) (p "T1"))
                  , (20, DVar (Dim2 (22, 16)) (p "T2"))
                  ])) ==
    Expression
        100
        (I.fromList
             [ (100, Op DimUnknown (Extract 2) [50])
             , (50, Op DimUnknown (Compound 2) [10, 20])
             , (10, Var (Dim2 (15, 16)) (p "T1"))
             , (20, DVar (Dim2 (22, 16)) (p "T2"))
             ])

{-


\subsection{Dot Product}
x2<.>y2 with x2 know and everything else unknown.
-}
dimCheck4_0 =
    exprDimCheck
        (Expression
             206990703776
             (I.fromList
                  [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
                  , (2029639, Var DimUnknown (p "Y2"))
                  , (206990703776, Op DimUnknown Dot [2029638, 2029639])
                  ])) ==
    Expression
        206990703776
        (I.fromList
             [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
             , (2029639, Var (Dim2 (3, 4)) (p "Y2"))
             , (206990703776, Op Dim0 Dot [2029638, 2029639])
             ])

dimCheck4_0_0 =
    I.difference
        (I.fromList
             [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
             , (2029639, Var DimUnknown (p "Y2"))
             , (206990703776, Op DimUnknown Dot [2029638, 2029639])
             ])
        (I.fromList
             [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
             , (2029639, Var (Dim2 (3, 4)) (p "Y2"))
             , (206990703776, Op Dim0 Dot [2029638, 2029639])
             ])

{-

Just checking that this doesn't raise an error.  Also, making sure that DVars work.
-}
dimCheck4_1 =
    exprDimCheck (unScalar (diff [p "X1"] (x1 <.> x1))) ==
    unScalar (diff [p "X1"] (x1 <.> x1))

{-

\subsection{SCZ Expression}

Nothing fancy, just making sure that this expression doesn't cause any issues.
-}
-- dimCheck5_0 = exprDimCheck (unTwoDC (cplxMult (x2 +: y2) (z2 +: w2))) == unTwoDC (cplxMult (x2 +: y2) (z2 +: w2))
{-

Everything is unknown except the operation to take the imaginary part of a complex number.
-}
-- dimCheck5_1 = exprDimCheck (Expression (-2455513033910336527) (I.fromList [(-7321471985049872260,Op DimUnknown (SCZ (Expression 2547278150439635795 (I.fromList [(479001599,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(37354474221766,Op DimUnknown Prod [479001599,479001778]),(37354502140038,Op DimUnknown Prod [479001957,479002136]),(2547278150439635795,Op DimUnknown Sum [37354474221766,37354502140038])]))) [3316722394239,3376212053631,3376215172611,3316719330219]),(-2455513033910336527,Op DimUnknown RealImag [1888520379435133720,-7321471985049872260]),(2029637,Var DimUnknown (p "W2")),(2029638,Var DimUnknown (p "X2")),(2029639,Var DimUnknown (p "Y2")),(2029640,Var DimUnknown (p "Z2")),(14873180852,Op DimUnknown RealImag [2029640,2029637]),(14873194592,Op DimUnknown RealImag [2029638,2029639]),(3316719330219,Op DimUnknown RealPart [14873180852]),(3316722394239,Op DimUnknown RealPart [14873194592]),(3376212053631,Op (Dim2 (3,4)) ImagPart [14873180852]),(3376215172611,Op DimUnknown ImagPart [14873194592]),(1888520379435133720,Op DimUnknown (SCZ (Expression (-4396327147522206377) (I.fromList [(-5525346519600307196,Const {unConstDims = DimUnknown, unConst = -1.0}),(-4396327147522206377,Op DimUnknown Sum [37354474221766,8754323996387218842]),(479001599,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(37354474221766,Op DimUnknown Prod [479001599,479001778]),(8754323996387218842,Op DimUnknown Prod [479001957,479002136,-5525346519600307196])]))) [3316722394239,3316719330219,3376215172611,3376212053631])])) == unTwoDC (cplxMult (x2 +: y2) (z2 +: w2))
-- dimCheck5_1_0 = [exprDimCheck (Expression (-2455513033910336527) (I.fromList [(-7321471985049872260,Op DimUnknown (SCZ (Expression 2547278150439635795 (I.fromList [(479001599,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(37354474221766,Op DimUnknown Prod [479001599,479001778]),(37354502140038,Op DimUnknown Prod [479001957,479002136]),(2547278150439635795,Op DimUnknown Sum [37354474221766,37354502140038])]))) [3316722394239,3376212053631,3376215172611,3316719330219]),(-2455513033910336527,Op DimUnknown RealImag [1888520379435133720,-7321471985049872260]),(2029637,Var DimUnknown (p "W2")),(2029638,Var DimUnknown (p "X2")),(2029639,Var DimUnknown (p "Y2")),(2029640,Var DimUnknown (p "Z2")),(14873180852,Op DimUnknown RealImag [2029640,2029637]),(14873194592,Op DimUnknown RealImag [2029638,2029639]),(3316719330219,Op DimUnknown RealPart [14873180852]),(3316722394239,Op DimUnknown RealPart [14873194592]),(3376212053631,Op (Dim2 (3,4)) ImagPart [14873180852]),(3376215172611,Op DimUnknown ImagPart [14873194592]),(1888520379435133720,Op DimUnknown (SCZ (Expression (-4396327147522206377) (I.fromList [(-5525346519600307196,Const {unConstDims = DimUnknown, unConst = -1.0}),(-4396327147522206377,Op DimUnknown Sum [37354474221766,8754323996387218842]),(479001599,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(37354474221766,Op DimUnknown Prod [479001599,479001778]),(8754323996387218842,Op DimUnknown Prod [479001957,479002136,-5525346519600307196])]))) [3316722394239,3316719330219,3376215172611,3376212053631])])), unTwoDC (cplxMult (x2 +: y2) (z2 +: w2))]
{-

Raises an exception because the dimensions do not match.
-}
-- dimCheck5_2 = exprDimCheck (Expression (-2455513033910336527) (I.fromList [(-7321471985049872260,Op (Dim2 (3,4)) (SCZ (Expression 2547278150439635795 (I.fromList [(479001599,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(37354474221766,Op DimUnknown Prod [479001599,479001778]),(37354502140038,Op DimUnknown Prod [479001957,479002136]),(2547278150439635795,Op DimUnknown Sum [37354474221766,37354502140038])]))) [3316722394239,3376212053631,3376215172611,3316719330219]),(-2455513033910336527,Op DimUnknown RealImag [1888520379435133720,-7321471985049872260]),(2029637,Var DimUnknown (p "W2")),(2029638,Var (Dim2 (4,3)) (p "X2")),(2029639,Var DimUnknown (p "Y2")),(2029640,Var DimUnknown (p "Z2")),(14873180852,Op DimUnknown RealImag [2029640,2029637]),(14873194592,Op DimUnknown RealImag [2029638,2029639]),(3316719330219,Op DimUnknown RealPart [14873180852]),(3316722394239,Op DimUnknown RealPart [14873194592]),(3376212053631,Op DimUnknown ImagPart [14873180852]),(3376215172611,Op DimUnknown ImagPart [14873194592]),(1888520379435133720,Op DimUnknown (SCZ (Expression (-4396327147522206377) (I.fromList [(-5525346519600307196,Const {unConstDims = DimUnknown, unConst = -1.0}),(-4396327147522206377,Op DimUnknown Sum [37354474221766,8754323996387218842]),(479001599,RelElem {reArray = 0, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001778,RelElem {reArray = 1, reBoundary = ZeroMargin, reIdx = [0,0]}),(479001957,RelElem {reArray = 2, reBoundary = ZeroMargin, reIdx = [0,0]}),(479002136,RelElem {reArray = 3, reBoundary = ZeroMargin, reIdx = [0,0]}),(37354474221766,Op DimUnknown Prod [479001599,479001778]),(8754323996387218842,Op DimUnknown Prod [479001957,479002136,-5525346519600307196])]))) [3316722394239,3316719330219,3376215172611,3376212053631])])) == unTwoDC (cplxMult (x2 +: y2) (z2 +: w2))
{-


\subsection{Scalar Product}

A scalar product must always have Dim0, so we can make the entire expression unknown
-}
dimCheck6_0 =
    exprDimCheck
        (Expression
             9436064
             (I.fromList
                  [ (120, Var DimUnknown (p "x"))
                  , (121, Var DimUnknown (p "y"))
                  , (9436064, Op DimUnknown Prod [120, 121])
                  ])) ==
    Expression
        9436064
        (I.fromList
             [ (120, Var Dim0 (p "x"))
             , (121, Var Dim0 (p "y"))
             , (9436064, Op Dim0 Prod [120, 121])
             ])

{-


-}
subA = SSNyquist [(3, (4, 5))]

subB = SSNyquist [(6, (7, 8))]

subL = SSNyquist [(1, (2, 3)), (2, (3, 4))]

subM = SSNyquist [(1, (1, 3)), (1, (1, 3))]

subN = SSNyquist [(3, (4, 5)), (6, (7, 8)), (9, (10, 11))]

subW = SSCrop [(1, 3), (0, 3)] [3, 4]

subX = SSCrop [(2, 3), (0, 3)] [3, 4]

{-

\subsection{Projections and Injections}

Just checking that it doesn't raise any exceptions.
-}
dimCheck7_0 = exprDimCheck (unTwoD (projSS subX x2)) == unTwoD (projSS subX x2)

dimCheck7_1 =
    exprDimCheck (unTwoD (projSS subX x2)) ==
    Expression
        75096643
        (I.fromList
             [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
             , ( 75096643
               , Op (Dim2 (2, 4))
                     (Project (SSCrop [(2, 3), (0, 3)] [3, 4]))
                     [2029638])
             ])

dimCheck7_2 =
    exprDimCheck (unTwoD (projSS subW x2)) ==
    Expression
        75096643
        (I.fromList
             [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
             , ( 75096643
               , Op (Dim2 (3, 4))
                     (Project (SSCrop [(1, 3), (0, 3)] [3, 4]))
                     [2029638])
             ])

{-
7\_4 should raise an error; actually, it kind of does in checkInj, but it's only trace.  Should it actually be an error? Probably.
-}
dimCheck7_3 =
    exprDimCheck
        (Expression
             75096643
             (I.fromList
                  [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
                  , ( 75096643
                    , Op (Dim2 (2, 4))
                          (Project (SSCrop [(1, 3), (0, 3)] [3, 4]))
                          [2029638])
                  ]))

dimCheck7_4 =
    exprDimCheck
        (Expression
             75096643
             (I.fromList
                  [ (2029638, Var DimUnknown (p "X2"))
                  , ( 75096643
                    , Op (Dim2 (2, 4))
                          (Project (SSCrop [(1, 3), (0, 3)] [3, 4]))
                          [2029638])
                  ]))

dimCheck7_5 =
    exprDimCheck
        (Expression
             75096643
             (I.fromList
                  [ (2029638, Var (Dim2 (3, 4)) (p "X2"))
                  , ( 75096643
                    , Op DimUnknown
                          (Project (SSCrop [(1, 3), (0, 3)] [3, 4]))
                          [2029638])
                  ]))

{-

\subsection{R2Star Check}

Just checking to make sure that fitToData is a valid expression; I don't actually feel like making more example with fitToData
-}
-- dimCheck8_0 = exprDimCheck (unScalar fitToData) == unScalar fitToData
{-

Okay, so this doesn't re-hash properly.  Should it? FIXME
-}
dimCheck9_0 =
    exprDimCheck
        (Expression
             8185171
             (I.fromList
                  [ (120, Var DimUnknown (p "x"))
                  , (8185171, Op DimUnknown Sum [120, 121])
                  , (121, Var Dim0 (p "x"))
                  ])) {-
\end{document}


-}
