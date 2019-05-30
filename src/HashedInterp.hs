{-
Interpretation of Hashed Expressions.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HashedInterp where

import HashedDerivative ()
import HashedExpression
import HashedInstances ()

import Data.Array.Unboxed as U

--import Maybe (isJust)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import HashedUtils

import Data.Complex as DC

--import qualified Data.Array.IArray as IA
import Debug.Trace

noWarn = trace "noWarn"

{-

Collect value tables of different dimensions.
-}
data ValMaps =
    ValMaps
        { vm0 :: Map ByteString Double
        , vm1 :: Map ByteString (U.UArray Int Double)
        , vm2 :: Map ByteString (U.UArray (Int, Int) Double)
        , vm3 :: Map ByteString (U.UArray (Int, Int, Int) Double)
        , vm4 :: Map ByteString (U.UArray (Int, Int, Int, Int) Double)
        }
    deriving (Eq, Show, Ord)

{-

SubsType provides a text based interface for the variable value map,
and subs converts those into the internal expression representations.
(I'm not sure why subs is written the way it is, seems unnecessarily verbose).

genSubs will generate the text based substitutions and then map subs over the list
to becomes a generator of usable value substitutions.

For now, only scalar values are provided, this will be corrected shortly.
-}
data SubsArray a b
    = SubsDense a
    | SubsSparse [a]

type SubsType
     = ( [(String, Double)]
       , [(String, U.UArray Int Double)]
       , [(String, U.UArray (Int, Int) Double)]
       , [(String, U.UArray (Int, Int, Int) Double)]
       , [(String, U.UArray (Int, Int, Int, Int) Double)])

type SubsListType
     = ( [(String, Double)]
       , [(String, [Double])]
       , [(String, [[Double]])]
       , [(String, [[[Double]]])]
       , [(String, [[[[Double]]]])])

class Convert a b | a -> b, b -> a where
    a2b :: a -> b
    b2a :: b -> a

instance Convert [Double] (U.UArray Int Double) where
    a2b list = listArray (0, length list - 1) list
    b2a array = elems array

instance Convert [[Double]] (U.UArray (Int, Int) Double) where
    a2b list =
        listArray ((0, 0), (length (head list) - 1, length list - 1)) $
        concat list
    b2a array =
        let ((x0, y0), (x1, y1)) = bounds array
         in [[array ! (x, y) | x <- [x0 .. x1]] | y <- [y0 .. y1]]

instance Convert [[[Double]]] (U.UArray (Int, Int, Int) Double) where
    a2b list =
        listArray
            ( (0, 0, 0)
            , ( length (head (head list)) - 1
              , length (head list) - 1
              , length list - 1)) $
        concat $ concat list
    b2a array =
        let ((x0, y0, z0), (x1, y1, zz1)) = bounds array
         in [ [[array ! (x, y, z) | x <- [x0 .. x1]] | y <- [y0 .. y1]]
            | z <- [z0 .. zz1]
            ]

instance Convert [[[[Double]]]] (U.UArray (Int, Int, Int, Int) Double) where
    a2b list =
        listArray
            ( (0, 0, 0, 0)
            , ( length (head (head (head list))) - 1
              , length (head (head list)) - 1
              , length (head list) - 1
              , length list - 1)) $
        concat $ concat $ concat list
    b2a array =
        let ((x0, y0, z0, t0), (x1, y1, zz1, t1)) = bounds array
         in [ [ [[array ! (x, y, z, t) | x <- [x0 .. x1]] | y <- [y0 .. y1]]
            | z <- [z0 .. zz1]
            ]
            | t <- [t0 .. t1]
            ]

subs :: SubsType -> ValMaps
subs (l0, l1, l2, l3, l4) =
    ValMaps
        (Map.fromList $ map (\(x, y) -> (C.pack x, y)) l0)
        (Map.fromList $ map (\(x, y) -> (C.pack x, y)) l1)
        (Map.fromList $ map (\(x, y) -> (C.pack x, y)) l2)
        (Map.fromList $ map (\(x, y) -> (C.pack x, y)) l3)
        (Map.fromList $ map (\(x, y) -> (C.pack x, y)) l4)

subsList :: SubsListType -> ValMaps
subsList (l0, l1, l2, l3, l4) =
    ValMaps
        (Map.fromList $ map (\(x, y) -> (C.pack x, y)) l0)
        (Map.fromList $
         map (\(x, y) -> (C.pack x, listArray (0, length y - 1) y)) l1)
        (Map.fromList $ map (\(x, y) -> (C.pack x, a2b y)) l2)
        (Map.fromList $ map (\(x, y) -> (C.pack x, a2b y)) l3)
        (Map.fromList $ map (\(x, y) -> (C.pack x, a2b y)) l4)

{-
Some sample values
-}
valx1 = [0 .. 16 ^ 1 - 1] :: [Double]

valx2 = [0 .. 16 ^ 2 - 1] :: [Double]

valx3 = [0 .. 16 ^ 3 - 1] :: [Double]

valx4 =
    [ if (i == (7 ^ 2))
        then 1
        else 1
    | i <- [0 .. 16 ^ 2 - 1]
    ] :: [Double]

valy1 = (replicate (16 ^ 1) 1) :: [Double]

valy2 = (replicate (16 ^ 2) 1) :: [Double]

valy3 = (replicate (16 ^ 3) 1) :: [Double]

{-
Some sample values
-}
subs1a =
    subs
        ( [("x", 1), ("y", 2), ("z", 3), ("u", 7), ("v", 11)]
        , [ ("X1", U.listArray (0, 15) [10000 + i | i <- [0 .. 15]])
          , ("Y1", U.listArray (0, 15) [20000 + i | i <- [0 .. 15]])
          , ("U1", U.listArray (0, 15) [30000 + i | i <- [0 .. 15]])
          , ("V1", U.listArray (0, 15) [40000 + i | i <- [0 .. 15]])
          ]
        , []
        , []
        , [])

subs2a =
    subs
        ( [("x", 1), ("y", 2), ("z", 3), ("u", 7), ("v", 11)]
        , []
        , [ ( "X2"
            , U.listArray
                  ((0, 0), (15, 15))
                  [10000 + i + 1000 * j | i <- [0 .. 15], j <- [0 .. 15]])
          , ( "Y2"
            , U.listArray
                  ((0, 0), (15, 15))
                  [20000 + i + 1000 * j | i <- [0 .. 15], j <- [0 .. 15]])
          , ( "U2"
            , U.listArray
                  ((0, 0), (15, 15))
                  [30000 + i + 1000 * j | i <- [0 .. 15], j <- [0 .. 15]])
          , ( "V2"
            , U.listArray
                  ((0, 0), (15, 15))
                  [40000 + i + 1000 * j | i <- [0 .. 15], j <- [0 .. 15]])
          ]
        , []
        , [])

{-

Generic Evaluation.
CKA: if we want to map evaluation over a whole graph, we need a generic evaluator.
     Does this work, or do we need TypeFamilies
-}
--CKA:  in the SZC complex case, we probably want the relative map to have complex scalar elements, which is going to complicate the type
class GenericEval une e | une -> e where
    eval :: ValMaps -> une -> e
    evalRel ::
           Maybe (ExpressionEdge -> Double)
        -> ValMaps
        -> Internal {-must be RelElem-}
        -> Node
        -> e

instance GenericEval Scalar Double where
    eval valMaps (Scalar e) = evalScalar (Scalar e) valMaps
    evalRel relMap valMaps exprs n =
        evalZeroD relMap (Expression n exprs) valMaps

instance GenericEval ScalarC (DC.Complex Double) where
    eval valMaps (ScalarC e) = evalScalarC (ScalarC e) valMaps
    evalRel relMap valMaps exprs n =
        evalZeroDC relMap (Expression n exprs) valMaps

instance GenericEval OneD (U.UArray Int Double) where
    eval valMaps (OneD e) = evalOneD (OneD e) valMaps
    evalRel Nothing valMaps exprs n = evalOneD (mkOneD exprs n) valMaps
    evalRel _ _ exprs n =
        error $ "HI.evalRel[OneD] Just relMap " ++ show (Expression n exprs)

instance GenericEval OneDC (Array Int (DC.Complex Double)) where
    eval valMaps (OneDC e) = evalOneDC (OneDC e) valMaps
    evalRel Nothing valMaps exprs n = evalOneDC (mkOneDC exprs n) valMaps
    evalRel _ _ exprs n =
        error $ "HI.evalRel[OneDC] Just relMap " ++ show (Expression n exprs)

instance GenericEval TwoD (U.UArray (Int, Int) Double) where
    eval valMaps (TwoD e) = evalTwoD (TwoD e) valMaps
    evalRel Nothing valMaps exprs n = evalTwoD (mkTwoD exprs n) valMaps
    evalRel _ _ exprs n =
        error $ "HI.evalRel[TwoD] Just relMap " ++ show (Expression n exprs)

instance GenericEval TwoDSparse (Map Int (U.UArray Int Double)) where
    eval valMaps (TwoDSparse sL e) = evalTwoDSparse (TwoDSparse sL e) valMaps
    evalRel Nothing valMaps exprs n =
        evalTwoDSparse (mkTwoDSparse exprs n) valMaps
    evalRel _ _ exprs n =
        error $
        "HI.evalRel[TwoDSparse] Just relMap " ++ show (Expression n exprs)

instance GenericEval TwoDC (Array (Int, Int) (DC.Complex Double)) where
    eval valMaps (TwoDC e) = evalTwoDC (TwoDC e) valMaps
    evalRel Nothing valMaps exprs n = evalTwoDC (mkTwoDC exprs n) valMaps
    evalRel _ _ exprs n =
        error $ "HI.evalRel[TwoDC] Just relMap " ++ show (Expression n exprs)

instance GenericEval TwoDCSparse (Map Int (Array Int (DC.Complex Double))) where
    eval valMaps (TwoDCSparse sL e) = evalTwoDCSparse (TwoDCSparse sL e) valMaps
    evalRel Nothing valMaps exprs n =
        evalTwoDCSparse (mkTwoDCSparse exprs n) valMaps
    evalRel _ _ exprs n =
        error $
        "HI.evalRel[TwoDSparse] Just relMap " ++ show (Expression n exprs)

instance GenericEval ThreeD (U.UArray (Int, Int, Int) Double) where
    eval valMaps (ThreeD e) = evalThreeD (ThreeD e) valMaps
    evalRel Nothing valMaps exprs n = evalThreeD (mkThreeD exprs n) valMaps
    evalRel _ _ exprs n =
        error $ "HI.evalRel[ThreeD] Just relMap " ++ show (Expression n exprs)

instance GenericEval ThreeDSparse (Map (Int, Int) (U.UArray Int Double)) where
    eval valMaps (ThreeDSparse sL e) =
        evalThreeDSparse (ThreeDSparse sL e) valMaps
    evalRel Nothing valMaps exprs n =
        evalThreeDSparse (mkThreeDSparse exprs n) valMaps
    evalRel _ _ exprs n =
        error $
        "HI.evalRel[ThreeDSparse] Just relMap " ++ show (Expression n exprs)

instance GenericEval ThreeDC (Array (Int, Int, Int) (DC.Complex Double)) where
    eval valMaps (ThreeDC e) = evalThreeDC (ThreeDC e) valMaps
    evalRel Nothing valMaps exprs n = evalThreeDC (mkThreeDC exprs n) valMaps
    evalRel _ _ exprs n =
        error $ "HI.evalRel[ThreeDC] Just relMap " ++ show (Expression n exprs)

instance GenericEval ThreeDCSparse (Map (Int, Int) (Array Int (DC.Complex Double))) where
    eval valMaps (ThreeDCSparse sL e) =
        evalThreeDCSparse (ThreeDCSparse sL e) valMaps
    evalRel Nothing valMaps exprs n =
        evalThreeDCSparse (mkThreeDCSparse exprs n) valMaps
    evalRel _ _ exprs n =
        error $
        "HI.evalRel[ThreeDSparse] Just relMap " ++ show (Expression n exprs)

{-

Evaluate a scalar expression given a map of variable values.
-}
evalScalar :: Scalar -> ValMaps -> Double
evalScalar (Scalar e) eMap = evalZeroD Nothing e eMap

evalZeroD :: Maybe (ExpressionEdge -> Double) -> Expression -> ValMaps -> Double {-must be RelElem-}
evalZeroD relEval (Expression node exprs) eMap =
    let evalZeroD' (_, next) = evalZeroD relEval (Expression next exprs) eMap
     in case I.lookup node exprs of
            Nothing ->
                error $
                "evalZeroD didn't find node " ++ (take 200 $ show (node, exprs))
            Just (Var Dim0 name) ->
                case Map.lookup name (vm0 eMap) of
                    Nothing -> error $ "evalZeroD didn't find var " {-++ show (var,eMap)-}
                    Just x -> x
            Just e@(Var _dims _name) ->
                error $ "evalZeroD found nonscalar " ++ show e
            Just e@(DVar _ _) -> error $ "evalZeroD found nonscalar " ++ show e
            Just e@(RelElem _ _ _) ->
                case relEval of
                    Nothing -> error $ "evalZeroD found nonscalar " ++ show e
                    Just fun -> fun e
            Just (Const Dim0 d) -> d
            Just (Const dims d) -> error $ "evalZeroD got " ++ show (dims, d)
            Just (Op _dims op inputs) ->
                let op1 fun name =
                        case inputs of
                            [x] -> fun $ evalZeroD' ([], x)
                            _ ->
                                error $
                                "evalZeroD " ++ name ++ " " ++ show inputs
                    op2 fun name =
                        case inputs of
                            [x, y] ->
                                fun (evalZeroD' ([], x)) (evalZeroD' ([], y))
                            _ ->
                                error $
                                "evalZeroD " ++ name ++ " " ++ show inputs
                 in case op of
                        Sum -> sum $ map (\x -> evalZeroD' ([], x)) inputs
                        Abs -> op1 abs "abs"
                        Signum -> op1 signum "signum"
                        Prod ->
                            foldr (*) 1 $ map (\x -> evalZeroD' ([], x)) inputs
                        Div -> op2 (/) "divide"
                        Sqrt -> op1 sqrt "sqrt"
                        Sin -> op1 sin "sin"
                        Cos -> op1 cos "cos"
                        Tan -> op1 tan "tan"
                        Exp -> op1 exp "exp"
                        Log -> op1 log "log"
                        Sinh -> op1 sinh "sinh"
                        Cosh -> op1 cosh "cosh"
                        Tanh -> op1 tanh "tanh"
                        Asin -> op1 asin "asin"
                        Acos -> op1 acos "acos"
                        Atan -> op1 atan "atan"
                        Asinh -> op1 asinh "asinh"
                        Acosh -> op1 acosh "acosh"
                        Atanh -> op1 atanh "atanh"
                        Dot ->
                            let [x, y] = inputs
                                dimsX = getDimE exprs x
                                dimsY = getDimE exprs y
                             in case (dimsX, dimsY) of
                                    (Dim1 dim1, Dim1 dim2) ->
                                        if dim1 == dim2
                                            then sum $
                                                 zipWith
                                                     (*)
                                                     (elems
                                                          (evalOneD
                                                               (OneD $
                                                                Expression
                                                                    x
                                                                    exprs)
                                                               eMap))
                                                     (elems
                                                          (evalOneD
                                                               (OneD $
                                                                Expression
                                                                    y
                                                                    exprs)
                                                               eMap))
                                            else error $
                                                 "evalZeroD Dot dim mismatch " ++
                                                 show
                                                     ( dim1
                                                     , dim2
                                                     , Scalar $
                                                       Expression node exprs)
                                    (Dim2 (d1, d2), Dim2 (dd1, dd2)) ->
                                        if (d1, d2) == (dd1, dd2)
                                            then sum $
                                                 zipWith
                                                     (*)
                                                     (elems
                                                          (evalTwoD
                                                               (TwoD $
                                                                Expression
                                                                    x
                                                                    exprs)
                                                               eMap))
                                                     (elems
                                                          (evalTwoD
                                                               (TwoD $
                                                                Expression
                                                                    y
                                                                    exprs)
                                                               eMap))
                                            else error $
                                                 "evalZeroD Dot dim mismatch " ++
                                                 show
                                                     ( (d1, d2)
                                                     , (dd1, dd2)
                                                     , Scalar $
                                                       Expression node exprs)
                                    (Dim3 (d1, d2, d3), Dim3 (dd1, dd2, dd3)) ->
                                        if (d1, d2, d3) == (dd1, dd2, dd3)
                                            then sum $
                                                 zipWith
                                                     (*)
                                                     (elems
                                                          (evalThreeD
                                                               (ThreeD $
                                                                Expression
                                                                    x
                                                                    exprs)
                                                               eMap))
                                                     (elems
                                                          (evalThreeD
                                                               (ThreeD $
                                                                Expression
                                                                    y
                                                                    exprs)
                                                               eMap))
                                            else error $
                                                 "evalZeroD Dot dim mismatch " ++
                                                 show
                                                     ( (d1, d2, d3)
                                                     , (dd1, dd2, dd3)
                                                     , Scalar $
                                                       Expression node exprs)
                                    _ ->
                                        error $
                                        "evalZeroD Dot needs 2 inputs " ++
                                        show (inputs, exprs)
                        RealPart ->
                            case inputs of
                                [x] ->
                                    realPart
                                        (evalScalarC
                                             (ScalarC (Expression x exprs))
                                             eMap)
                                _ ->
                                    error $ "evalZeroD RealPart " ++ show inputs
                        ImagPart ->
                            case inputs of
                                [x] ->
                                    imagPart
                                        (evalScalarC
                                             (ScalarC (Expression x exprs))
                                             eMap)
                                _ ->
                                    error $ "evalZeroD RealPart " ++ show inputs
                        Compound _ -> 0
                        _ ->
                            error $
                            "evalZeroD haven't implemented " ++
                            show op ++ " yet  " ++ show (inputs, exprs)

{-
Evaluate a complex scalar expression given a map of variable values.
-}
evalScalarC :: ScalarC -> ValMaps -> (DC.Complex Double)
evalScalarC (ScalarC e) eMap = evalZeroDC Nothing e eMap

evalZeroDC ::
       Maybe (ExpressionEdge -> Double)
    -> Expression
    -> ValMaps
    -> (DC.Complex Double) {-must be RelElem-}
evalZeroDC relEval (Expression node exprs) eMap =
    let evalZeroDC' (_, next) = evalZeroDC relEval (Expression next exprs) eMap
     in case I.lookup node exprs of
            Nothing ->
                error $
                "evalZeroDC didn't find node " ++
                (take 200 $ show (node, exprs))
            Just e@(Var _dims _name) ->
                error $ "evalZeroDC found nonscalar " ++ show e
            Just e@(DVar _ _) -> error $ "evalZeroDC found nonscalar " ++ show e
            Just e@(RelElem _ _ _) ->
                error $ "evalZeroDC found RelElem " ++ show e
            Just (Const Dim0 d) -> error $ "evalZeroDC found const" ++ show d
            Just (Const dims d) -> error $ "evalZeroDC got " ++ show (dims, d)
            Just (Op _dims op inputs) ->
                let op1 fun name =
                        case inputs of
                            [x] -> fun $ evalZeroDC' ([], x)
                            _ ->
                                error $
                                "evalZeroDC " ++ name ++ " " ++ show inputs
                    op2 fun name =
                        case inputs of
                            [x, y] ->
                                fun (evalZeroDC' ([], x)) (evalZeroDC' ([], y))
                            _ ->
                                error $
                                "evalZeroDC " ++ name ++ " " ++ show inputs
                 in case op of
                        Sum -> sum $ map (\x -> evalZeroDC' ([], x)) inputs
                        Abs -> op1 abs "abs"
                        Signum -> op1 signum "signum"
                        Prod ->
                            foldr (*) 1 $ map (\x -> evalZeroDC' ([], x)) inputs
                        Div -> op2 (/) "divide"
                        Sqrt -> op1 sqrt "sqrt"
                        Sin -> op1 sin "sin"
                        Cos -> op1 cos "cos"
                        Tan -> op1 tan "tan"
                        Exp -> op1 exp "exp"
                        Log -> op1 log "log"
                        Sinh -> op1 sinh "sinh"
                        Cosh -> op1 cosh "cosh"
                        Tanh -> op1 tanh "tanh"
                        Asin -> op1 asin "asin"
                        Acos -> op1 acos "acos"
                        Atan -> op1 atan "atan"
                        Asinh -> op1 asinh "asinh"
                        Acosh -> op1 acosh "acosh" -- JLMP FIXME:  add errors here
                        Atanh ->
                            error
                                "HInterp.ScalarC cannot evaluate atanh of Complex scalar"
          -- Dot -> case inputs of -- JLMP fixme:  hermese
            -- _ -> error $ "evalZeroDC Dot needs 2 inputs " ++ show (inputs,exprs)
                        RealImag ->
                            case map (\n ->
                                          evalZeroD
                                              relEval
                                              (Expression n exprs)
                                              eMap)
                                     inputs of
                                [re, im] -> re :+ im
                                _ ->
                                    error $
                                    "HI.evalZeroD RealImag needs two inputs not " ++
                                    show inputs
                        _ ->
                            error $
                            "evalZeroDC haven't implemented " ++
                            show op ++ " yet"

{-
Evaluate a RE given a  expression given a map of variable values.
-}
evalRelElem :: (ExpressionEdge -> Double) -> ZipConvElem -> ValMaps -> Double {-must be RelElem-}
evalRelElem efun (ZCE e) eMap = evalZeroD (Just efun) e eMap

{-
Evaluate a one-dimensional expression given a map of variable values.
-}
evalOneD :: OneD -> ValMaps -> (U.UArray Int Double)
evalOneD (OneD (Expression node exprs)) eMap =
    let evalOneD' next =
            evalOneD (OneD (Expression next exprs)) eMap :: (U.UArray Int Double)
     in case I.lookup node exprs of
            Nothing ->
                error $
                "evalOneD found node " ++ (take 200 $ show (node, exprs))
            Just (Var (Dim1 _dim) name) ->
                case Map.lookup name (vm1 eMap) of
                    Nothing ->
                        error $ "evalOneD' find var " ++ show (name, eMap)
                    Just x -> x
            Just (Const (Dim1 dim) d) ->
                U.listArray (0, dim - 1) (replicate dim d)
            Just (Op dims@(Dim1 dim) op inputs) ->
                case op of
                    ScaleV ->
                        let scale ::
                                   Double
                                -> (U.UArray Int Double)
                                -> (U.UArray Int Double)
                            scale s a =
                                U.listArray
                                    (0, dim - 1)
                                    [(a ! i) * s | i <- [0 .. dim - 1]]
                         in case inputs of
                                [n1, n2] ->
                                    case (getDimE exprs n1, getDimE exprs n2) of
                                        (Dim0, (Dim1 _)) ->
                                            scale
                                                (evalZeroD
                                                     Nothing
                                                     (Expression n1 exprs)
                                                     eMap) $
                                            evalOneD' n2
                                        _ ->
                                            error $
                                            "evalOneD requires Const as first arg  " ++
                                            show (inputs, exprs)
                                _ ->
                                    error $
                                    "evalOneD requires 2 args  " ++ show inputs
                    Sum ->
                        let sumFun ::
                                   [U.UArray Int Double]
                                -> (U.UArray Int Double) -- this seems slow,,,
                            sumFun (as:bs:rest) =
                                sumFun
                                    (U.listArray
                                         (bounds as)
                                         [ (as ! i) + (bs ! i)
                                         | i <- [0 .. dim - 1]
                                         ] :
                                     rest)
                            sumFun [cs] = cs
                            sumFun [] = error "evalOneD.sumFun []"
                         in sumFun $ map (evalOneD') inputs
                    Inject ss ->
                        let v =
                                case inputs of
                                    [x] -> evalOneD' x
                                    _ ->
                                        error $
                                        "evalOneD needs 1 input " ++
                                        show (inputs, exprs)
                         in case ss of
                                (SSCrop [(low, high)] _) ->
                                    U.listArray
                                        (0, dim - 1)
                                        [ if i < low || i > high
                                            then 0
                                            else v ! (i - low)
                                        | i <- [0 .. dim - 1]
                                        ]
                -- TODO JLMP: Nyquist
                                _ ->
                                    error $
                                    "evalOneD doesn't implement " ++
                                    show (ss, exprs)
                    Project ss ->
                        let v =
                                case inputs of
                                    [x] -> evalOneD' x
                                    _ ->
                                        error $
                                        "evalOneD needs 1 input " ++
                                        show (inputs, exprs)
                         in case ss of
                                (SSCrop [(low, high)] _) ->
                                    U.listArray
                                        (0, high - low)
                                        [v ! i | i <- [low .. high]]
                                (SSNyquist [(p, (start, stop))]) ->
                                    let keepers =
                                            [start,start + p .. (dim - stop)] :: [Int]
                                     in U.listArray
                                            (0, dim - 1)
                                            [v ! idx | idx <- keepers]
                                _ ->
                                    error $
                                    "evalOneD doesn't implement " ++
                                    show (ss, exprs)
                    SCZ sczExpr ->
                        let vs = map (\x -> evalOneD' x) inputs -- CKA the inputs don't need to have the same dimension
                            dimsMatch (Dim1 d) (Dim1 v) = d /= v || dim /= d
                            dimsMatch dims _ =
                                error $ "HI.evalOneD/SCZ not 1d " ++ show dims
                            vDims =
                                [ Dim1 (snd (bounds v) - fst (bounds v) + 1)
                                | v <- vs
                                ] :: [Dims]
                         in if List.or $
                               zipWith
                                   dimsMatch
                                   (replicate (length inputs) dims)
                                   vDims
                                then error $
                                     "evalOnD SCZ wrong input sizes " ++
                                     show (dims, vs, exprs)
                        -- function which returns input array at index -- getting the inputs
                                else let arrayFun idx =
                                             case I.lookup idx arrays of
                                                 Just arry -> arry
                                                 Nothing ->
                                                     error $
                                                     "evalOneD index out of range" ++
                                                     show (idx, inputs)
                        -- creates an intmap of input arrays (since have log(n) access)
                                         arrays :: IntMap (U.UArray Int Double)
                                         arrays = I.fromList $ zip [0 ..] vs
                        -- calculates the contribution to an output value at a certain index
                                         efun :: Int -> ExpressionEdge -> Double {-must be RelElem-}
                                         efun idx (RelElem arrayIdx ZeroMargin [offset]) =
                                             let idx' = idx + offset
                                              in if 0 <= idx' && idx' < dim
                                                     then (arrayFun arrayIdx) !
                                                          idx'
                                                     else 0
                                         efun idx re =
                                             error $
                                             "evalOneD array index problem " ++
                                             show (re, idx, exprs)
                                      in U.listArray
                                             (0, dim - 1)
                                             [ evalZeroD
                                                 (Just $ efun idx)
                                                 sczExpr
                                                 eMap
                                             | idx <- [0 .. dim - 1]
                                             ]
                    RealPart ->
                        let 
                         in case inputs of
                                [x] ->
                                    let v =
                                            evalOneDC
                                                (OneDC (Expression x exprs))
                                                eMap :: Array Int (DC.Complex Double)
                                     in U.listArray
                                            (0, dim - 1)
                                            [ realPart (v ! i)
                                            | i <- [0 .. dim - 1]
                                            ]
                                _ -> error $ "evalOneD " ++ show inputs
                    ImagPart ->
                        let 
                         in case inputs of
                                [x] ->
                                    let v =
                                            evalOneDC
                                                (OneDC (Expression x exprs))
                                                eMap :: Array Int (DC.Complex Double)
                                     in U.listArray
                                            (0, dim - 1)
                                            [ imagPart (v ! i)
                                            | i <- [0 .. dim - 1]
                                            ]
                                _ -> error $ "evalOneD " ++ show inputs
                    Shift (OS1d (offset, c)) ->
                        let 
                         in case inputs of
                                [x] ->
                                    let original = evalOneD' x
                                        shiftedElemAt i =
                                            if i >= offset && i - offset < dim
                                                then (original ! (i - offset)) *
                                                     c
                                                else 0
                                     in U.listArray
                                            (0, dim - 1)
                                            (map shiftedElemAt [0 .. dim - 1])
                                _ ->
                                    error
                                        "evalOneD wrong number of input Shift, expect 1"
                    _ -> error $ "evalOneD' implemented " ++ show op ++ " yet"
            Just e -> error $ "evalOneD found " ++ show e

{-

-}
evalOneDC :: OneDC -> ValMaps -> (Array Int (DC.Complex Double))
evalOneDC (OneDC (Expression node exprs)) eMap =
    let evalOneDC' node =
            evalOneDC (OneDC (Expression node exprs)) eMap :: (Array Int (DC.Complex Double))
     in case I.lookup node exprs of
            Nothing ->
                error $
                "evalOneDC't find node " ++ (take 200 $ show (node, exprs))
            Just (Const (Dim1 dim) d) ->
                listArray (0, dim - 1) (replicate dim (d :+ 0))
            Just (Op (Dim1 dim) op inputs) ->
                case op of
                    RealImag ->
                        case map (\n ->
                                      evalOneD (OneD (Expression n exprs)) eMap)
                                 inputs of
                            [res, ims] ->
                                listArray
                                    (bounds res)
                                    [res ! i :+ ims ! i | i <- [0 .. dim - 1]]
                            _ ->
                                error $
                                "HI.evalOneDC needs 2 inputs " ++ show inputs
                    FT True ->
                        case inputs of
                            [x] ->
                                listArray
                                    (0, dim - 1)
                                    (error "FFT.fft $ elems $ evalOneDC' x")
                            _ -> error $ "evalOneDC " ++ show inputs
          -- do the inverse FFT.  Note that the ifft includes a 1/N factor, which is not standard
                    FT False ->
                        case inputs of
                            [x] ->
                                let s = (fromIntegral $ dim)
                                 in listArray
                                        (0, dim - 1)
                                        (map (* s) $
                                         error "FFT.ifft $ elems $ evalOneDC' x")
                            _ -> error $ "evalOneDC " ++ show inputs
                    ScaleV ->
                        let scaleR ::
                                   Double
                                -> Array Int (DC.Complex Double)
                                -> Array Int (DC.Complex Double)
                            scaleR sr x =
                                listArray
                                    (bounds x)
                                    [(x ! i) * (sr :+ 0) | i <- [0 .. dim - 1]]
                            scaleC ::
                                   (DC.Complex Double)
                                -> Array Int (DC.Complex Double)
                                -> Array Int (DC.Complex Double)
                            scaleC s x =
                                listArray
                                    (0, dim - 1)
                                    [(x ! i) * s | i <- [0 .. dim - 1]]
                         in case inputs of
                                [n1, n2] ->
                                    case (getDimE exprs n1, getDimE exprs n2) of
                                        (Dim0, (Dim1 _)) ->
                                            if (nodeIsComplex exprs n1)
                                                then scaleC
                                                         (evalZeroDC
                                                              Nothing
                                                              (Expression
                                                                   n1
                                                                   exprs)
                                                              eMap) $
                                                     evalOneDC' n2
                                                else scaleR
                                                         (evalZeroD
                                                              Nothing
                                                              (Expression
                                                                   n1
                                                                   exprs)
                                                              eMap) $
                                                     evalOneDC' n2
                                        _ ->
                                            error $
                                            "evalOneDC requires Const as first arg  " ++
                                            show inputs
                                _ ->
                                    error $
                                    "evalOneDC requires 2 arguments  " ++
                                    show inputs
                    Sum -- FIXME:   should this be a an array of arrays??
                     ->
                        let sumFun ::
                                   [Array Int (DC.Complex Double)]
                                -> Array Int (DC.Complex Double)
                            sumFun (as:bs:rest) =
                                sumFun
                                    ((U.listArray
                                          (0, dim - 1)
                                          [as ! i + bs ! i | i <- [0 .. dim]]) :
                                     rest)
                            sumFun [cs] = cs
                            sumFun [] = error "evalOneDC.sumFun []"
                         in sumFun $ map (\x -> evalOneDC' x) inputs
                    Inject ss ->
                        let v =
                                case inputs of
                                    [x] -> evalOneDC' x
                                    _ ->
                                        error $
                                        "evalOneDC needs 1 input " ++
                                        show (inputs, exprs)
                         in case ss of
                                (SSCrop [(low, high)] _) ->
                                    listArray
                                        (0, dim - 1)
                                        [ if i < low || i > high
                                            then 0
                                            else v ! (i - low)
                                        | i <- [0 .. dim - 1]
                                        ]
                -- TODO JLMP: Nyquist
                                _ ->
                                    error $
                                    "evalOneD doesn't implement " ++
                                    show (ss, exprs)
                    Project ss ->
                        let v =
                                case inputs of
                                    [x] -> evalOneDC' x
                                    _ ->
                                        error $
                                        "evalOneD needs 1 input " ++
                                        show (inputs, exprs)
                         in case ss of
                                (SSCrop [(low, high)] _) ->
                                    listArray
                                        (0, high - low)
                                        [v ! i | i <- [low .. high]]
                                (SSNyquist [(p, (start, stop))]) ->
                                    let keepers =
                                            [start,start + p .. (dim - stop)] :: [Int]
                                     in listArray
                                            (0, dim - 1)
                                            [v ! idx | idx <- keepers]
                                _ ->
                                    error $
                                    "evalOneDC doesn't implement " ++
                                    show (ss, exprs)
                    Shift (OS1d (offset, c)) ->
                        let 
                         in case inputs of
                                [x] ->
                                    let original = evalOneDC' x
                                        shiftedElemAt i =
                                            if i >= offset && i - offset < dim
                                                then (original ! (i - offset)) *
                                                     fromReal c
                                                else fromReal 0
                                     in U.listArray
                                            (0, dim - 1)
                                            (map shiftedElemAt [0 .. dim - 1])
                                _ ->
                                    error
                                        "evalOneD wrong number of input Shift, expect 1"
                    _ -> error $ "evalOneDC't implemented " ++ show op ++ " yet"
            Just e -> error $ "evalOneDC found " ++ show e

{-


-}
evalTwoD :: TwoD -> ValMaps -> U.UArray (Int, Int) Double
evalTwoD (TwoD (Expression node exprs)) eMap =
    let evalTwoD' next =
            evalTwoD (TwoD (Expression next exprs)) eMap :: U.UArray (Int, Int) Double
     in case I.lookup node exprs of
            Nothing ->
                error $
                "evalTwoD't find node " ++ (take 200 $ show (node, exprs))
            Just (Var (Dim2 _) name) ->
                case Map.lookup name (vm2 eMap) of
                    Nothing ->
                        error $
                        "evalTwoD't find var with name: " ++
                        show name ++ show eMap
                    Just x -> x
            Just (Const (Dim2 (dim1, dim2)) d) ->
                U.listArray
                    ((0, 0), (dim1 - 1, dim2 - 1))
                    (replicate (dim1 * dim2) d)
            Just (Op (Dim2 (dim1, dim2)) op inputs) ->
                case op of
                    ScaleV ->
                        let scale ::
                                   Double
                                -> U.UArray (Int, Int) Double
                                -> U.UArray (Int, Int) Double
                            scale s x =
                                U.listArray
                                    ((0, 0), (dim1 - 1, dim2 - 1))
                                    [ s * (x ! (i, j))
                                    | i <- [0 .. dim1 - 1]
                                    , j <- [0 .. dim2 - 1]
                                    ]
                         in case inputs of
                                [n1, n2] ->
                                    case (getDimE exprs n1, getDimE exprs n2) of
                                        (Dim0, (Dim2 _)) ->
                                            scale
                                                (evalZeroD
                                                     Nothing
                                                     (Expression n1 exprs)
                                                     eMap) $
                                            evalTwoD' n2
                                        _ ->
                                            error $
                                            "evalOneD requires Const as first arg  " ++
                                            show inputs
                                _ ->
                                    error $
                                    "evalTwoD ScaleV requires Const as first arg  " ++
                                    show inputs
                    Sum ->
                        let sumFun ::
                                   [U.UArray (Int, Int) Double]
                                -> (U.UArray (Int, Int) Double) -- this seems slow,,,
                            sumFun (as:bs:rest) =
                                sumFun
                                    (U.listArray
                                         (bounds as)
                                         [ (as ! (i, j)) + (bs ! (i, j))
                                         | i <- [0 .. dim1 - 1]
                                         , j <- [0 .. dim2 - 1]
                                         ] :
                                     rest)
                            sumFun [cs] = cs
                            sumFun [] = error "evalTwoD.sumFun []"
                         in sumFun $ map (\x -> evalTwoD' x) inputs
                    Transpose SCR ->
                        let transpose ::
                                   (U.UArray (Int, Int) Double)
                                -> (U.UArray (Int, Int) Double)
                            transpose as =
                                U.array
                                    ((0, 0), (dim1 - 1, dim2 - 1))
                                    [ ((j, i), (as ! (i, j)))
                                    | i <- [0 .. dim2 - 1]
                                    , j <- [0 .. dim1 - 1]
                                    ]
                         in case inputs of
                                [x] -> transpose $ evalTwoD' x
                                _ ->
                                    error
                                        "evalTwoD Transpose requires a single input node "
                    Inject ss ->
                        let v =
                                case inputs of
                                    [x] -> evalTwoD' x
                                    _ ->
                                        error $
                                        "evalTwoD Inject needs 1 input " ++
                                        show (inputs, exprs)
                         in case ss of
                                (SSCrop [(low1, high1), (low2, high2)] _) ->
                                    U.listArray
                                        ((0, 0), (dim1 - 1, dim2 - 1))
                                        [ if i1 < low1 ||
                                             i1 > high1 ||
                                             i2 < low2 || i2 > high2
                                            then 0.0
                                            else v ! (i1 - low1, i2 - low2)
                                        | i1 <- [0 .. dim1 - 1]
                                        , i2 <- [0 .. dim2 - 1]
                                        ]
                                _ ->
                                    error $
                                    "evalTwoD Inject doesn't implement " ++
                                    show (ss, exprs)
                    Project ss ->
                        let v =
                                case inputs of
                                    [x] -> evalTwoD' x
                                    _ ->
                                        error $
                                        "evalTwoD Project needs 1 input " ++
                                        show (inputs, exprs)
                         in case ss of
                                (SSCrop [(low1, high1), (low2, high2)] _) ->
                                    listArray
                                        ((0, 0), (high1 - low1, high2 - low2))
                                        [ v ! (i, j)
                                        | i <- [low1 .. high1]
                                        , j <- [low2 .. high2]
                                        ]
                                (SSNyquist [(p1, (start1, stop1)), (p2, (start2, stop2))]) ->
                                    U.listArray
                                        ((0, 0), (dim1 - 1, dim2 - 1))
                                        [ v ! (i, j)
                                        | i <-
                                              [start1,start1 + p1 .. (dim1 -
                                                                      stop1)]
                                        , j <-
                                              [start2,start2 + p2 .. (dim2 -
                                                                      stop2)]
                                        ]
                                (SSList2D (SparseList2D len Dii1 toKeep _)) ->
                                    U.listArray
                                        ((0, 0), (dim1 - 1, len))
                                        [ v ! (i, j)
                                        | i <- elems toKeep
                                        , j <- [0 .. dim1 - 1]
                                        ]
                                _ ->
                                    error $
                                    "evalTwoD Project doesn't implement " ++
                                    show (ss, exprs)
                    SCZ sczExpr ->
                        let vs = map (\x -> evalTwoD' x) inputs -- CKA the inputs don't need to have the same dimension
                        -- function which returns input array at index
                         in let arrayFun arrayIdx =
                                    case I.lookup arrayIdx arrays of
                                        Just arry -> arry
                                        Nothing ->
                                            error $
                                            "evalTwoD index out of range" ++
                                            show (arrayIdx, inputs)
                                arrays :: IntMap (U.UArray (Int, Int) Double)
                                arrays = I.fromList $ zip [0 ..] vs
                                efun :: (Int, Int) -> ExpressionEdge -> Double {-must be RelElem-}
                                efun (idx, idy) (RelElem arrayIdx ZeroMargin [offsetX, offsetY]) =
                                    let idx' = idx + offsetX
                                        idy' = idy + offsetY
                                     in if 0 <= idx' &&
                                           idx' < dim1 &&
                                           0 <= idy' && idy' < dim2
                                            then (arrayFun arrayIdx) !
                                                 (idx', idy')
                                            else 0
                                efun (idx, idy) re =
                                    error $
                                    "evalTwoD SCZ array index problem " ++
                                    show (re, idx, idy, exprs)
                             in U.listArray
                                    ((0, 0), (dim1 - 1, dim2 - 1))
                                    [ evalZeroD
                                        (Just $ efun (idx, idy))
                                        sczExpr
                                        eMap
                                    | idx <- [0 .. dim1 - 1]
                                    , idy <- [0 .. dim2 - 1]
                                    ]
                    RealPart ->
                        case inputs of
                            [x] ->
                                let x' =
                                        evalTwoDC
                                            (TwoDC (Expression x exprs))
                                            eMap
                                 in U.listArray
                                        ((0, 0), (dim1 - 1, dim2 - 1))
                                        [ realPart (x' ! (i, j))
                                        | i <- [0 .. dim1 - 1]
                                        , j <- [0 .. dim2 - 1]
                                        ]
                            _ -> error $ "evalTwoD RealPart " ++ show inputs
                    ImagPart ->
                        case inputs of
                            [x] ->
                                let x' =
                                        evalTwoDC
                                            (TwoDC (Expression x exprs))
                                            eMap :: Array (Int, Int) (DC.Complex Double)
                                 in U.listArray
                                        ((0, 0), (dim1 - 1, dim2 - 1))
                                        [ imagPart (x' ! (i, j))
                                        | i <- [0 .. dim1 - 1]
                                        , j <- [0 .. dim2 - 1]
                                        ]
                            _ -> error $ "evalTwoD ImagPart " ++ show inputs
                    Shift (OS2d ((offset1, offset2), c)) ->
                        let 
                         in case inputs of
                                [x] ->
                                    let original = evalTwoD' x
                                        shiftedElemAt (i, j) =
                                            if i >= offset1 &&
                                               j >= offset2 &&
                                               i - offset1 < dim1 &&
                                               j - offset2 < dim2
                                                then (original !
                                                      (i - offset1, j - offset2)) *
                                                     c
                                                else 0
                                     in U.listArray
                                            ((0, 0), (dim1 - 1, dim2 - 1))
                                            [ shiftedElemAt (i, j)
                                            | i <- [0 .. dim1 - 1]
                                            , j <- [0 .. dim2 - 1]
                                            ]
                                _ ->
                                    error
                                        "evalTwoD wrong number of input Shift, expect 1"
                    _ ->
                        error $
                        "evalTwoD't implemented " ++
                        show op ++
                        " yet" ++
                        "\n" ++
                        pretty (exprs, node) ++ "\n" ++ show (exprs, node)
            Just e -> error $ "evalTwoD found " ++ show e

{-


Evaluate 2D Sparse
-}
evalTwoDSparse :: TwoDSparse -> ValMaps -> Map Int (U.UArray Int Double)
evalTwoDSparse (TwoDSparse sL@(SparseList2D _len2 _ toKeep _) (Expression node exprs)) eMap =
    let evalTwoDSparse' (sL, next) =
            evalTwoDSparse (TwoDSparse sL (Expression next exprs)) eMap :: Map Int (U.UArray Int Double)
     in case I.lookup node exprs of
            Nothing ->
                error $
                "evalTwoDSparse find node " ++ (take 200 $ show (node, exprs))
            Just (Const (Dim2SL1 (_dim1, dim2) _dim) d) ->
                Map.fromList
                    [ (i, (listArray (0, dim2 - 1) (replicate dim2 d)))
                    | i <- (elems toKeep)
                    ]
            Just (Const (Dim2SL2 (dim1, _dim2) _dim) d) ->
                Map.fromList
                    [ (i, (listArray (0, dim1 - 1) (replicate dim1 d)))
                    | i <- (elems toKeep)
                    ]
            Just (Const _ _) ->
                error "evalTwoDSparse found invalid dimensions for const "
            Just (Op dims op inputs) ->
                let dimKeep =
                        case dims of
                            (Dim2SL1 (_dim1, dim2) _sDim) -> dim2
                            (Dim2SL2 (dim1, _dim2) _sDim) -> dim1
                            d -> error $ "eval2DS dims " ++ show d
                 in case op of
                        ScaleV ->
                            let scale ::
                                       Double
                                    -> Map Int (U.UArray Int Double)
                                    -> Map Int (U.UArray Int Double)
                                scale s x = Map.map (amap (* s)) x
                             in case inputs of
                                    [n1, n2] ->
                                        case ( getDimE exprs n1
                                             , getDimE exprs n2) of
                                            (Dim0, Dim2SL1 _ _) ->
                                                scale
                                                    (evalZeroD
                                                         Nothing
                                                         (Expression n1 exprs)
                                                         eMap) $
                                                evalTwoDSparse' (sL, n2)
                                            (Dim0, Dim2SL2 _ _) ->
                                                scale
                                                    (evalZeroD
                                                         Nothing
                                                         (Expression n1 exprs)
                                                         eMap) $
                                                evalTwoDSparse' (sL, n2)
                                            _ ->
                                                error $
                                                "evalTwoD ScaleV requires Const as first arg  " ++
                                                show inputs
                                    _ ->
                                        error $
                                        "evalTwoD ScaleV requires Const as first arg  " ++
                                        show inputs
                        Sum ->
                            let sumAr ::
                                       U.UArray Int Double
                                    -> U.UArray Int Double
                                    -> U.UArray Int Double
                                sumAr as bs =
                                    (U.listArray
                                         (0, dimKeep - 1)
                                         [ (as ! i) + (bs ! i)
                                         | i <- [0 .. dimKeep - 1]
                                         ])
                                sumFun ::
                                       [Map Int (U.UArray Int Double)]
                                    -> (Map Int (U.UArray Int Double))
                                sumFun (as:bs:rest) =
                                    sumFun
                                        ((Map.unionWith (sumAr) as bs) : rest)
                                sumFun [cs] = cs
                                sumFun [] = error "evalTwoDSparse.sumFun []"
                             in sumFun $
                                map (\x -> evalTwoDSparse' (sL, x)) inputs
                        Project ss ->
                            let v =
                                    case inputs of
                                        [x] ->
                                            evalTwoD
                                                (TwoD $ Expression x exprs)
                                                eMap
                                        _ ->
                                            error $
                                            "evalTwoD Project needs 1 input " ++
                                            show (inputs, exprs)
                             in case ss of
                                    (SSList2D (SparseList2D len2 Dii1 toKeep2 _)) ->
                                        Map.fromList
                                            [ ( j
                                              , U.listArray
                                                    (0, dimKeep * len2 - 1) -- TODO: Double check this is the correct dim
                                                    [ v ! (i, j)
                                                    | i <- [0 .. dimKeep - 1]
                                                    ])
                                            | j <- (elems toKeep2)
                                            ]
                                    _ ->
                                        error $
                                        "evalTwoDSparse Project doesn't implement " ++
                                        show (ss, exprs) ++ " yet"
                        RealPart ->
                            case inputs of
                                [x] ->
                                    let x' =
                                            evalTwoDCSparse
                                                (TwoDCSparse
                                                     sL
                                                     (Expression x exprs))
                                                eMap :: Map Int (Array Int (DC.Complex Double))
                                     in Map.map (acrmap realPart) x' :: Map Int (UArray Int Double)
                                _ -> error $ "evalTwoD RealPart " ++ show inputs
                        ImagPart ->
                            case inputs of
                                [x] ->
                                    let x' =
                                            evalTwoDCSparse
                                                (TwoDCSparse
                                                     sL
                                                     (Expression x exprs))
                                                eMap :: Map Int (Array Int (DC.Complex Double))
                                     in Map.map (acrmap imagPart) x' :: Map Int (UArray Int Double)
                                _ -> error $ "evalTwoD ImagPart " ++ show inputs
                        _ ->
                            error $
                            "evalTwoD't implemented " ++ show op ++ " yet"
            Just e -> error $ "evalTwoD found " ++ show e

{-



-}
evalTwoDC :: TwoDC -> ValMaps -> Array (Int, Int) (DC.Complex Double)
evalTwoDC (TwoDC (Expression node exprs)) eMap =
    let evalTwoDC' next =
            evalTwoDC (TwoDC (Expression next exprs)) eMap :: Array (Int, Int) (DC.Complex Double)
     in case I.lookup node exprs of
            Nothing ->
                error $
                "evalTwoDC didn't find node " ++
                (take 10000 $ show (node, exprs))
            Just (Const (Dim2 (dim1, dim2)) d) ->
                listArray
                    ((0, 0), (dim1 - 1, dim2 - 1))
                    (replicate (dim2 * dim1) (d :+ 0))
            Just (Op (Dim2 (dim1, dim2)) op inputs) ->
                case op of
                    RealImag ->
                        case map (\n ->
                                      evalTwoD (TwoD (Expression n exprs)) eMap)
                                 inputs of
                            [res, ims] ->
                                listArray
                                    ((0, 0), (dim1 - 1, dim2 - 1))
                                    [ res ! (i, j) :+ ims ! (i, j)
                                    | i <- [0 .. dim1 - 1]
                                    , j <- [0 .. dim2 - 1]
                                    ]
                            _ ->
                                error $
                                "HI.evalTwoDC RealImag needs 2 inputs " ++
                                show inputs
                    FT True ->
                        case inputs of
                            [x] ->
                                let xSolved = evalTwoDC' x
                                    xList =
                                        [ [ xSolved ! (i, j)
                                        | j <- [0 .. dim2 - 1]
                                        ]
                                        | i <- [0 .. dim1 - 1]
                                        ]
                                 in listArray
                                        ((0, 0), (dim1 - 1, dim2 - 1))
                                        (error
                                             "concat $ List.transpose $ map FFT.fft $ List.transpose $ map FFT.fft xList")
                            _ -> error $ "evalTwoDC " ++ show inputs
          -- we only plan to use Column FT because it is easy to SIMDize
                    PFT dir Column ->
                        case inputs of
                            [x] ->
                                let xSolved = evalTwoDC' x
                                    xList =
                                        [ [ xSolved ! (i, j)
                                        | i <- [0 .. dim1 - 1]
                                        ]
                                        | j <- [0 .. dim2 - 1]
                                        ] -- TRANSPOSED ON PURPOSE
                                 in listArray
                                        ((0, 0), (dim1 - 1, dim2 - 1))
                                        (concat $
                                         List.transpose $
                                         map
                                             (case dir of
                                                  True -> error "FFT.fft"
                                                  False -> error "FFT.ifft")
                                             xList)
                            _ -> error $ "evalTwoDC " ++ show inputs
          -- do the inverse FFT.  Note that the ifft includes a 1/N factor.
                    FT False ->
                        case inputs of
                            [x] ->
                                let s = (fromIntegral $ dim1 * dim2)
                                    xSolved = evalTwoDC' x
                                    xList =
                                        [ [ xSolved ! (i, j)
                                        | j <- [0 .. dim2 - 1]
                                        ]
                                        | i <- [0 .. dim1 - 1]
                                        ]
                                 in listArray
                                        ((0, 0), (dim1 - 1, dim2 - 1))
                                        (error
                                             "concat $ map (map (*s)) $ List.transpose $ map FFT.ifft $ List.transpose $ map FFT.ifft xList")
                            _ -> error $ "evalTwoDC " ++ show inputs
                    ScaleV ->
                        let scaleR ::
                                   Double
                                -> Array (Int, Int) (DC.Complex Double)
                                -> Array (Int, Int) (DC.Complex Double)
                            scaleR sr v =
                                listArray
                                    (bounds v)
                                    [ (v ! (i, j)) * (sr :+ 0)
                                    | i <- [0 .. dim1 - 1]
                                    , j <- [0 .. dim2 - 1]
                                    ]
                            scaleC ::
                                   (DC.Complex Double)
                                -> Array (Int, Int) (DC.Complex Double)
                                -> Array (Int, Int) (DC.Complex Double)
                            scaleC s v =
                                listArray
                                    (bounds v)
                                    [ (v ! (i, j)) * s
                                    | i <- [0 .. dim1 - 1]
                                    , j <- [0 .. dim2 - 1]
                                    ]
                         in case inputs of
                                [n1, n2] ->
                                    case (getDimE exprs n1, getDimE exprs n2) of
                                        (Dim0, (Dim2 _)) ->
                                            if (nodeIsComplex exprs n1)
                                                then scaleC
                                                         (evalZeroDC
                                                              Nothing
                                                              (Expression
                                                                   n1
                                                                   exprs)
                                                              eMap) $
                                                     evalTwoDC' n2
                                                else scaleR
                                                         (evalZeroD
                                                              Nothing
                                                              (Expression
                                                                   n1
                                                                   exprs)
                                                              eMap) $
                                                     evalTwoDC' n2
                                        _ ->
                                            error $
                                            "evalTwoDC ScaleV requires Const as first arg and OneD second  " ++
                                            show inputs
                                _ ->
                                    error $
                                    "evalTwoDC ScaleV requires two args arg  " ++
                                    show inputs
                    Sum ->
                        let sumFun ::
                                   [Array (Int, Int) (DC.Complex Double)]
                                -> (Array (Int, Int) (DC.Complex Double)) -- this seems slow,,,
                            sumFun (as:bs:rest) =
                                sumFun
                                    (U.listArray
                                         (bounds as)
                                         [ (as ! (i, j)) + (bs ! (i, j))
                                         | i <- [0 .. dim1 - 1]
                                         , j <- [0 .. dim2 - 1]
                                         ] :
                                     rest)
                            sumFun [cs] = cs
                            sumFun [] = error "evalTwoDC.sumFun []"
                         in sumFun $ map (\x -> evalTwoDC' x) inputs
                    Transpose SCR ->
                        let transpose ::
                                   (Array (Int, Int) (DC.Complex Double))
                                -> (Array (Int, Int) (DC.Complex Double))
                            transpose as =
                                array
                                    ((0, 0), (dim1 - 1, dim2 - 1))
                                    [ ((j, i), (as ! (i, j)))
                                    | i <- [0 .. dim2 - 1]
                                    , j <- [0 .. dim1 - 1]
                                    ]
                         in case inputs of
                                [x] -> transpose $ evalTwoDC' x
                                _ ->
                                    error
                                        "evalTwoDC Transpose requires a single input node "
                    Inject ss ->
                        let v =
                                case inputs of
                                    [x] -> evalTwoDC' x
                                    _ ->
                                        error $
                                        "evalTwoDC Inject needs 1 input " ++
                                        show (inputs, exprs)
                         in case ss of
                                (SSCrop [(low1, high1), (low2, high2)] _) ->
                                    listArray
                                        ((0, 0), (dim1 - 1, dim2 - 1))
                                        [ if i1 < low1 ||
                                             i1 > high1 ||
                                             i2 < low2 || i2 > high2
                                            then 0.0
                                            else v ! (i1 - low1, i2 - low2)
                                        | i1 <- [0 .. dim1 - 1]
                                        , i2 <- [0 .. dim2 - 1]
                                        ]
                                _ ->
                                    error $
                                    "evalTwoDC Inject doesn't implement " ++
                                    show (ss, exprs)
                    Project ss ->
                        let v =
                                case inputs of
                                    [x] -> evalTwoDC' x
                                    _ ->
                                        error $
                                        "evalTwoDC Project needs 1 input " ++
                                        show (inputs, exprs)
                         in case ss of
                                (SSCrop [(low1, high1), (low2, high2)] _) ->
                                    listArray
                                        ((0, 0), (high1 - low1, high2 - low2))
                                        [ v ! (i, j)
                                        | i <- [low1 .. high1]
                                        , j <- [low2 .. high2]
                                        ]
                                (SSNyquist [(p1, (start1, stop1)), (p2, (start2, stop2))]) ->
                                    listArray
                                        ((0, 0), (dim1 - 1, dim2 - 1))
                                        [ v ! (i, j)
                                        | i <-
                                              [start1,start1 + p1 .. (dim1 -
                                                                      stop1)]
                                        , j <-
                                              [start2,start2 + p2 .. (dim2 -
                                                                      stop2)]
                                        ]
                                (SSList2D (SparseList2D len Dii1 toKeep _)) ->
                                    listArray
                                        ((0, 0), (dim1 - 1, len))
                                        [ v ! (i, j)
                                        | i <- elems toKeep
                                        , j <- [0 .. dim1 - 1]
                                        ]
                                _ ->
                                    error $
                                    "evalTwoDC Project doesn't implement " ++
                                    show (ss, exprs)
                    Shift (OS2d ((offset1, offset2), c)) ->
                        let 
                         in case inputs of
                                [x] ->
                                    let original = evalTwoDC' x
                                        shiftedElemAt (i, j) =
                                            if i >= offset1 &&
                                               j >= offset2 &&
                                               i - offset1 < dim1 &&
                                               j - offset2 < dim2
                                                then (original !
                                                      (i - offset1, j - offset2)) *
                                                     fromReal c
                                                else fromReal 0
                                     in U.listArray
                                            ((0, 0), (dim1 - 1, dim2 - 1))
                                            [ shiftedElemAt (i, j)
                                            | i <- [0 .. dim1 - 1]
                                            , j <- [0 .. dim2 - 1]
                                            ]
                                _ ->
                                    error
                                        "evalTwoD wrong number of input Shift, expect 1"
                    _ ->
                        error $
                        "evalTwoDC doesn't implemented " ++ show op ++ " yet"
            Just e -> error $ "evalTwoDC found " ++ show e

{-


-}
evalTwoDCSparse ::
       TwoDCSparse -> ValMaps -> Map Int (Array Int (DC.Complex Double))
evalTwoDCSparse (TwoDCSparse sL@(SparseList2D _len2 _ toKeep _) (Expression node exprs)) eMap =
    let evalTwoDCSparse' (sL, next) =
            evalTwoDCSparse (TwoDCSparse sL (Expression next exprs)) eMap :: Map Int (Array Int (DC.Complex Double))
     in case I.lookup node exprs of
            Nothing ->
                error $
                "HInterp - evalTwoDSparse find node " ++
                (take 200 $ show (node, exprs))
            Just (Const (Dim2SL1 (dim1, _dim2) _dim) d) ->
                Map.fromList
                    [ (i, (listArray (0, dim1 - 1) (replicate dim1 (d :+ 0))))
                    | i <- (elems toKeep)
                    ]
            Just (Const (Dim2SL2 (_dim1, dim2) _dim) d) ->
                Map.fromList
                    [ (i, (listArray (0, dim2 - 1) (replicate dim2 (d :+ 0))))
                    | i <- (elems toKeep)
                    ]
            Just (Const _ _) ->
                error
                    "HInterp - evalTwoDCSparse found const with incorrect dims "
            Just (Op dims op inputs) ->
                let dimKeep =
                        case dims of
                            (Dim2SL1 (dim1, _dim2) _sDim) -> dim1
                            (Dim2SL2 (_dim1, dim2) _sDim) -> dim2
                            d -> error $ "HInterp.eval2DCS dims " ++ show d
                 in case op of
                        ScaleV ->
                            let scaleR ::
                                       Double
                                    -> Map Int (Array Int (DC.Complex Double))
                                    -> Map Int (Array Int (DC.Complex Double))
                                scaleR sr x =
                                    Map.map
                                        (\a ->
                                             listArray (0, dimKeep - 1) $
                                             map (* (sr :+ 0)) $ elems a)
                                        x
                                scaleC ::
                                       (DC.Complex Double)
                                    -> Map Int (Array Int (DC.Complex Double))
                                    -> Map Int (Array Int (DC.Complex Double))
                                scaleC s x =
                                    Map.map
                                        (\a ->
                                             listArray (0, dimKeep - 1) $
                                             map (* s) $ elems a)
                                        x
                             in case inputs of
                                    [n1, n2] ->
                                        case ( getDimE exprs n1
                                             , getDimE exprs n2) of
                                            (Dim0, _) ->
                                                if (nodeIsComplex exprs n1)
                                                    then scaleC
                                                             (evalZeroDC
                                                                  Nothing
                                                                  (Expression
                                                                       n1
                                                                       exprs)
                                                                  eMap) $
                                                         evalTwoDCSparse'
                                                             (sL, n2)
                                                    else scaleR
                                                             (evalZeroD
                                                                  Nothing
                                                                  (Expression
                                                                       n1
                                                                       exprs)
                                                                  eMap) $
                                                         evalTwoDCSparse'
                                                             (sL, n2)
                                            _ ->
                                                error $
                                                "evalTwoDCSparse ScaleV requires Const as first arg and TwoD second arg " ++
                                                show inputs
                                    _ ->
                                        error $
                                        "evalTwoDCSparse ScaleV requires two arguments  " ++
                                        show inputs
                        Sum ->
                            let sumAr ::
                                       Array Int (DC.Complex Double)
                                    -> Array Int (DC.Complex Double)
                                    -> Array Int (DC.Complex Double)
                                sumAr as bs =
                                    (listArray
                                         (0, dimKeep - 1)
                                         [ (as ! i) + (bs ! i)
                                         | i <- [0 .. dimKeep - 1]
                                         ])
                                sumFun ::
                                       [Map Int (Array Int (DC.Complex Double))]
                                    -> (Map Int (Array Int (DC.Complex Double)))
                                sumFun (as:bs:rest) =
                                    sumFun
                                        ((Map.unionWith (sumAr) as bs) : rest)
                                sumFun [cs] = cs
                                sumFun [] = error "evalTwoDCSparse.sumFun []"
                             in sumFun $
                                map (\x -> evalTwoDCSparse' (sL, x)) inputs
                        Project ss ->
                            let v =
                                    case inputs of
                                        [x] ->
                                            evalTwoDC
                                                (TwoDC $ Expression x exprs)
                                                eMap
                                        _ ->
                                            error $
                                            "evalTwoDCSparse Project needs 1 input " ++
                                            show (inputs, exprs)
                             in case ss of
                                    (SSList2D (SparseList2D len2 Dii1 toKeep2 _)) ->
                                        Map.fromList
                                            [ ( j
                                              , U.listArray
                                                    (0, dimKeep * len2 - 1) -- TODO: Double check this is the correct dim
                                                    [ v ! (i, j)
                                                    | i <- [0 .. dimKeep - 1]
                                                    ])
                                            | j <- (elems toKeep2)
                                            ]
                                    _ ->
                                        error $
                                        "evalTwoDCSparse Project doesn't implement " ++
                                        show (ss, exprs)
                        _ ->
                            error $
                            "evalTwoDCSparse doesn't implemented " ++
                            show op ++ " yet"
            Just e -> error $ "evalTwoDCSparse found " ++ show e

{-



-}
evalThreeD :: ThreeD -> ValMaps -> U.UArray (Int, Int, Int) Double
evalThreeD (ThreeD (Expression node exprs)) eMap =
    let evalThreeD' next =
            evalThreeD (ThreeD (Expression next exprs)) eMap :: U.UArray ( Int
                                                                         , Int
                                                                         , Int) Double
     in case I.lookup node exprs of
            Nothing ->
                error $
                "evalThreeD't find node " ++ (take 200 $ show (node, exprs))
            Just (Var (Dim3 _) name) ->
                case Map.lookup name (vm3 eMap) of
                    Nothing ->
                        error $
                        "evalThreeD can't find var " ++ show (name, eMap)
                    Just x -> x
            Just (Const (Dim3 (dim1, dim2, dim3)) d) ->
                U.listArray
                    ((0, 0, 0), (dim1 - 1, dim2 - 1, dim3 - 1))
                    (replicate (dim1 * dim2 * dim3) d)
            Just (Op (Dim3 (dim1, dim2, dim3)) op inputs) ->
                case op of
                    ScaleV ->
                        let scale ::
                                   Double
                                -> U.UArray (Int, Int, Int) Double
                                -> U.UArray (Int, Int, Int) Double
                            scale s x =
                                U.listArray
                                    ((0, 0, 0), (dim1 - 1, dim2 - 1, dim3 - 1))
                                    [ s * (x ! (i, j, k))
                                    | i <- [0 .. dim1 - 1]
                                    , j <- [0 .. dim2 - 1]
                                    , k <- [0 .. dim3 - 1]
                                    ]
                         in case inputs of
                                [n1, n2] ->
                                    case (getDimE exprs n1, getDimE exprs n2) of
                                        (Dim0, Dim3 _) ->
                                            scale
                                                (evalZeroD
                                                     Nothing
                                                     (Expression n1 exprs)
                                                     eMap) $
                                            evalThreeD' n2
                                        _ ->
                                            error $
                                            "evalThreeD ScaleV requires Const as first arg  " ++
                                            show inputs
                                _ ->
                                    error $
                                    "evalThreeD ScaleV requires Const as first arg  " ++
                                    show inputs
                    Sum ->
                        let sumFun ::
                                   [U.UArray (Int, Int, Int) Double]
                                -> (U.UArray (Int, Int, Int) Double) -- this seems slow,,,
                            sumFun (as:bs:rest) =
                                sumFun
                                    (U.listArray
                                         (bounds as)
                                         [ (as ! (i, j, k)) + (bs ! (i, j, k))
                                         | i <- [0 .. dim1 - 1]
                                         , j <- [0 .. dim2 - 1]
                                         , k <- [0 .. dim3 - 1]
                                         ] :
                                     rest)
                            sumFun [cs] = cs
                            sumFun [] = error "evalThreeDC.sumFun []"
                         in sumFun $ map (\x -> evalThreeD' x) inputs
                    Inject ss ->
                        let v =
                                case inputs of
                                    [x] -> evalThreeD' x
                                    _ ->
                                        error $
                                        "evalThreeD Inject needs 1 input " ++
                                        show (inputs, exprs)
                         in case ss of
                                (SSCrop [(low1, high1), (low2, high2), (low3, high3)] _) ->
                                    U.listArray
                                        (bounds v)
                                        [ if i1 < low1 ||
                                             i1 > high1 ||
                                             i2 < low2 ||
                                             i2 > high2 ||
                                             i3 < low3 || i3 > high3
                                            then 0.0
                                            else v !
                                                 ( i1 - low1
                                                 , i2 - low2
                                                 , i3 - low3)
                                        | i1 <- [0 .. dim1 - 1]
                                        , i2 <- [0 .. dim2 - 1]
                                        , i3 <- [0 .. dim3 - 1]
                                        ]
                                (SSList3D (SparseList3D _len2 Diii1 toKeep2 _ toKeep3 _)) ->
                                    U.array
                                        ( (0, 0, 0)
                                        , (dim1 - 1, dim2 - 1, dim3 - 1)) $
                                    zip
                                        [ (i, j, k)
                                        | i <- [0 .. dim1 - 1]
                                        , (j, k) <-
                                              zip
                                                  (elems toKeep2)
                                                  (elems toKeep3)
                                        ]
                                        (elems v)
                                _ ->
                                    error $
                                    "evalThreeD Inject doesn't implement " ++
                                    show (ss, exprs)
                    Project ss ->
                        let v =
                                case inputs of
                                    [x] -> evalThreeD' x
                                    _ ->
                                        error $
                                        "evalThreeD Project needs 1 input " ++
                                        show (inputs, exprs)
                         in case ss of
                                (SSCrop [(low1, high1), (low2, high2), (low3, high3)] _) ->
                                    listArray
                                        ( (0, 0, 0)
                                        , ( high1 - low1
                                          , high2 - low2
                                          , high3 - low3))
                                        [ v ! (i, j, k)
                                        | i <- [low1 .. high1]
                                        , j <- [low2 .. high2]
                                        , k <- [low3 .. high3]
                                        ]
                                (SSNyquist [(p1, (start1, stop1)), (p2, (start2, stop2)), (p3, (start3, stop3))]) ->
                                    U.listArray
                                        ( (0, 0, 0)
                                        , (dim1 - 1, dim2 - 1, dim3 - 1))
                                        [ v ! (i, j, k)
                                        | i <-
                                              [start1,start1 + p1 .. (dim1 -
                                                                      stop1)]
                                        , j <-
                                              [start2,start2 + p2 .. (dim2 -
                                                                      stop2)]
                                        , k <-
                                              [start3,start3 + p3 .. (dim3 -
                                                                      stop3)]
                                        ]
                                _ ->
                                    error $
                                    "evalThreeD Project doesn't implement " ++
                                    show (ss, exprs)
                    SCZ sczExpr ->
                        let vs = map (\x -> evalThreeD' x) inputs -- CKA the inputs don't need to have the same dimension
                      -- function which returns input array at index
                         in let arrayFun arrayIdx =
                                    case I.lookup arrayIdx arrays of
                                        Just arry -> arry
                                        Nothing ->
                                            error $
                                            "evalThreeD index out of range" ++
                                            show (arrayIdx, inputs)
                                arrays ::
                                       IntMap (U.UArray (Int, Int, Int) Double)
                                arrays = I.fromList $ zip [0 ..] vs
                                efun ::
                                       (Int, Int, Int)
                                    -> ExpressionEdge
                                    -> Double {-must be RelElem-}
                                efun (idx, idy, idz) (RelElem arrayIdx ZeroMargin [offsetX, offsetY, offsetZ]) =
                                    let idx' = idx + offsetX
                                        idy' = idy + offsetY
                                        idz' = idz + offsetZ
                                     in if 0 <= idx' &&
                                           idx' < dim1 &&
                                           0 <= idy' &&
                                           idy' < dim2 &&
                                           0 <= idz' && idz' < dim3
                                            then (arrayFun arrayIdx) !
                                                 (idx', idy', idz')
                                            else 0
                                efun (idx, idy, idz) re =
                                    error $
                                    "evalThreeD SCZ array index problem " ++
                                    show (re, idx, idy, idz, exprs)
                             in U.listArray
                                    ((0, 0, 0), (dim1 - 1, dim2 - 1, dim3 - 1))
                                    [ evalZeroD
                                        (Just $ efun (idx, idy, idz))
                                        sczExpr
                                        eMap
                                    | idx <- [0 .. dim1 - 1]
                                    , idy <- [0 .. dim2 - 1]
                                    , idz <- [0 .. dim3 - 1]
                                    ]
                    RealPart ->
                        case inputs of
                            [x] ->
                                let x' =
                                        evalThreeDC
                                            (ThreeDC (Expression x exprs))
                                            eMap :: Array (Int, Int, Int) (DC.Complex Double)
                                 in listArray
                                        ( (0, 0, 0)
                                        , (dim1 - 1, dim2 - 1, dim3 - 1))
                                        [ realPart (x' ! (i, j, k))
                                        | i <- [0 .. dim1 - 1]
                                        , j <- [0 .. dim2 - 1]
                                        , k <- [0 .. dim3 - 1]
                                        ]
                            _ -> error $ "evalThreeD RealPart " ++ show inputs
                    ImagPart ->
                        case inputs of
                            [x] ->
                                let x' =
                                        evalThreeDC
                                            (ThreeDC (Expression x exprs))
                                            eMap :: Array (Int, Int, Int) (DC.Complex Double)
                                 in listArray
                                        ( (0, 0, 0)
                                        , (dim1 - 1, dim2 - 1, dim3 - 1))
                                        [ imagPart (x' ! (i, j, k))
                                        | i <- [0 .. dim1 - 1]
                                        , j <- [0 .. dim2 - 1]
                                        , k <- [0 .. dim3 - 1]
                                        ]
                            _ -> error $ "evalThreeD ImagPart " ++ show inputs
                    Transpose SCRS ->
                        let transpose ::
                                   (U.UArray (Int, Int, Int) Double)
                                -> (U.UArray (Int, Int, Int) Double)
                            transpose as =
                                U.array
                                    ((0, 0, 0), (dim1 - 1, dim2 - 1, dim3 - 1))
                                    [ ((j, i, k), (as ! (i, j, k)))
                                    | i <- [0 .. dim2 - 1]
                                    , j <- [0 .. dim1 - 1]
                                    , k <- [0 .. dim3 - 1]
                                    ]
                         in case inputs of
                                [x] -> transpose $ evalThreeD' x
                                _ ->
                                    error
                                        "evalThreeD Transpose requires a single input node "
                    Transpose SCSR ->
                        let transpose ::
                                   (U.UArray (Int, Int, Int) Double)
                                -> (U.UArray (Int, Int, Int) Double)
                            transpose as =
                                U.array
                                    ((0, 0, 0), (dim1 - 1, dim2 - 1, dim3 - 1))
                                    [ ((j, k, i), (as ! (i, j, k)))
                                    | i <- [0 .. dim3 - 1]
                                    , j <- [0 .. dim1 - 1]
                                    , k <- [0 .. dim2 - 1]
                                    ]
                         in case inputs of
                                [x] -> transpose $ evalThreeD' x
                                _ ->
                                    error
                                        "evalThreeD Transpose requires a single input node "
                    Transpose SSRC ->
                        let transpose ::
                                   (U.UArray (Int, Int, Int) Double)
                                -> (U.UArray (Int, Int, Int) Double)
                            transpose as =
                                U.array
                                    ((0, 0, 0), (dim1 - 1, dim2 - 1, dim3 - 1))
                                    [ ((k, i, j), (as ! (i, j, k)))
                                    | i <- [0 .. dim2 - 1]
                                    , j <- [0 .. dim3 - 1]
                                    , k <- [0 .. dim1 - 1]
                                    ]
                         in case inputs of
                                [x] -> transpose $ evalThreeD' x
                                _ ->
                                    error
                                        "evalThreeD Transpose requires a single input node "
                    Transpose SSCR ->
                        let transpose ::
                                   (U.UArray (Int, Int, Int) Double)
                                -> (U.UArray (Int, Int, Int) Double)
                            transpose as =
                                U.array
                                    ((0, 0, 0), (dim1 - 1, dim2 - 1, dim3 - 1))
                                    [ ((k, j, i), (as ! (i, j, k)))
                                    | i <- [0 .. dim3 - 1]
                                    , j <- [0 .. dim2 - 1]
                                    , k <- [0 .. dim1 - 1]
                                    ]
                         in case inputs of
                                [x] -> transpose $ evalThreeD' x
                                _ ->
                                    error
                                        "evalThreeD Transpose requires a single input node "
                    Transpose SRSC ->
                        let transpose ::
                                   (U.UArray (Int, Int, Int) Double)
                                -> (U.UArray (Int, Int, Int) Double)
                            transpose as =
                                U.array
                                    ((0, 0, 0), (dim1 - 1, dim2 - 1, dim3 - 1))
                                    [ ((i, k, j), (as ! (i, j, k)))
                                    | i <- [0 .. dim1 - 1]
                                    , j <- [0 .. dim3 - 1]
                                    , k <- [0 .. dim2 - 1]
                                    ]
                         in case inputs of
                                [x] -> transpose $ evalThreeD' x
                                _ ->
                                    error
                                        "evalThreeD Transpose requires a single input node "
                    Shift (OS3d ((offset1, offset2, offset3), c)) ->
                        let 
                         in case inputs of
                                [x] ->
                                    let original = evalThreeD' x
                                        shiftedElemAt (i, j, k) =
                                            if i >= offset1 &&
                                               j >= offset2 &&
                                               k >= offset3 &&
                                               i - offset1 < dim1 &&
                                               j - offset2 < dim2 &&
                                               k - offset3 < dim3
                                                then (original !
                                                      ( i - offset1
                                                      , j - offset2
                                                      , k - offset3)) *
                                                     c
                                                else 0
                                     in U.listArray
                                            ( (0, 0, 0)
                                            , (dim1 - 1, dim2 - 1, dim3 - 1))
                                            [ shiftedElemAt (i, j, k)
                                            | i <- [0 .. dim1 - 1]
                                            , j <- [0 .. dim2 - 1]
                                            , k <- [0 .. dim3 - 1]
                                            ]
                                _ ->
                                    error
                                        "evalThreeD wrong number of input Shift, expect 1"
                    _ ->
                        error $ "evalThreeD't implemented " ++ show op ++ " yet"
            Just e -> error $ "evalThreeD found " ++ show e

{-


Evaluate 3D Sparse
-}
evalThreeDSparse ::
       ThreeDSparse -> ValMaps -> Map (Int, Int) (U.UArray Int Double)
evalThreeDSparse (ThreeDSparse sL@(SparseList3D _len2 _ toKeep1 _ toKeep2 _) (Expression node exprs)) eMap =
    let evalThreeDSparse' (sL, next) =
            evalThreeDSparse (ThreeDSparse sL (Expression next exprs)) eMap :: Map ( Int
                                                                                   , Int) (U.UArray Int Double)
     in case I.lookup node exprs of
            Nothing ->
                error $
                "evalThreeDSparse find node " ++ (take 200 $ show (node, exprs))
            Just (Const (Dim3SL1 (dim1, _, _) _dim) d) ->
                Map.fromList
                    [ ((i, j), (U.listArray (0, dim1 - 1) (replicate dim1 d)))
                    | i <- (elems toKeep1)
                    , j <- (elems toKeep2)
                    ]
            Just (Const (Dim3SL2 (_, dim2, _) _dim) d) ->
                Map.fromList
                    [ ((i, j), (U.listArray (0, dim2 - 1) (replicate dim2 d)))
                    | i <- (elems toKeep1)
                    , j <- (elems toKeep2)
                    ]
            Just (Const (Dim3SL3 (_, _, dim3) _dim) d) ->
                Map.fromList
                    [ ((i, j), (U.listArray (0, dim3 - 1) (replicate dim3 d)))
                    | i <- (elems toKeep1)
                    , j <- (elems toKeep2)
                    ]
            Just (Const _ _) ->
                error "evalThreeDSparse found invalid dimensions for const "
            Just (Op dims op inputs) ->
                let dimKeep =
                        case dims of
                            (Dim3SL1 (dim1, _dim2, _dim3) _sDim) -> dim1
                            (Dim3SL2 (_dim1, dim2, _dim3) _sDim) -> dim2
                            (Dim3SL3 (_dim1, _dim2, dim3) _sDim) -> dim3
                            d -> error $ "eval3DS dims " ++ show d
                 in case op of
                        ScaleV ->
                            let scale ::
                                       Double
                                    -> U.UArray Int Double
                                    -> U.UArray Int Double
                                scale s x = amap (* s) x
                             in case inputs of
                                    [n1, n2] ->
                                        case ( getDimE exprs n1
                                             , getDimE exprs n2) of
                                            (Dim0, Dim3SL1 _ _) ->
                                                Map.map
                                                    (scale
                                                         (evalZeroD
                                                              Nothing
                                                              (Expression
                                                                   n1
                                                                   exprs)
                                                              eMap)) $
                                                evalThreeDSparse' (sL, n2)
                                            (Dim0, Dim3SL2 _ _) ->
                                                Map.map
                                                    (scale
                                                         (evalZeroD
                                                              Nothing
                                                              (Expression
                                                                   n1
                                                                   exprs)
                                                              eMap)) $
                                                evalThreeDSparse' (sL, n2)
                                            (Dim0, Dim3SL3 _ _) ->
                                                Map.map
                                                    (scale
                                                         (evalZeroD
                                                              Nothing
                                                              (Expression
                                                                   n1
                                                                   exprs)
                                                              eMap)) $
                                                evalThreeDSparse' (sL, n2)
                                            _ ->
                                                error $
                                                "evalThreeD ScaleV requires Const as first arg  " ++
                                                show inputs
                                    _ ->
                                        error $
                                        "evalThreeD ScaleV requires Const as first arg  " ++
                                        show inputs
                        Sum ->
                            let sumAr ::
                                       U.UArray Int Double
                                    -> U.UArray Int Double
                                    -> U.UArray Int Double
                                sumAr as bs =
                                    (U.listArray
                                         (0, dimKeep - 1)
                                         [ (as ! i) + (bs ! i)
                                         | i <- [0 .. dimKeep - 1]
                                         ])
                                sumFun ::
                                       [Map (Int, Int) (U.UArray Int Double)]
                                    -> (Map (Int, Int) (U.UArray Int Double))
                                sumFun (as:bs:rest) =
                                    sumFun
                                        ((Map.unionWith (sumAr) as bs) : rest)
                                sumFun [cs] = cs
                                sumFun [] = error "evalThreeDSparse.sumFun []"
                             in sumFun $
                                map (\x -> evalThreeDSparse' (sL, x)) inputs
                        Project ss ->
                            let v =
                                    case inputs of
                                        [x] ->
                                            evalThreeD
                                                (ThreeD $ Expression x exprs)
                                                eMap
                                        _ ->
                                            error $
                                            "evalThreeD Project needs 1 input " ++
                                            show (inputs, exprs)
                             in case ss of
                                    (SSList3D (SparseList3D _len2 Diii1 toKeep2 _ toKeep3 _)) ->
                                        Map.fromList
                                            [ ( (j, k)
                                              , U.listArray
                                                    (0, dimKeep - 1)
                                                    [ v ! (i, j, k)
                                                    | i <- [0 .. dimKeep - 1]
                                                    ])
                                            | j <- (elems toKeep2)
                                            , k <- (elems toKeep3)
                                            ]
                                    _ ->
                                        error $
                                        "evalThreeDSparse Project doesn't implement " ++
                                        show (ss, exprs)
                        RealPart ->
                            case inputs of
                                [x] ->
                                    let x' =
                                            evalThreeDCSparse
                                                (ThreeDCSparse
                                                     sL
                                                     (Expression x exprs))
                                                eMap :: Map (Int, Int) (Array Int (DC.Complex Double))
                                     in Map.map (acrmap realPart) x' :: Map ( Int
                                                                            , Int) (UArray Int Double)
                                _ ->
                                    error $
                                    "evalThreeD RealPart " ++ show inputs
                        ImagPart ->
                            case inputs of
                                [x] ->
                                    let x' =
                                            evalThreeDCSparse
                                                (ThreeDCSparse
                                                     sL
                                                     (Expression x exprs))
                                                eMap :: Map (Int, Int) (Array Int (DC.Complex Double))
                                     in Map.map (acrmap imagPart) x' :: Map ( Int
                                                                            , Int) (UArray Int Double)
                                _ ->
                                    error $
                                    "evalThreeD ImagPart " ++ show inputs
                        _ ->
                            error $
                            "evalThreeSparseD't implemented " ++
                            show op ++ " yet"
            Just e -> error $ "evalThreeSparseD found " ++ show e

{-

-}
evalThreeDC :: ThreeDC -> ValMaps -> Array (Int, Int, Int) (DC.Complex Double)
evalThreeDC (ThreeDC (Expression node exprs)) eMap =
    let evalThreeDC' next =
            evalThreeDC (ThreeDC (Expression next exprs)) eMap :: Array ( Int
                                                                        , Int
                                                                        , Int) (DC.Complex Double)
     in case I.lookup node exprs of
            Nothing ->
                error $
                "evalThreeDC't find node " ++ (take 200 $ show (node, exprs))
            Just (Const (Dim3 (dim1, dim2, dim3)) d) ->
                listArray
                    ((0, 0, 0), (dim1 - 1, dim2 - 1, dim3 - 1))
                    (replicate (dim1 * dim2 * dim3) (d :+ 0))
            Just (Op (Dim3 (dim1, dim2, dim3)) op inputs) ->
                case op of
                    RealImag ->
                        case map (\n ->
                                      evalThreeD
                                          (ThreeD (Expression n exprs))
                                          eMap)
                                 inputs of
                            [res, ims] ->
                                listArray
                                    (bounds res)
                                    [ res ! (i, j, k) :+ ims ! (i, j, k)
                                    | i <- [0 .. dim1 - 1]
                                    , j <- [0 .. dim2 - 1]
                                    , k <- [0 .. dim3 - 1]
                                    ]
                            _ ->
                                error $
                                "HI.evalThreeDC RealImag needs 2 inputs " ++
                                show inputs
                    FT True ->
                        case inputs of
                            [x] ->
                                let e0 = elems $ evalThreeDC' x
                                    r0 =
                                        listArray
                                            ( (0, 0, 0)
                                            , (dim1 - 1, dim2 - 1, dim3 - 1)) $
                                        map DC.realPart e0 :: U.UArray ( Int
                                                                       , Int
                                                                       , Int) Double
                                    i0 =
                                        listArray
                                            ( (0, 0, 0)
                                            , (dim1 - 1, dim2 - 1, dim3 - 1)) $
                                        map DC.imagPart e0 :: U.UArray ( Int
                                                                       , Int
                                                                       , Int) Double
                                    e1 =
                                        concat
                                            [ zip [ (i, j, k)
                                                | k <- [0 .. dim3 - 1]
                                                ] $
                                            error
                                                "FFT.fft [r0 ! (i,j,k) :+ i0 ! (i,j,k) | k <- [0..dim3-1]]"
                                            | i <- [0 .. dim1 - 1]
                                            , j <- [0 .. dim2 - 1]
                                            ]
                                    r1 =
                                        array
                                            ( (0, 0, 0)
                                            , (dim1 - 1, dim2 - 1, dim3 - 1)) $
                                        map (\(i, e) -> (i, DC.realPart e)) e1 :: U.UArray ( Int
                                                                                           , Int
                                                                                           , Int) Double
                                    i1 =
                                        array
                                            ( (0, 0, 0)
                                            , (dim1 - 1, dim2 - 1, dim3 - 1)) $
                                        map (\(i, e) -> (i, DC.imagPart e)) e1 :: U.UArray ( Int
                                                                                           , Int
                                                                                           , Int) Double
                                    e2 =
                                        concat
                                            [ zip [ (i, j, k)
                                                | j <- [0 .. dim2 - 1]
                                                ] $
                                            error
                                                "FFT.fft [r1 ! (i,j,k) :+ i1 ! (i,j,k) | j <- [0..dim2-1]]"
                                            | i <- [0 .. dim1 - 1]
                                            , k <- [0 .. dim3 - 1]
                                            ]
                                    r2 =
                                        array
                                            ( (0, 0, 0)
                                            , (dim1 - 1, dim2 - 1, dim3 - 1)) $
                                        map (\(i, e) -> (i, DC.realPart e)) e2 :: U.UArray ( Int
                                                                                           , Int
                                                                                           , Int) Double
                                    i2 =
                                        array
                                            ( (0, 0, 0)
                                            , (dim1 - 1, dim2 - 1, dim3 - 1)) $
                                        map (\(i, e) -> (i, DC.imagPart e)) e2 :: U.UArray ( Int
                                                                                           , Int
                                                                                           , Int) Double
                                    e3 =
                                        concat
                                            [ zip [ (i, j, k)
                                                | i <- [0 .. dim1 - 1]
                                                ] $
                                            error
                                                "FFT.fft [r2 ! (i,j,k) :+ i2 ! (i,j,k)| i <- [0..dim1-1]]"
                                            | k <- [0 .. dim3 - 1]
                                            , j <- [0 .. dim2 - 1]
                                            ]
                                    r3 =
                                        array
                                            ( (0, 0, 0)
                                            , (dim1 - 1, dim2 - 1, dim3 - 1)) $
                                        map (\(i, e) -> (i, DC.realPart e)) e3 :: U.UArray ( Int
                                                                                           , Int
                                                                                           , Int) Double
                                    i3 =
                                        array
                                            ( (0, 0, 0)
                                            , (dim1 - 1, dim2 - 1, dim3 - 1)) $
                                        map (\(i, e) -> (i, DC.imagPart e)) e3 :: U.UArray ( Int
                                                                                           , Int
                                                                                           , Int) Double
                                 in array
                                        ( (0, 0, 0)
                                        , (dim1 - 1, dim2 - 1, dim3 - 1))
                                        [ ( (i, j, k)
                                          , r3 ! (i, j, k) :+ i3 ! (i, j, k))
                                        | i <- [0 .. dim1 - 1]
                                        , j <- [0 .. dim2 - 1]
                                        , k <- [0 .. dim3 - 1]
                                        ]
                            _ -> error $ "evalThreeDC " ++ show inputs
           -- do the inverse FFT.  Note that the ifft includes a 1/N factor.
                    FT False ->
                        case inputs of
                            [x] ->
                                let s = (fromIntegral $ dim1 * dim2 * dim3)
                                    e0 = elems $ evalThreeDC' x
                                    r0 =
                                        listArray
                                            ( (0, 0, 0)
                                            , (dim1 - 1, dim2 - 1, dim3 - 1)) $
                                        map DC.realPart e0 :: U.UArray ( Int
                                                                       , Int
                                                                       , Int) Double
                                    i0 =
                                        listArray
                                            ( (0, 0, 0)
                                            , (dim1 - 1, dim2 - 1, dim3 - 1)) $
                                        map DC.imagPart e0 :: U.UArray ( Int
                                                                       , Int
                                                                       , Int) Double
                                    e1 =
                                        concat
                                            [ zip [ (i, j, k)
                                                | k <- [0 .. dim3 - 1]
                                                ] $
                                            error
                                                "FFT.ifft [r0 ! (i,j,k) :+ i0 ! (i,j,k) | k <- [0..dim3-1]]"
                                            | i <- [0 .. dim1 - 1]
                                            , j <- [0 .. dim2 - 1]
                                            ]
                                    r1 =
                                        array
                                            ( (0, 0, 0)
                                            , (dim1 - 1, dim2 - 1, dim3 - 1)) $
                                        map (\(i, e) -> (i, DC.realPart e)) e1 :: U.UArray ( Int
                                                                                           , Int
                                                                                           , Int) Double
                                    i1 =
                                        array
                                            ( (0, 0, 0)
                                            , (dim1 - 1, dim2 - 1, dim3 - 1)) $
                                        map (\(i, e) -> (i, DC.imagPart e)) e1 :: U.UArray ( Int
                                                                                           , Int
                                                                                           , Int) Double
                                    e2 =
                                        concat
                                            [ zip [ (i, j, k)
                                                | j <- [0 .. dim2 - 1]
                                                ] $
                                            error
                                                "FFT.ifft [r1 ! (i,j,k) :+ i1 ! (i,j,k) | j <- [0..dim2-1]]"
                                            | i <- [0 .. dim1 - 1]
                                            , k <- [0 .. dim3 - 1]
                                            ]
                                    r2 =
                                        array
                                            ( (0, 0, 0)
                                            , (dim1 - 1, dim2 - 1, dim3 - 1)) $
                                        map (\(i, e) -> (i, DC.realPart e)) e2 :: U.UArray ( Int
                                                                                           , Int
                                                                                           , Int) Double
                                    i2 =
                                        array
                                            ( (0, 0, 0)
                                            , (dim1 - 1, dim2 - 1, dim3 - 1)) $
                                        map (\(i, e) -> (i, DC.imagPart e)) e2 :: U.UArray ( Int
                                                                                           , Int
                                                                                           , Int) Double
                                    e3 =
                                        concat
                                            [ zip [ (i, j, k)
                                                | i <- [0 .. dim1 - 1]
                                                ] $
                                            error
                                                "FFT.ifft [r2 ! (i,j,k) :+ i2 ! (i,j,k)| i <- [0..dim1-1]]"
                                            | k <- [0 .. dim3 - 1]
                                            , j <- [0 .. dim2 - 1]
                                            ]
                                    r3 =
                                        array
                                            ( (0, 0, 0)
                                            , (dim1 - 1, dim2 - 1, dim3 - 1)) $
                                        map (\(i, e) -> (i, DC.realPart e)) e3 :: U.UArray ( Int
                                                                                           , Int
                                                                                           , Int) Double
                                    i3 =
                                        array
                                            ( (0, 0, 0)
                                            , (dim1 - 1, dim2 - 1, dim3 - 1)) $
                                        map (\(i, e) -> (i, DC.imagPart e)) e3 :: U.UArray ( Int
                                                                                           , Int
                                                                                           , Int) Double
                                 in array
                                        ( (0, 0, 0)
                                        , (dim1 - 1, dim2 - 1, dim3 - 1))
                                        [ ( (i, j, k)
                                          , (s * r3 ! (i, j, k)) :+
                                            (s * i3 ! (i, j, k)))
                                        | i <- [0 .. dim1 - 1]
                                        , j <- [0 .. dim2 - 1]
                                        , k <- [0 .. dim3 - 1]
                                        ]
                            _ -> error $ "evalTwoDC " ++ show inputs
                    ScaleV ->
                        let scaleR ::
                                   Double
                                -> Array (Int, Int, Int) (DC.Complex Double)
                                -> Array (Int, Int, Int) (DC.Complex Double)
                            scaleR sr x =
                                listArray
                                    (bounds x)
                                    [ x ! (i, j, k) * (sr :+ 0)
                                    | i <- [0 .. dim1 - 1]
                                    , j <- [0 .. dim2 - 1]
                                    , k <- [0 .. dim3 - 1]
                                    ]
                            scaleC ::
                                   (DC.Complex Double)
                                -> Array (Int, Int, Int) (DC.Complex Double)
                                -> Array (Int, Int, Int) (DC.Complex Double)
                            scaleC s x =
                                listArray
                                    (bounds x)
                                    [ x ! (i, j, k) * s
                                    | i <- [0 .. dim1 - 1]
                                    , j <- [0 .. dim2 - 1]
                                    , k <- [0 .. dim1 - 1]
                                    ]
                         in case inputs of
                                [n1, n2] ->
                                    case (getDimE exprs n1, getDimE exprs n2) of
                                        (Dim0, Dim3 _) ->
                                            if (nodeIsComplex exprs n1)
                                                then scaleC
                                                         (evalZeroDC
                                                              Nothing
                                                              (Expression
                                                                   n1
                                                                   exprs)
                                                              eMap) $
                                                     evalThreeDC' n2
                                                else scaleR
                                                         (evalZeroD
                                                              Nothing
                                                              (Expression
                                                                   n1
                                                                   exprs)
                                                              eMap) $
                                                     evalThreeDC' n2
                                        _ ->
                                            error $
                                            "evalThreeDC ScaleV requires Const as first arg and ThreeD second arg " ++
                                            show inputs
                                _ ->
                                    error $
                                    "evalThreeDC ScaleV requires two arguments  " ++
                                    show inputs
                    Sum ->
                        let sumFun ::
                                   [Array (Int, Int, Int) (DC.Complex Double)]
                                -> (Array (Int, Int, Int) (DC.Complex Double)) -- this seems slow,,,
                            sumFun (as:bs:rest) =
                                sumFun
                                    (U.listArray
                                         (bounds as)
                                         [ (as ! (i, j, k)) + (bs ! (i, j, k))
                                         | i <- [0 .. dim1 - 1]
                                         , j <- [0 .. dim2 - 1]
                                         , k <- [0 .. dim3 - 1]
                                         ] :
                                     rest)
                            sumFun [cs] = cs
                            sumFun [] = error "evalThreeDC.sumFun []"
                         in sumFun $ map (\x -> evalThreeDC' x) inputs
                    Shift (OS3d ((offset1, offset2, offset3), c)) ->
                        let 
                         in case inputs of
                                [x] ->
                                    let original = evalThreeDC' x
                                        shiftedElemAt (i, j, k) =
                                            if i >= offset1 &&
                                               j >= offset2 &&
                                               k >= offset3 &&
                                               i - offset1 < dim1 &&
                                               j - offset2 < dim2 &&
                                               k - offset3 < dim3
                                                then (original !
                                                      ( i - offset1
                                                      , j - offset2
                                                      , k - offset3)) *
                                                     fromReal c
                                                else fromReal 0
                                     in U.listArray
                                            ( (0, 0, 0)
                                            , (dim1 - 1, dim2 - 1, dim3 - 1))
                                            [ shiftedElemAt (i, j, k)
                                            | i <- [0 .. dim1 - 1]
                                            , j <- [0 .. dim2 - 1]
                                            , k <- [0 .. dim3 - 1]
                                            ]
                                _ ->
                                    error
                                        "evalThreeDC wrong number of input Shift, expect 1"
                    _ ->
                        error $
                        "evalThreeDC't implemented " ++ show op ++ " yet"
            Just e -> error $ "evalThreeDC found " ++ show e

{-

-}
evalThreeDCSparse ::
       ThreeDCSparse
    -> ValMaps
    -> Map (Int, Int) (Array Int (DC.Complex Double))
evalThreeDCSparse (ThreeDCSparse sL@(SparseList3D _len2 _ toKeep1 _ toKeep2 _) (Expression node exprs)) eMap =
    let evalThreeDCSparse' (sL, next) =
            evalThreeDCSparse (ThreeDCSparse sL (Expression next exprs)) eMap :: Map ( Int
                                                                                     , Int) (Array Int (DC.Complex Double))
     in case I.lookup node exprs of
            Nothing ->
                error $
                "HInterp - evalThreeDSparse find node " ++
                (take 200 $ show (node, exprs))
            Just (Const (Dim3SL1 (dim1, _, _) _dim) d) ->
                Map.fromList
                    [ ( (i, j)
                      , (listArray (0, dim1 - 1) (replicate dim1 (d :+ 0))))
                    | i <- (elems toKeep1)
                    , j <- (elems toKeep2)
                    ]
            Just (Const (Dim3SL2 (_, dim2, _) _dim) d) ->
                Map.fromList
                    [ ( (i, k)
                      , (listArray (0, dim2 - 1) (replicate dim2 (d :+ 0))))
                    | i <- (elems toKeep1)
                    , k <- (elems toKeep2)
                    ]
            Just (Const (Dim3SL3 (_, _, dim3) _dim) d) ->
                Map.fromList
                    [ ( (j, k)
                      , (listArray (0, dim3 - 1) (replicate dim3 (d :+ 0))))
                    | j <- (elems toKeep1)
                    , k <- (elems toKeep2)
                    ]
            Just (Const _ _) ->
                error
                    "HInterp - evalThreeDCSparse found const with incorrect dims "
            Just (Op dims op inputs) ->
                let dimKeep =
                        case dims of
                            (Dim3SL1 (dim1, _, _) _) -> dim1
                            (Dim3SL2 (_, dim2, _) _) -> dim2
                            (Dim3SL3 (_, _, dim3) _) -> dim3
                            d -> error $ "HInterp.eval3DCS dims " ++ show d
                 in case op of
                        ScaleV ->
                            let scaleR ::
                                       Double
                                    -> Map (Int, Int) (Array Int (DC.Complex Double))
                                    -> Map (Int, Int) (Array Int (DC.Complex Double))
                                scaleR sr x =
                                    Map.map
                                        (\a ->
                                             listArray (0, dimKeep - 1) $
                                             map (* (sr :+ 0)) $ elems a)
                                        x
                                scaleC ::
                                       (DC.Complex Double)
                                    -> Map (Int, Int) (Array Int (DC.Complex Double))
                                    -> Map (Int, Int) (Array Int (DC.Complex Double))
                                scaleC s x =
                                    Map.map
                                        (\a ->
                                             listArray (0, dimKeep - 1) $
                                             map (* s) $ elems a)
                                        x
                             in case inputs of
                                    [n1, n2] ->
                                        case ( getDimE exprs n1
                                             , getDimE exprs n2) of
                                            (Dim0, _) ->
                                                if (nodeIsComplex exprs n1)
                                                    then scaleC
                                                             (evalZeroDC
                                                                  Nothing
                                                                  (Expression
                                                                       n1
                                                                       exprs)
                                                                  eMap) $
                                                         evalThreeDCSparse'
                                                             (sL, n2)
                                                    else scaleR
                                                             (evalZeroD
                                                                  Nothing
                                                                  (Expression
                                                                       n1
                                                                       exprs)
                                                                  eMap) $
                                                         evalThreeDCSparse'
                                                             (sL, n2)
                                            _ ->
                                                error $
                                                "evalThreeDCSparse ScaleV requires Const as first arg and ThreeD second arg " ++
                                                show inputs
                                    _ ->
                                        error $
                                        "evalThreeDC ScaleV requires two arguments  " ++
                                        show inputs
                        Sum ->
                            let sumAr ::
                                       Array Int (DC.Complex Double)
                                    -> Array Int (DC.Complex Double)
                                    -> Array Int (DC.Complex Double)
                                sumAr as bs =
                                    (U.listArray
                                         (0, dimKeep - 1)
                                         [ (as ! i) + (bs ! i)
                                         | i <- [0 .. dimKeep - 1]
                                         ])
                                sumFun ::
                                       [Map (Int, Int) (Array Int (DC.Complex Double))]
                                    -> (Map (Int, Int) (Array Int (DC.Complex Double)))
                                sumFun (as:bs:rest) =
                                    sumFun
                                        ((Map.unionWith (sumAr) as bs) : rest)
                                sumFun [cs] = cs
                                sumFun [] = error "evalThreeDCSparse.sumFun []"
                             in sumFun $
                                map (\x -> evalThreeDCSparse' (sL, x)) inputs
                        Project ss ->
                            let v =
                                    case inputs of
                                        [x] ->
                                            evalThreeDC
                                                (ThreeDC $ Expression x exprs)
                                                eMap
                                        _ ->
                                            error $
                                            "evalThreeDC Project needs 1 input " ++
                                            show (inputs, exprs)
                             in case ss of
                                    (SSList3D (SparseList3D _len2 Diii1 toKeep2 _ toKeep3 _)) ->
                                        Map.fromList
                                            [ ( (j, k)
                                              , listArray
                                                    (0, dimKeep - 1)
                                                    [ v ! (i, j, k)
                                                    | i <- [0 .. dimKeep - 1]
                                                    ])
                                            | j <- (elems toKeep2)
                                            , k <- (elems toKeep3)
                                            ]
                                    _ ->
                                        error $
                                        "evalThreeDCSparse Project doesn't implement " ++
                                        show (ss, exprs)
                        _ ->
                            error $
                            "evalThreeD't implemented " ++ show op ++ " yet"
            Just e -> error $ "evalThreeD found " ++ show e

{-







-}
acrmap f ac = U.listArray (bounds ac) [f (ac ! i) | i <- reverse (indices ac)]

acmap f ac = U.listArray (bounds ac) [f (ac ! i) | i <- reverse (indices ac)]
{-
-}
