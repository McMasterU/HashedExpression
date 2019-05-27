module EvalSpec where

import Control.Monad
import Data.Array.Unboxed as U
import qualified Data.ByteString.Char8 as C
import Data.Complex
import qualified Data.IntMap as I
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import Data.Maybe
import HashedConvZip
import HashedDerivative
import HashedDot
import HashedExpression
import HashedInstances
import HashedInterp
import HashedSimplify
import qualified Polynomials as P
import Test.Hspec
import Test.QuickCheck hiding (scale)

--import System.IO.Unsafe
--import HashedExamples
import Debug.Trace
import Test.Hspec

[x, y, z, u, v, w] = map var ["x", "y", "z", "u", "v", "w"]

[x1, y1, z1, u1, v1, w1] = map (var1d 4) ["X1", "Y1", "Z1", "U1", "V1", "W1"]

[x2, y2, z2, u2, v2, w2] =
    map (var2d (4, 4)) ["X2", "Y2", "Z2", "U2", "V2", "W2"]

[x3, y3, z3, u3, v3, w3] =
    map (var3d (4, 4, 4)) ["X3", "Y3", "Z3", "U3", "V3", "W3"]

zero1 = simplify $ x1 - x1

zero2 = simplify $ x2 - x2

zero3 = simplify $ x3 - x3

{- Here are a few subspaces for testing.   FIXME if you can't turn it into an expression, is probably isn't valid.
-}
subA = SSNyquist [(3, (4, 5))]

subB = SSNyquist [(6, (7, 8))]

subM = SSNyquist [(3, (4, 5)), (6, (7, 8))]

subN = SSNyquist [(3, (4, 5)), (6, (7, 8)), (9, (10, 11))]

subX = (SSCrop [(1, 3), (0, 3)] [4, 4])

subY = (SSCrop [(2, 3), (2, 3)] [4, 4]) --trying to make a degenerate crop; this still makes a Dim2 (2,2); which means it's (probably?) not possible to make a degenerate crop

{- and a few types of subspaces which are not properly defined, so we can't use them
-}
subC = SSUnion [subA, subB]

subD = SSInter [subA, subB]

subE = SSComplement subA

subF = SSCoord [Just 3, Nothing]

{-


\section{Eval Tests}

--gScalarsRnd :: Int -> Generator [(String,Double)]
--gScalarsRnd s = genMap (zip scalarV) $ gComposeAll genListAll (genDblRngRnd (-1000.0, 1000) s)

--gSubs sd = genMap (\s -> (s,[],[],[],[])) (gScalarsRnd sd)

-- pair up the substitution maps with the expressions and map the evaluation over the expressions
--gExprAllScalSub = genZip gExprAllVar (gSubs 654843)
--gExprRndScalSub = genZip gExprAllVar (gSubs 2168743)
\end{comment}

Some sample values
-}
vx1 = [0 .. 16 ^ 1 - 1] :: [Double]

vx2 = [0 .. 16 ^ 2 - 1] :: [Double]

vx3 = [0 .. 16 ^ 3 - 1] :: [Double]

vy1 = (replicate (16 ^ 1) 1) :: [Double]

vy2 = (replicate (16 ^ 2) 1) :: [Double]

vy3 = (replicate (16 ^ 3) 1) :: [Double]

subs1 =
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

subs2 =
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


-}
replist 0 _l = []
replist i l = l ++ (replist (i - 1) l)

vm0_0 = ([("x", 1), ("y", 2), ("z", 3)], [], [], [], [])

vm0_7 = ([("x", 5), ("y", 4)], [], [], [], [])

evalTest0_0 n m o =
    evalScalar x (subs ([("x", n), ("y", m), ("z", o)], [], [], [], [])) == n

evalTest0_1 n m o =
    evalScalar (-x) (subs ([("x", n), ("y", m), ("z", o)], [], [], [], [])) ==
    negate n

evalTest0_2 n m o =
    evalScalar (x + y) (subs ([("x", n), ("y", m), ("z", o)], [], [], [], [])) ==
    n + m

evalTest0_3 n m o =
    evalScalar
        (x + x + x)
        (subs ([("x", n), ("y", m), ("z", o)], [], [], [], [])) ==
    3 * n

evalTest0_4 n m o =
    evalScalar (2 * x) (subs ([("x", n), ("y", m), ("z", o)], [], [], [], [])) ==
    n + n

evalTest0_5 n m o =
    evalScalar (2 * y) (subs ([("x", n), ("y", m), ("z", o)], [], [], [], [])) ==
    m + m

evalTest0_1b n m o
    | n /= 0 =
        evalScalar (-x) (subs ([("x", n), ("y", m), ("z", o)], [], [], [], [])) /=
        n -- should fail
    | otherwise =
        evalScalar (-x) (subs ([("x", n), ("y", m), ("z", o)], [], [], [], [])) ==
        n

evalTest0_7 a b =
    evalScalar (xRe (x +: y)) (subs ([("x", a), ("y", b)], [], [], [], [])) == a

evalTest0_8 a b =
    evalScalar (xIm (x +: y)) (subs ([("x", a), ("y", b)], [], [], [], [])) == b

evalTest1_0 n a1 a2 a3 a4 a5 a6 =
    let size = 2 + (n `mod` 10)
        x1 = var1d size "X1"
     in evalOneD
            (simplify $ x1)
            (subs
                 ( []
                 , [ ( "X1"
                     , U.listArray
                           (0, size - 1)
                           (a1 : a2 : a3 : a4 : a5 : (replicate size a6)))
                   ]
                 , []
                 , []
                 , [])) ==
        U.listArray (0, size - 1) (a1 : a2 : a3 : a4 : a5 : (replicate size a6))

evalTest1_1 n a1 a2 a3 a4 a5 a6 =
    let size = 2 + (n `mod` 10)
        x1 = var1d size "X1"
     in evalOneD
            (simplify $ -x1)
            (subs
                 ( []
                 , [ ( "X1"
                     , U.listArray
                           (0, size - 1)
                           (a1 : a2 : a3 : a4 : a5 : (replicate size a6)))
                   ]
                 , []
                 , []
                 , [])) ==
        U.listArray
            (0, size - 1)
            (map (* (-1)) (a1 : a2 : a3 : a4 : a5 : (replicate size a6)))

evalTest1_2 n a1 a2 a3 a4 a5 a6 =
    let size = 2 + (n `mod` 10)
        x1 = var1d size "X1"
     in evalOneD
            (simplify $ x1 + x1)
            (subs
                 ( []
                 , [ ( "X1"
                     , U.listArray
                           (0, size - 1)
                           (a1 : a2 : a3 : a4 : a5 : (replicate size a6)))
                   ]
                 , []
                 , []
                 , [])) ==
        U.listArray
            (0, size - 1)
            (map (* 2) (a1 : a2 : a3 : a4 : a5 : (replicate size a6)))

evalTest1_3 n a1 a2 a3 a4 a5 a6 =
    let size = 2 + (n `mod` 10)
        x1 = var1d size "X1"
     in evalOneD
            (simplify $ x1 + x1 + x1)
            (subs
                 ( []
                 , [ ( "X1"
                     , U.listArray
                           (0, size - 1)
                           (a1 : a2 : a3 : a4 : a5 : (replicate size a6)))
                   ]
                 , []
                 , []
                 , [])) ==
        U.listArray
            (0, size - 1)
            (map (* 3) (a1 : a2 : a3 : a4 : a5 : (replicate size a6)))

evalTest1_4 n a1 a2 a3 a4 a5 a6 =
    let size = 2 + (n `mod` 10)
        x1 = var1d size "X1"
     in evalOneD
            (simplify $ 2.0 `scale` x1)
            (subs
                 ( []
                 , [ ( "X1"
                     , U.listArray
                           (0, size - 1)
                           (a1 : a2 : a3 : a4 : a5 : (replicate size a6)))
                   ]
                 , []
                 , []
                 , [])) ==
        U.listArray
            (0, size - 1)
            (map (* 2) (a1 : a2 : a3 : a4 : a5 : (replicate size a6)))

evalTest1_5 ::
       Int
    -> Int
    -> Int
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Bool
evalTest1_5 i n h a1 a2 a3 a4 a5 a6 =
    let m = h `mod` 10 + 2
        o = i `mod` 10 + 2
        size = 2 * (abs m) + 2 * (abs o) + (abs n) `mod` 10 + 2
        x1 = var1d size "X1"
        list = a1 : a2 : a3 : a4 : a5 : (replicate (size - 5) a6)
        left =
            evalOneD
                (([(abs m, (abs m + abs o))], [size]) `crop` x1)
                (subs ([], [("X1", U.listArray (0, size - 1) list)], [], [], []))
        right =
            U.listArray (0, abs o) (take (1 + abs o) $ drop (abs m) list) :: U.UArray Int Double
     in if left == right
            then True
            else error $ "1_5 " ++ show (left, right)

evalTest1_6 h j i a1 a2 a3 a4 a5 a6 =
    let m = abs $ h `mod` 10
        o = abs $ i `mod` 10
        j' = abs $ j `mod` 10
        n =
            if m + o + j' == 0
                then j' + 2
                else j'
        size = m + o + n
        list = take o $ a1 : a2 : a3 : a4 : a5 : [a6,a6 + 1 ..]
        x1 = var1d size "X1"
        left =
            evalOneD
                (([(m, m + o - 1)], [size]) `uncrop` x1)
                (subs ([], [("X1", U.listArray (0, o - 1) list)], [], [], []))
        right =
            U.listArray
                (0, size - 1)
                ((replicate m 0) ++ list ++ (replicate n 0)) :: U.UArray Int Double
     in if left == right
            then True
            else error $ "1_6 " ++ show (left, right)

--fails quickCheck
evalTest1_7 n a1 a2 a3 a4 a5 a6 =
    let size = 2 + (n `mod` 10)
        x1 = var1d size "X1"
     in evalOneD
            (xRe (x1 +: x1))
            (subs
                 ( []
                 , [ ( "X1"
                     , U.listArray
                           (0, size - 1)
                           (a1 : a2 : a3 : a4 : a5 : (replicate size a6)))
                   ]
                 , []
                 , []
                 , [])) ==
        U.listArray (0, size - 1) (a1 : a2 : a3 : a4 : a5 : (replicate size a6))

evalTest1_8 n a1 a2 a3 a4 a5 a6 =
    let size = 2 + (n `mod` 10)
        x1 = var1d size "X1"
     in evalOneD
            (xIm (x1 +: x1))
            (subs
                 ( []
                 , [ ( "X1"
                     , U.listArray
                           (0, size - 1)
                           (a1 : a2 : a3 : a4 : a5 : (replicate size a6)))
                   ]
                 , []
                 , []
                 , [])) ==
        U.listArray (0, size - 1) (a1 : a2 : a3 : a4 : a5 : (replicate size a6))

evalTest2_0 n a1 a2 a3 a4 a5 a6 =
    let size = 2 + (n `mod` 10)
        x2 = var2d (size, size) "X2"
     in evalTwoD
            (simplify $ x2)
            ((subs
                  ( []
                  , []
                  , [ ( "X2"
                      , U.listArray
                            ((0, 0), (size - 1, size - 1))
                            (a1 :
                             a2 : a3 : a4 : a5 : ((replicate (size ^ 2 - 6) a6))))
                    ]
                  , []
                  , []))) ==
        U.listArray
            ((0, 0), (size - 1, size - 1))
            (a1 : a2 : a3 : a4 : a5 : (replicate (size ^ 2 - 6) a6))

evalTest2_1 n a1 a2 a3 a4 a5 a6 =
    let size = 2 + (n `mod` 10)
        x2 = var2d (size, size) "X2"
     in evalTwoD
            (simplify $ -x2)
            (subs
                 ( []
                 , []
                 , [ ( "X2"
                     , U.listArray
                           ((0, 0), (size - 1, size - 1))
                           (a1 :
                            a2 : a3 : a4 : a5 : ((replicate (size ^ 2 - 6) a6))))
                   ]
                 , []
                 , [])) ==
        U.listArray
            ((0, 0), (size - 1, size - 1))
            (map negate (a1 : a2 : a3 : a4 : a5 : (replicate (size ^ 2 - 6) a6)))

evalTest2_2 n a1 a2 a3 a4 a5 a6 =
    let size = 2 + (n `mod` 10)
        x2 = var2d (size, size) "X2"
     in evalTwoD
            (simplify $ (x2 + x2))
            (subs
                 ( []
                 , []
                 , [ ( "X2"
                     , U.listArray
                           ((0, 0), (size - 1, size - 1))
                           (a1 :
                            a2 : a3 : a4 : a5 : ((replicate (size ^ 2 - 6) a6))))
                   ]
                 , []
                 , [])) ==
        U.listArray
            ((0, 0), (size - 1, size - 1))
            (map (* 2) (a1 : a2 : a3 : a4 : a5 : (replicate (size ^ 2 - 6) a6)))

evalTest2_3 n a1 a2 a3 a4 a5 a6 =
    let size = 2 + (n `mod` 10)
        x2 = var2d (size, size) "X2"
     in evalTwoD
            (simplify $ x2 + x2 + x2)
            (subs
                 ( []
                 , []
                 , [ ( "X2"
                     , U.listArray
                           ((0, 0), (size - 1, size - 1))
                           (a1 :
                            a2 : a3 : a4 : a5 : ((replicate (size ^ 2 - 6) a6))))
                   ]
                 , []
                 , [])) ==
        U.listArray
            ((0, 0), (size - 1, size - 1))
            (map (* 3) (a1 : a2 : a3 : a4 : a5 : (replicate (size ^ 2 - 6) a6)))

evalTest2_4 n a1 a2 a3 a4 a5 a6 =
    let size = 2 + (n `mod` 10)
        x2 = var2d (size, size) "X2"
     in evalTwoD
            (simplify $ (2.0 `scale` x2))
            (subs
                 ( []
                 , []
                 , [ ( "X2"
                     , U.listArray
                           ((0, 0), (size - 1, size - 1))
                           (a1 :
                            a2 : a3 : a4 : a5 : ((replicate (size ^ 2 - 6) a6))))
                   ]
                 , []
                 , [])) ==
        U.listArray
            ((0, 0), (size - 1, size - 1))
            (map (* 2) (a1 : a2 : a3 : a4 : a5 : (replicate (size ^ 2 - 6) a6)))

--evalTest2_5 m n o p q =
--  let
--    h = (abs m) `mod` 10 + 1
--    i = (abs n) `mod` 10 + ((abs m) `mod` 10) + 1
--    j = (abs o) `mod` 10 + 1
--    k = (abs o) `mod` 10 + ((abs p) `mod` 10) + 1
--    size = i + k + q `mod` 10 + 1
--    x2 = var2d (size,size) "X2"
--    right = U.array ((0,j-h),(0,k-i)) ([((x,y),fromIntegral $ 100 * x + y) | x <- [0..], y <- [0..5]]) :: U.UArray (Int, Int) Double
--    left = evalTwoD (([(h,j),(i,k)],[size,size]) `crop` x2) (subs ([], [], [("X2",U.array ((0,size-1),(0,size-1)) ([((i,j),fromIntegral $ 100 * i + j) | i <- [0..1], j <- [0..5]]))],[],[]))
--  in
--    if left == right then True
--      else error $ "2_5 " ++ show (left,right)
--evalTest2_6 m n o p q a1 a2 a3 a4 a5 a6 =
--  let
--    h = (abs m) `mod` 10 + 1
--    i = (abs n) `mod` 10 + ((abs m) `mod` 10) + 1
--    j = (abs o) `mod` 10 + 1
--    k = (abs o) `mod` 10 + ((abs p) `mod` 10) + 1
--    size = i + k + q `mod` 10 + 1
--    x2 = var2d (size,size) "X2"
--    left = evalTwoD (([(h,i),(j,k)],[size,size]) `uncrop` x2) (subs ([], [], [("X2",U.listArray ((0,0),(size-1,size-1)) (a1:a2:a3:a4:a5:(replicate (size^2-6) a6)))],[],[]))
--    right = U.array ((0,j-h),(0,k-i)) ((replicate size 0.0) ++ (replist 14 ([0.0] ++ replicate 14 1.0 ++ [0.0])) ++ (replicate 16 0.0)) :: U.UArray (Int,Int) Double
--  in
--    left == right
evalTest2_7 n a1 a2 a3 a4 a5 a6 =
    let size = 2 + (n `mod` 10)
        x2 = var2d (size, size) "X2"
     in evalTwoD
            (xRe (x2 +: x2))
            (subs
                 ( []
                 , []
                 , [ ( "X2"
                     , U.listArray
                           ((0, 0), (size - 1, size - 1))
                           (a1 :
                            a2 : a3 : a4 : a5 : ((replicate (size ^ 2 - 6) a6))))
                   ]
                 , []
                 , [])) ==
        U.listArray
            ((0, 0), (size - 1, size - 1))
            (a1 : a2 : a3 : a4 : a5 : (replicate (size ^ 2 - 6) a6))

evalTest2_8 n a1 a2 a3 a4 a5 a6 =
    let size = 2 + (n `mod` 10)
        x2 = var2d (size, size) "X2"
     in evalTwoD
            (xIm (x2 +: x2))
            (subs
                 ( []
                 , []
                 , [ ( "X2"
                     , U.listArray
                           ((0, 0), (size - 1, size - 1))
                           (a1 :
                            a2 : a3 : a4 : a5 : ((replicate (size ^ 2 - 6) a6))))
                   ]
                 , []
                 , [])) ==
        U.listArray
            ((0, 0), (size - 1, size - 1))
            (a1 : a2 : a3 : a4 : a5 : (replicate (size ^ 2 - 6) a6))

{-

--\begin{code}

x2 = var2d 16 "X2"

evalTest2_0 = evalTwoD (subs ([], [], [("X2",U.listArray ((0,0),(15,15)) vx2)],[],[])) == U.listArray ((0,0),(15,15)) vx2
evalTest2_1 = evalTwoD (-x2) (subs ([], [], [("X2",U.listArray ((0,0),(15,15)) vx2)],[],[])) == U.listArray ((0,0),(15,15)) (map negate vx2)
evalTest2_2 = evalTwoD (x2+x2) (subs ([], [], [("X2",U.listArray ((0,0),(15,15)) vx2)],[],[])) == U.listArray ((0,0),(15,15)) (map (*2) vx2)
evalTest2_3 = evalTwoD (simplify $ x2+x2+x2) (subs ([], [], [("X2",U.listArray ((0,0),(15,15)) vx2)],[],[])) == U.listArray ((0,0),(15,15)) (map (*3) vx2)
evalTest2_4 = evalTwoD (2.0 `scale` x2) (subs ([], [], [("X2",U.listArray ((0,0),(15,15)) vx2)],[],[])) == U.listArray ((0,0),(15,15)) (map (*2) vx2)
evalTest2_5 = evalTwoD (([(1,14),(1,14)],[16,16]) `crop` x2) (subs ([], [], [("Y2",U.listArray ((0,0),(15,15)) vx2)],[],[]))  == U.listArray ((0,0),(13,13)) (replicate (15^2) 1.0)
--evalTest2_6 = evalTwoD (([(1,14),(1,14)],[16,16]) `uncrop` x2) (subs ([], [], [("Y2",U.listArray ((0,0),(15,15)) vx2)],[],[]))
--t = U.listArray ((0,0),(15,15)) ((replicate 16 0.0) ++ (replist 14 ([0.0] ++ replicate 14 1.0 ++ [0.0])) ++ (replicate 16 0.0)) :: U.UArray (Int,Int) Double
evalTest2_7 = evalTwoD (xRe (x2 +: x2)) (subs ([], [], [("X2",U.listArray ((0,0),(15,15)) vx2)],[],[])) == U.listArray ((0,0),(15,15)) vx2
evalTest2_8 = evalTwoD (xIm (x2 +: x2)) (subs ([], [], [("X2",U.listArray ((0,0),(15,15)) vx2)],[],[])) == U.listArray ((0,0),(15,15)) vx2

--\end{code}
--\begin{code}

evalTest3_0 = evalThreeD (subs ([],[],[], [("X3", U.listArray ((0,0,0),(15,15,15)) vx3)],[])) == U.listArray ((0,0,0),(15,15,15))  vx3
evalTest3_1 = evalThreeD (-x3) (subs ([],[],[], [("X3", U.listArray ((0,0,0),(15,15,15)) vx3)],[])) == U.listArray ((0,0,0),(15,15,15))  (map negate vx3)
evalTest3_2 = evalThreeD (x3+x3) (subs ([],[],[], [("X3", U.listArray ((0,0,0),(15,15,15)) vx3)],[])) == U.listArray ((0,0,0),(15,15,15))  (map (*2) vx3)
evalTest3_3 = evalThreeD (x3+x3+x3) (subs ([],[],[], [("X3", U.listArray ((0,0,0),(15,15,15)) vx3)],[])) == U.listArray ((0,0,0),(15,15,15))  (map (*3) vx3)
evalTest3_4 = evalThreeD (2.0 `scale` x3) (subs ([],[],[], [("X3", U.listArray ((0,0,0),(15,15,15)) vx3)],[])) == U.listArray ((0,0,0),(15,15,15))  (map (*2) vx3)
evalTest3_5 = evalThreeD (([(1,15),(1,15),(1,15)],[16,16,16]) `crop` y3) (subs ([],[],[], [("Y3", U.listArray ((0,0,0),(15,15,15)) vy3)],[])) ==
        U.listArray ((0,0,0),(14,14,14))  (replicate (15^3) 1)
evalTest3_6 = evalThreeD (([(1,15),(0,15),(0,15)],[16,16,16]) `uncrop` y3) (subs ([],[],[], [("Y3", U.listArray ((0,0,0),(15,15,15)) vy3)],[])) == U.listArray ((0,0,0),(15,15,15)) ((replicate (16^2) 0)++(replicate (15*16^2) 1))
evalTest3_7 = evalThreeD (xRe (x3 +: x3)) (subs ([],[],[], [("X3", U.listArray ((0,0,0),(15,15,15)) vx3)],[])) == U.listArray ((0,0,0),(15,15,15))  vx3
evalTest3_8 = evalThreeD (xIm (x3 +: x3)) (subs ([],[],[], [("X3", U.listArray ((0,0,0),(15,15,15)) vx3)],[])) == U.listArray ((0,0,0),(15,15,15))  vx3

--\end{code}

-}
evalTest4_1 str = "0.0" == (show $ simplify $ 0 * (var $ "x" ++ str)) -- FIXME broken as of 12May2015

evalTest3_0 n a1 a2 a3 a4 a5 a6 =
    let x3 = var3d (size, size, size) "X3"
        size = 2 + (n `mod` 10)
     in evalThreeD
            (simplify $ x3)
            (subs
                 ( []
                 , []
                 , []
                 , [ ( "X3"
                     , U.listArray
                           ((0, 0, 0), (size - 1, size - 1, size - 1))
                           (a1 :
                            a2 : a3 : a4 : a5 : (replicate (size ^ 3 - 6) a6)))
                   ]
                 , [])) ==
        U.listArray
            ((0, 0, 0), (size - 1, size - 1, size - 1))
            (a1 : a2 : a3 : a4 : a5 : (replicate (size ^ 3 - 6) a6))

evalTest3_1 n a1 a2 a3 a4 a5 a6 =
    let x3 = var3d (size, size, size) "X3"
        size = 2 + (n `mod` 10)
     in evalThreeD
            (simplify $ -x3)
            (subs
                 ( []
                 , []
                 , []
                 , [ ( "X3"
                     , U.listArray
                           ((0, 0, 0), (size - 1, size - 1, size - 1))
                           (a1 :
                            a2 : a3 : a4 : a5 : (replicate (size ^ 3 - 6) a6)))
                   ]
                 , [])) ==
        U.listArray
            ((0, 0, 0), (size - 1, size - 1, size - 1))
            (map negate (a1 : a2 : a3 : a4 : a5 : (replicate (size ^ 3 - 6) a6)))

evalTest3_2 n a1 a2 a3 a4 a5 a6 =
    let x3 = var3d (size, size, size) "X3"
        size = 2 + (n `mod` 10)
     in evalThreeD
            (simplify $ (x3 + x3))
            (subs
                 ( []
                 , []
                 , []
                 , [ ( "X3"
                     , U.listArray
                           ((0, 0, 0), (size - 1, size - 1, size - 1))
                           (a1 :
                            a2 : a3 : a4 : a5 : (replicate (size ^ 3 - 6) a6)))
                   ]
                 , [])) ==
        U.listArray
            ((0, 0, 0), (size - 1, size - 1, size - 1))
            (map (* 2) (a1 : a2 : a3 : a4 : a5 : (replicate (size ^ 3 - 6) a6)))

evalTest3_3 n a1 a2 a3 a4 a5 a6 =
    let x3 = var3d (size, size, size) "X3"
        size = 2 + (n `mod` 10)
     in evalThreeD
            (simplify $ (x3 + x3 + x3))
            (subs
                 ( []
                 , []
                 , []
                 , [ ( "X3"
                     , U.listArray
                           ((0, 0, 0), (size - 1, size - 1, size - 1))
                           (a1 :
                            a2 : a3 : a4 : a5 : (replicate (size ^ 3 - 6) a6)))
                   ]
                 , [])) ==
        U.listArray
            ((0, 0, 0), (size - 1, size - 1, size - 1))
            (map (* 3) (a1 : a2 : a3 : a4 : a5 : (replicate (size ^ 3 - 6) a6)))

evalTest3_4 n a1 a2 a3 a4 a5 a6 =
    let x3 = var3d (size, size, size) "X3"
        size = 2 + (n `mod` 10)
     in evalThreeD
            (simplify $ 2.0 `scale` x3)
            (subs
                 ( []
                 , []
                 , []
                 , [ ( "X3"
                     , U.listArray
                           ((0, 0, 0), (size - 1, size - 1, size - 1))
                           (a1 :
                            a2 : a3 : a4 : a5 : (replicate (size ^ 3 - 6) a6)))
                   ]
                 , [])) ==
        U.listArray
            ((0, 0, 0), (size - 1, size - 1, size - 1))
            (map (* 2) (a1 : a2 : a3 : a4 : a5 : (replicate (size ^ 3 - 6) a6)))

evalTest3_5 n a1 a2 a3 a4 a5 a6 =
    let x3 = var3d (size, size, size) "X3"
        size = 2 + (n `mod` 10)
     in evalThreeD
            (([(1, 15), (1, 15), (1, 15)], [16, 16, 16]) `crop` x3)
            (subs
                 ( []
                 , []
                 , []
                 , [ ( "X3"
                     , U.listArray
                           ((0, 0, 0), (size - 1, size - 1, size - 1))
                           (a1 :
                            a2 : a3 : a4 : a5 : (replicate (size ^ 3 - 6) a6)))
                   ]
                 , [])) ==
        U.listArray ((0, 0, 0), (14, 14, 14)) (replicate (15 ^ 3) 1)

evalTest3_6 n a1 a2 a3 a4 a5 a6 =
    let x3 = var3d (size, size, size) "X3"
        size = 2 + (n `mod` 10)
     in evalThreeD
            (([(1, 15), (0, 15), (0, 15)], [16, 16, 16]) `uncrop` x3)
            (subs
                 ( []
                 , []
                 , []
                 , [ ( "X3"
                     , U.listArray
                           ((0, 0, 0), (size - 1, size - 1, size - 1))
                           (a1 :
                            a2 : a3 : a4 : a5 : (replicate (size ^ 3 - 6) a6)))
                   ]
                 , [])) ==
        U.listArray
            ((0, 0, 0), (size - 1, size - 1, size - 1))
            ((replicate (16 ^ 2) 0) ++ (replicate (15 * 16 ^ 2) 1))

evalTest3_7 n a1 a2 a3 a4 a5 a6 =
    let x3 = var3d (size, size, size) "X3"
        size = 2 + (n `mod` 10)
     in evalThreeD
            (xRe (x3 +: x3))
            (subs
                 ( []
                 , []
                 , []
                 , [ ( "X3"
                     , U.listArray
                           ((0, 0, 0), (size - 1, size - 1, size - 1))
                           (a1 :
                            a2 : a3 : a4 : a5 : (replicate (size ^ 3 - 6) a6)))
                   ]
                 , [])) ==
        U.listArray
            ((0, 0, 0), (size - 1, size - 1, size - 1))
            (a1 : a2 : a3 : a4 : a5 : (replicate (size ^ 3 - 6) a6))

evalTest3_8 n a1 a2 a3 a4 a5 a6 =
    let x3 = var3d (size, size, size) "X3"
        size = 2 + (n `mod` 10)
     in evalThreeD
            (xIm (x3 +: x3))
            (subs
                 ( []
                 , []
                 , []
                 , [ ( "X3"
                     , U.listArray
                           ((0, 0, 0), (size - 1, size - 1, size - 1))
                           (a1 :
                            a2 : a3 : a4 : a5 : (replicate (size ^ 3 - 6) a6)))
                   ]
                 , [])) ==
        U.listArray
            ((0, 0, 0), (size - 1, size - 1, size - 1))
            (a1 : a2 : a3 : a4 : a5 : (replicate (size ^ 3 - 6) a6))

{-
--\begin{code}



evalTestC2_0 = evalTwoDC (simplify (ft (z2 +: y2))) $ subs ([],[],[("Z2", U.listArray ((0,0),(15,15)) (replicate (16^2) 0)),  ("Y2", U.listArray ((0,0),(15,15))  (replicate (16^2) 0))],[],[])

evalTestC3_0 = evalThreeDC (ft (z3 +: y3)) $ subs ([],[],[],[("Z3", U.listArray ((0,0,0),(15,15,15)) (replicate (16^3) 0)),  ("Y3", U.listArray ((0,0,0),(15,15,15))  (replicate (16^3) 0))],[])
evalTestC3_1 = evalThreeDC (ft (z3 +: y3)) $ subs ([],[],[],[("Z3", U.listArray ((0,0,0),(15,15,15)) (replicate (16^3) 0)),  ("Y3", U.listArray ((0,0,0),(15,15,15))  ([1] ++ replicate (16^3-1) 0))],[])
evalTestC3_2 = evalThreeDC (ft (z3 +: y3)) $ subs ([],[],[],[("Z3", U.listArray ((0,0,0),(15,15,15)) (replicate (16^3) 0)),  ("Y3", U.listArray ((0,0,0),(15,15,15))  ((replicate (16^3-20) 0) ++ [1] ++ replicate 19 0 ))],[])






--\end{code}

Show functions

-}
{-

\begin{comment}
--\begin{code}
same2d a1 a2 = let
  ((_0,_0'),(dim1,dim2)) = bounds a1
  test = [[ if (a1 ! (i,j) == a2 ! (i,j)) then True else error "do not match at: (" ++ show i ++ "," ++ show j ++ ") " | j <- [0..dim1-1]] | i <- [0..dim2-1]]
  in True


replist 0 l = []
replist i l = l++l++(replist (i-1) l)


--\end{code}

\section{Diff Tests}

 functions to test
test_funcs = [{- JLMP t1,t2,t3,-} t4,t5,t6,t7]

--JLMP t1 = norm2 $ comb [(2,(0,1))] $ (ft (x1,y1)) - (u1,v1) :: Scalar
--JLMP t2 = norm2 $ (ft (x1,y1)) - (u1,v1) :: Scalar
--JLMP t3 = norm2 $ (ft (x1,y1)) :: Scalar
t4 = norm2 $ x1 - u1 :: Scalar
t5 = phi2 x2
t6 = diff (mp ["X2"]) $ t5
t7 = l2diff x2


-- derivatives of functions to tests
--JLMP test_ders = [td1,td2,td3,td4]

--JLMP td1 = diff (mp ["X","Y"]) t1
--JLMP td2 = diff (mp ["X","Y"]) t2
--JLMP td3 = diff (mp ["X","Y"]) t3
-- td4 = diff (mp ["X","Y"]) t4
\end{comment}

Tests of simplification.
Things which should be zero.  Taking derivative with respect to variables not present.
-}
{-
-}
spec :: Spec
spec =
    describe "eval test" $ do
        specify "evalTest0_0" $ property evalTest0_0
        specify "evalTest0_1" $ property evalTest0_1
        specify "evalTest0_2" $ property evalTest0_2
        specify "evalTest0_3" $ property evalTest0_3
        specify "evalTest0_4" $ property evalTest0_4
        specify "evalTest0_5" $ property evalTest0_5
        specify "evalTest0_1b" $ property evalTest0_1b
        specify "evalTest0_7" $ property evalTest0_7
        specify "evalTest0_8" $ property evalTest0_8
        specify "evalTest1_0" $ property evalTest1_0
        specify "evalTest1_1" $ property evalTest1_1
        specify "evalTest1_2" $ property evalTest1_2
        specify "evalTest1_3" $ property evalTest1_3
        specify "evalTest1_4" $ property evalTest1_4
        specify "evalTest1_5" $ property evalTest1_5
        specify "evalTest1_6" $ property evalTest1_6
        specify "evalTest1_7" $ property evalTest1_7
        specify "evalTest1_8" $ property evalTest1_8
        specify "evalTest2_0" $ property evalTest2_0
        specify "evalTest2_1" $ property evalTest2_1
        specify "evalTest2_2" $ property evalTest2_2
        specify "evalTest2_3" $ property evalTest2_3
        specify "evalTest2_4" $ property evalTest2_4
        {-specify "evalTest2_6" $
            property evalTest2_6-}
        specify "evalTest2_7" $ property evalTest2_7
        specify "evalTest2_8" $ property evalTest2_8
        specify "evalTest3_0" $ property evalTest3_0
        specify "evalTest3_1" $ property evalTest3_1
        specify "evalTest3_2" $ property evalTest3_2
        specify "evalTest3_3" $ property evalTest3_3
        specify "evalTest3_4" $ property evalTest3_4
        --specify "evalTest3_5" $ property evalTest3_5
        --specify "evalTest3_6" $ property evalTest3_6
        specify "evalTest3_7" $ property evalTest3_7
        specify "evalTest3_8" $ property evalTest3_8
        specify "evalTest4_1" $ property evalTest4_1
