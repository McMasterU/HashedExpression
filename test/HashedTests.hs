{-
\documentclass[10pt]{amsart}

\usepackage{comment}         %for block comments \begin{comment}...\end{comment}
\usepackage{geometry}
\usepackage{algpseudocode}
\usepackage{algorithm}
\usepackage{titletoc}        %for making a list of the sections; not used yet
\usepackage{listingsutf8}    %package for code environment; use this instead of verbatim to get automatic line break; use this instead of listings to get (<.>)
\lstnewenvironment{code}     %defining the code environment
  {\lstset{
    language=Haskell,
    basicstyle=\scriptsize\ttfamily,
    breaklines=true,             % for code to break at end of line
    literate={<.>}{{$\bullet$}}1,  % defining the bullet
    }}
  {}

\begin{document}

\title{HashedTests.lhs}
\maketitle

(c) 2010-2015 Christopher Kumar Anand, Jessica LM Pavlin, Konrad AF Anand, Tanya Bouman

Tests for new expression library.
Compile with

|ghc -O2 -main-is HashedTests.regressionTestAll HashedTests.lhs|

\begin{comment}
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HashedTests where

import HashedDerivative
import HashedExpression
import HashedInstances
import HashedInterp

-- import HashedComplexInstances
import HashedSimplify

-- import HashedMRI --for the R2Star tests
import HashedConvZip
import HashedDot
import qualified Polynomials as P

-- import R2Star
--import Maybe (isJust)
--import Data.IntMap (IntMap)
import Control.Monad
import Data.Array.Unboxed as U
import qualified Data.ByteString.Char8 as C
import Data.Complex
import qualified Data.IntMap as I
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import Data.Maybe
import Test.QuickCheck hiding (scale)

--import System.IO.Unsafe
--import HashedExamples
import Debug.Trace
[x, y, z, u, v, w] = map var ["x", "y", "z", "u", "v", "w"]

[x1, y1, z1, u1, v1, w1] = map (var1d 4) ["X1", "Y1", "Z1", "U1", "V1", "W1"]

[x2, y2, z2, u2, v2, w2] =
    map (var2d (4, 4)) ["X2", "Y2", "Z2", "U2", "V2", "W2"]

[x3, y3, z3, u3, v3, w3] =
    map (var3d (4, 4, 4)) ["X3", "Y3", "Z3", "U3", "V3", "W3"]

zero1 = simplify $ x1 - x1

zero2 = simplify $ x2 - x2

zero3 = simplify $ x3 - x3


{-

FIXME sin (asin x) == x

\subsection{applyOne}
Previous testing used all of simplify.  Now we want to check which rules do what.

First, applyOne, the function which applies the simp1 rules.  It only applies simplifications to the top node so far.
-}
scalarApplyOne (Scalar (Expression n e)) =
    case applyOne (e, n) simp1 of
        Nothing -> pretty $ Expression n e
        _ -> pretty $ fromJust $ applyOne (e, n) simp1

{-

Now some tests.
-}
--simpTest11_0 = scalarApplyOne      something else to try--simplify (czZip (\x y -> x*y) (xRe (x1+:y1)) (xIm (x1 +: y1)))
{-

For testing multiple tests at a time, I followed the same format as evalTest and diffTests.  The test prints true or false, however, I leave quickCheck for tests which produce exceptions because otherwise the test gets stuck at the exception.
-}
--



--Run all QuickCheck Tests through regressionTest
regressionTestAll =
    sequence [regressionTestEval, regressionTestPartDiff]

regressionTestEval = sequence [regressionTestEval1]

regressionTestEval1 =
    sequence
        [ putStrLn "evalTest0_0"
        , quickCheck evalTest0_0
        , putStrLn "evalTest0_1"
        , quickCheck evalTest0_1
        , putStrLn "evalTest0_2"
        , quickCheck evalTest0_2
        , putStrLn "evalTest0_3"
        , quickCheck evalTest0_3
        , putStrLn "evalTest0_4"
        , quickCheck evalTest0_4
        , putStrLn "evalTest0_5"
        , quickCheck evalTest0_5
        , putStrLn "evalTest0_1b"
        , quickCheck evalTest0_1b
        , putStrLn "evalTest0_7"
        , quickCheck evalTest0_7
        , putStrLn "evalTest0_8"
        , quickCheck evalTest0_8
        , putStrLn "evalTest1_0"
        , quickCheck evalTest1_0
        , putStrLn "evalTest1_1"
        , quickCheck evalTest1_1
        , putStrLn "evalTest1_2"
        , quickCheck evalTest1_2
        , putStrLn "evalTest1_3"
        , quickCheck evalTest1_3
        , putStrLn "evalTest1_4"
        , quickCheck evalTest1_4
        , putStrLn "evalTest1_5"
        , quickCheck evalTest1_5
        , putStrLn "evalTest1_6"
        , quickCheck evalTest1_6
        , putStrLn "evalTest1_7"
        , quickCheck evalTest1_7
        , putStrLn "evalTest1_8"
        , quickCheck evalTest1_8
        , putStrLn "evalTest2_0"
        , quickCheck evalTest2_0
        , putStrLn "evalTest2_1"
        , quickCheck evalTest2_1
        , putStrLn "evalTest2_2"
        , quickCheck evalTest2_2
        , putStrLn "evalTest2_3"
        , quickCheck evalTest2_3
        , putStrLn "evalTest2_4"
        , quickCheck evalTest2_4
    --                      , putStrLn "evalTest2_6", quickCheck evalTest2_6
        , putStrLn "evalTest2_7"
        , quickCheck evalTest2_7
        , putStrLn "evalTest2_8"
        , quickCheck evalTest2_8
        , putStrLn "evalTest3_0"
        , quickCheck evalTest3_0
        , putStrLn "evalTest3_1"
        , quickCheck evalTest3_1
        , putStrLn "evalTest3_2"
        , quickCheck evalTest3_2
        , putStrLn "evalTest3_3"
        , quickCheck evalTest3_3
        , putStrLn "evalTest3_4"
        , quickCheck evalTest3_4
        , putStrLn "evalTest3_5"
        , quickCheck evalTest3_5
        , putStrLn "evalTest3_6"
        , quickCheck evalTest3_6
        , putStrLn "evalTest3_7"
        , quickCheck evalTest3_7
        , putStrLn "evalTest3_8"
        , quickCheck evalTest3_8
        , putStrLn "evalTest4_1"
        , quickCheck evalTest4_1
        ]

--                          , putStrLn "evalTest2_5", quickCheck evalTest2_5
{-


\section{Eval Tests}

\begin{comment}
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

{-

Run Tests
-- runs all tests in test statement
-- \begin{code}
run_tests = do
            -- runSimpFixedPtTests test_ders
             runSimpFixedPtTests test_funcs
--\end{code}

Testing if simplify reaches fixed point
--\begin{code}
testSimplifyReachFixedPt n max exp = if n == max then error("Failed Test: Simplify reach fixed point - Reached max without reaching fixed point for:  " ++ show exp )
                            else
                                if ((exp) == ( simplify $ exp))
                                then trace ("-> " ++ show n) True
                                else testSimplifyReachFixedPt (n+1) max (simplify $ exp)

runSimpFixedPtTests []      = True
runSimpFixedPtTests (x:xs)  = (testSimplifyReachFixedPt 0 100 x) && (runSimpFixedPtTests xs)

--\end{code}

\begin{comment}
%\begin{code}

vm0_0 = ([("x",1),("y",2),("z",3)],[],[],[],[])
vm0_7 = ([("x",4),("y",4)],[],[],[],[])

evalTest0_0 = evalScalar x  (subs vm0_0) == 1.0
evalTest0_1 = evalScalar (-x) (subs vm0_0) == -1.0
evalTest0_2 = evalScalar (x+y) (subs vm0_0) == 3
evalTest0_3 = evalScalar (x+x+x) (subs vm0_0) == 3
evalTest0_4 = evalScalar (2*x) (subs vm0_0) == 2
evalTest0_5 = evalScalar (2*x) (subs vm0_0) == 2

evalTest0_1b = evalScalar (-x) (subs vm0_0) == 1.0 -- should fail

evalTest0_7 = evalScalar (xRe (x +: y)) (subs vm0_7) == 4
evalTest0_8 = evalScalar (xIm (x +: y)) (subs vm0_7) == 4

%\end{code}
\end{comment}
-}
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

{-

\begin{comment}
%\begin{code}
vm1_0 =  ([],[("X1",U.listArray (0,15) vx1)],[],[],[])

evalTest1_0 = evalOneD (simplify $ x1)    (subs vm1_0) == U.listArray (0,15) vx2
evalTest1_1 = evalOneD (simplify $ -x1)   (subs vm1_0) == U.listArray (0,15) (map (*(-1)) vx1)
evalTest1_2 = evalOneD (simplify $ x1+x1) (subs vm1_0) == U.listArray (0,15) (map (*2) vx1)
evalTest1_3 = evalOneD (simplify $ x1+x1+x1) (subs vm1_0) == U.listArray (0,15) (map (*3) vx1)
evalTest1_4 = evalOneD (simplify $ 2.0 `scale` x1) (subs vm1_0) == U.listArray (0,15) (map (*2) vx1)
evalTest1_5 = evalOneD (([(2,5)],[16]) `crop` x1) (subs vm1_0)  == U.listArray (0,3) [2..5]
evalTest1_6 = evalOneD (([(2,5)],[16]) `uncrop` x1) (subs vm1_0) == U.listArray (0,15) ([0,0,2,3,4,5] ++ (replicate 10 0))
evalTest1_7 = evalOneD (xRe (x1 +: x1)) (subs vm1_0)  == U.listArray (0,15) vx2
evalTest1_8 = evalOneD (xIm (x1 +: x1)) (subs vm1_0)  == U.listArray (0,15) vx2
evalTest1_9 = evalOneD (xRe (ft (x1 +: y1))) subs1
evalTest1_10 = evalOneD (xIm (ft (x1 +: y1))) subs1

%\end{code}
\end{comment}

-}
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

--evalTest1_9 = evalOneD (xRe (ft (x1 +: y1))) subs1
--evalTest1_10 = evalOneD (xIm (ft (x1 +: y1))) subs1
{-



-}
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
evalTest4_1 str = "0.0" == (show $ simplify $ 0 * (var $ "x" ++ str))-- FIXME broken as of 12May2015

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
display1d :: U.UArray Int Double -> IO ()
display1d a =
    let (_0, b) = bounds a
        l = [a ! i | i <- [0 .. b]]
     in putStrLn (concat $ List.intersperse "\t" $ map show l)

display2d :: U.UArray (Int, Int) Double -> IO ()
display2d a =
    let ((_0, _0'), (b1, b2)) = bounds a
        l = [[a ! (i, j) | j <- [0 .. b2]] | i <- [0 .. b1]]
     in putStrLn
            (concat $
             concat $
             List.intersperse (["\n"]) $
             map (List.intersperse "\t") $ map (map show) l)

display3d :: U.UArray (Int, Int, Int) Double -> IO ()
display3d a =
    let ((_0, _0', _0''), (b1, b2, b3)) = bounds a
        l =
            [ [[a ! (i, j, k) | k <- [0 .. b3]] | j <- [0 .. b2]]
            | i <- [0 .. b1]
            ]
     in putStrLn
            (concat $
             concat $
             List.intersperse (["\n\n"]) $
             concat $
             List.intersperse ([["\n"]]) $
             map (map (List.intersperse "\t")) $ map (map (map show)) l)

display3dc :: Array (Int, Int, Int) (Data.Complex.Complex Double) -> IO ()
display3dc a =
    let ((_0, _0', _0''), (b1, b2, b3)) = bounds a
        l =
            [ [[a ! (i, j, k) | k <- [0 .. b3]] | j <- [0 .. b2]]
            | i <- [0 .. b1]
            ]
     in putStrLn
            (concat $
             concat $
             List.intersperse (["\n\n"]) $
             concat $
             List.intersperse ([["\n"]]) $
             map (map (List.intersperse "\t")) $ map (map (map show)) l)

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
simpTest1 = errCmp "simpTest1" 0 $ simplify $ diff (mp ["w"]) (x * y)

simpTest2 =
    let e1 = simplify $ diff (mp ["w"]) (x * y + 2 * w)
        e2 = simplify $ diff (mp ["w"]) (w + w)
     in errCmp "simpTest2" e1 e2

{-


Utility functions, to print expressions in different forms in case of errors
-}
errCmp name e1 e2 =
    if e1 == e2
        then Nothing
        else Just $ name ++ " " ++ errExpr e1 ++ "<not equal>" ++ errExpr e2

{-

-}
class ErrExpr a where
    errExpr :: a -> String

instance ErrExpr Scalar where
    errExpr s@(Scalar e) = concat ["<<", show s, "|not equal|", show e, ">>"]

instance ErrExpr OneD where
    errExpr (OneD e) = concat ["<<", show e, "|", errExpr' e, ">>"]

instance ErrExpr TwoD where
    errExpr (TwoD e) = concat ["<<", show e, "|", errExpr' e, ">>"]

instance ErrExpr ThreeD where
    errExpr (ThreeD e) = concat ["<<", show e, "|", errExpr' e, ">>"]

errExpr' (Expression n es) = show (n, es)

{-
x
-}
regressionTestPartDiffZero =
    sequence
        [ putStrLn "partDiffTest0_0"
        , quickCheck partDiffTest0_0
        , putStrLn "partDiffTest0_1"
        , quickCheck partDiffTest0_1
        , putStrLn "partDiffTest0_2"
        , quickCheck partDiffTest0_2
        , putStrLn "partDiffTest0_3"
        , quickCheck partDiffTest0_3
        , putStrLn "partDiffTest0_4"
        , quickCheck partDiffTest0_4
        , putStrLn "partDiffTest0_5"
        , quickCheck partDiffTest0_5
        , putStrLn "partDiffTest0_6"
        , quickCheck partDiffTest0_6
        , putStrLn "partDiffTest0_7"
        , quickCheck partDiffTest0_7
        , putStrLn "partDiffTest0_8"
        , quickCheck partDiffTest0_8
        , putStrLn "partDiffTest0_9"
        , quickCheck partDiffTest0_9
        ]

regressionTestPartDiffOne =
    sequence
        [ putStrLn "partDiffTest1_2"
        , quickCheck partDiffTest1_2
        , putStrLn "partDiffTest1_3"
        , quickCheck partDiffTest1_3
        , putStrLn "partDiffTest1_4"
        , quickCheck partDiffTest1_4
        , putStrLn "partDiffTest1_5"
        , quickCheck partDiffTest1_5
        , putStrLn "partDiffTest1_6"
        , quickCheck partDiffTest1_6
        , putStrLn "partDiffTest1_7"
        , quickCheck partDiffTest1_7
        ]

regressionTestPartDiff =
    sequence [regressionTestPartDiffZero, regressionTestPartDiffOne]

{-

\section{Differentiation Tests}

We use simplify in these tests so that 1*(cos x) matches with (cos x)*1.  We assume that there are enough tests above for simplify that we can trust it.  We use (\^{}\^{}) for exponents because (\^{}) doesn't like negative exponents.

Simplifying the partDiff of $x^{-2}$ gives $\dfrac{-2x}{x^4}$, which is not wrong, but it isn't totally simplified, so instead we substitute values and check those.

Possibilities for a fix:
\begin{itemize}
\item Define an IntPow to replace \^{}, \^{}\^{} and \*{} \*{} from GHC which are defined as repeated multiplication or exp(log(base)*power) %That's ^, ^^, and ** if you're reading the textfile
\item Use a dangerous simplify to factor out the extras.
\end{itemize}
-}
-} -}
{-
Define a type where the Aribtrary instance is between -50 and 50 so that tests don't take so long or collapse or blow up.
-}
newtype FiftyRange =
    FiftyRange Int
    deriving (Show, Num, Ord, Eq, Integral, Real, Enum)

instance Arbitrary FiftyRange where
    arbitrary = FiftyRange `liftM` choose (-50, 50)

{-

Define a type where the Aribtrary instance is between -5 and 5 so that tests for hyperbolic trig functions don't collapse or blow up.
-}
newtype FiveRange =
    FiveRange Int
    deriving (Show, Num, Ord, Eq, Integral, Real, Enum)

instance Arbitrary FiveRange where
    arbitrary = FiveRange `liftM` choose (-5, 5)

{-

Define a type where the Aribtrary instance is between -1 and 1 so that tests work on inverse trig functions.  toDouble is necessary because ValMaps require doubles for substitution.
-}
newtype OneRange =
    OneRange Double
    deriving (Show, Ord, Num, Eq, Real, Fractional)

instance Arbitrary OneRange where
    arbitrary = OneRange `liftM` choose (-1, 1)

{-

Define a type where the Aribtrary instance is between -70 and 70 so that tests for hyperbolic trig functions don't collapse or blow up.
-}
newtype SeventyRange =
    SeventyRange Double
    deriving (Show, Ord, Num, Eq, Real, Fractional)

instance Arbitrary SeventyRange where
    arbitrary = SeventyRange `liftM` choose (-70, 70)

{-

-}
newtype TwoThouRange =
    TwoThouRange Double
    deriving (Show, Ord, Num, Eq, Real, Fractional)

instance Arbitrary TwoThouRange where
    arbitrary = TwoThouRange `liftM` choose (-2000, 2000)

{-


Functions to convert to a Double so that values can be used in ValMaps.
-}
oneDouble :: OneRange -> Double
oneDouble (OneRange n) = n

seventyDouble :: SeventyRange -> Double
seventyDouble (SeventyRange n) = n

twoThouDouble :: TwoThouRange -> Double
twoThouDouble (TwoThouRange n) = n

{-

When $a <= 0$, there are negative exponents, so when n == 0, it is undefined.  We mark these cases trivial and let them be true.  We also use special types to restrict the range to avoid overflow or underflow.  These tests need type declarations for quickCheck.  The second version of each test does not use quickCheck; it just returns a list containing the result of the  derivative and the expected answer.
$$\frac{d}{dx}bx^a = bax^{a-1}$$
-}
partDiffTest0_0 :: FiftyRange -> Double -> Double -> Property
partDiffTest0_0 a b n =
    classify
        (n == 0 && b <= 0)
        "trivial"
        (if (n == 0 && b <= 0)
             then True
             else abs (evalScalar
                           (simplify (partDiff (p "x") ((fromDbl b) * (x ^^ a))) -
                            (fromDbl b) * (fromIntegral a) * (x ^^ (a - 1)))
                           (subs ([("x", n)], [], [], [], []))) <=
                  0.00001 *
                  abs
                      (evalScalar
                           (simplify
                                ((fromDbl b) * (fromIntegral a) * (x ^^ (a - 1))))
                           (subs ([("x", n)], [], [], [], []))))

partDiffTest0_0_1 a b n =
    [ evalScalar
          (simplify (partDiff (p "x") ((fromDbl b) * (x ^^ a))))
          (subs ([("x", n)], [], [], [], []))
    , evalScalar
          (simplify ((fromDbl b) * (fromIntegral a) * (x ^^ (a - 1))))
          (subs ([("x", n)], [], [], [], []))
    ]

{-

$$\frac{d}{dx}sin^bx = bsin^{b-1}xcosx$$
-}
partDiffTest0_1 :: FiftyRange -> Double -> Property
partDiffTest0_1 b n =
    classify
        (n == 0 && b < 0)
        "trivial"
        (if (n == 0 && b < 0)
             then True
             else abs (evalScalar
                           ((simplify (partDiff (p "x") ((sin x) ^^ b))) -
                            (simplify
                                 ((cos x) * (fromIntegral b) *
                                  ((sin x) ^^ (b - 1)))))
                           (subs ([("x", n)], [], [], [], []))) <=
                  0.0001 *
                  abs
                      (evalScalar
                           (simplify
                                ((cos x) * (fromIntegral b) *
                                 ((sin x) ^^ (b - 1))))
                           (subs ([("x", n)], [], [], [], []))))

partDiffTest0_1_1 b n =
    [ evalScalar
          ((simplify (partDiff (p "x") ((sin x) ^^ b))))
          (subs ([("x", n)], [], [], [], []))
    , evalScalar
          (simplify ((cos x) * (fromIntegral b) * ((sin x) ^^ (b - 1))))
          (subs ([("x", n)], [], [], [], []))
    ]

{-

$$\frac{d}{dx}cos^bx = bcos^{b-1}xsinx$$
-}
partDiffTest0_2 :: FiftyRange -> Double -> Property
partDiffTest0_2 b n =
    classify
        (n == 0 && b < 0)
        "trivial"
        (if n == 0 && b < 0
             then True
             else abs (evalScalar
                           (simplify (partDiff (p "x") ((cos x) ^^ b)) -
                            (negate (fromIntegral b) * (sin x) *
                             ((cos x) ^^ (b - 1))))
                           (subs ([("x", n)], [], [], [], []))) <=
                  0.0001 *
                  abs
                      (evalScalar
                           (simplify
                                (negate (fromIntegral b) * (sin x) *
                                 ((cos x) ^^ (b - 1))))
                           (subs ([("x", n)], [], [], [], []))))

partDiffTest0_2_1 b n =
    [ evalScalar
          ((simplify (partDiff (p "x") ((cos x) ^^ b))))
          (subs ([("x", n)], [], [], [], []))
    , evalScalar
          (simplify (negate (fromIntegral b) * (sin x) * ((cos x) ^^ (b - 1))))
          (subs ([("x", n)], [], [], [], []))
    ]

{-

$$\frac{d}{dx}tan^bx = \frac{btan^{b-1}(x)}{cos^2x}$$
-}
partDiffTest0_3 :: FiftyRange -> Double -> Property
partDiffTest0_3 b n =
    classify
        (n == 0 && b <= 0)
        "trivial"
        (if (n == 0 && b <= 0)
             then True
             else abs (evalScalar
                           (simplify (partDiff (p "x") ((tan x) ^^ b)) -
                            (fromIntegral b) * recip ((cos x) ^ 2) *
                            ((tan x) ^^ (b - 1)))
                           (subs ([("x", n)], [], [], [], []))) <=
                  0.0001 *
                  abs
                      (evalScalar
                           (simplify (fromIntegral b) * recip ((cos x) ^ 2) *
                            ((tan x) ^^ (b - 1)))
                           (subs ([("x", n)], [], [], [], []))))

{-

$$\frac{d}{dx}asin^bx = \frac{b*asin^{b-1}x}{\sqrt{1-x^2}}$$
Arcsine only works on inputs between -1 and 1; use OneRange
-}
partDiffTest0_4 :: FiftyRange -> OneRange -> Property
partDiffTest0_4 b n =
    classify
        (n == 0 && b <= 0)
        "trivial"
        (if n == 0 && b <= 0
             then True
             else abs (evalScalar
                           (simplify (partDiff (p "x") ((asin x) ^^ b)) -
                            (recip (sqrt (1 - x ^ 2)) * (asin x) ^^ (b - 1) *
                             (fromIntegral b)))
                           (subs ([("x", oneDouble n)], [], [], [], []))) <=
                  0.0001 *
                  abs
                      (evalScalar
                           (simplify
                                (recip (sqrt (1 - x ^ 2)) * (asin x) ^^ (b - 1) *
                                 (fromIntegral b)))
                           (subs ([("x", oneDouble n)], [], [], [], []))))

{-

$$\frac{d}{dx}acos^bx = \frac{-b*acos^{b-1}x}{\sqrt{1-x^2}}$$
-} -}
partDiffTest0_5 :: FiftyRange -> OneRange -> Property
partDiffTest0_5 b n =
    classify
        (n == 0 && b <= 0)
        "trivial"
        (if n == 0 && b <= 0
             then True
             else abs (evalScalar
                           (simplify (partDiff (p "x") ((acos x) ^^ b)) -
                            (-recip (sqrt (1 - x ^ 2)) * (acos x) ^^ (b - 1) *
                              (fromIntegral b)))
                           (subs ([("x", oneDouble n)], [], [], [], []))) <=
                  0.0001 *
                  abs
                      (evalScalar
                           (simplify
                                (-recip (sqrt (1 - x ^ 2)) * (acos x) ^^ (b - 1) *
                                  (fromIntegral b)))
                           (subs ([("x", oneDouble n)], [], [], [], []))))

{-

$$\frac{d}{dx}atan^bx = \frac{b*atan^{b-1}x}{1+x^2}$$
-}
partDiffTest0_6 :: FiftyRange -> Double -> Property
partDiffTest0_6 b n =
    classify
        (n == 0 && b <= 0)
        "trivial"
        (if (n == 0 && b <= 0)
             then True
             else abs (evalScalar
                           (simplify (partDiff (p "x") ((atan x) ^^ b)) -
                            (recip (1 + x ^ 2) * (atan x) ^^ (b - 1) *
                             (fromIntegral b)))
                           (subs ([("x", n)], [], [], [], []))) <=
                  0.0001 *
                  abs
                      (evalScalar
                           (simplify
                                (recip (1 + x ^ 2) * (atan x) ^^ (b - 1) *
                                 (fromIntegral b)))
                           (subs ([("x", n)], [], [], [], []))))

{-

$$\frac{d}{dx}sinh^bx = bsinh^{b-1}xcoshx$$
sinh and cosh overflow after 710 and -710; things get worse when taking powers of sinh and cosh, so SeventyRange and FiveRange; the problem is even worse because negative exponents use the quotient rule
-}
partDiffTest0_7 :: FiveRange -> SeventyRange -> Property
partDiffTest0_7 b n =
    classify
        (n == 0 && b <= 0)
        "trivial"
        (if (n == 0 && b < 0)
             then True
             else abs (evalScalar
                           (simplify
                                (partDiff (p "x") ((sinh x) ^^ b) -
                                 (sinh x) ^^ (b - 1) * (fromIntegral b) *
                                 (cosh x)))
                           (subs ([("x", seventyDouble n)], [], [], [], []))) <=
                  0.0001 *
                  abs
                      (evalScalar
                           (simplify
                                ((sinh x) ^^ (b - 1) * (fromIntegral b) *
                                 (cosh x)))
                           (subs ([("x", seventyDouble n)], [], [], [], []))))

partDiffTest0_7_1 b n =
    [ evalScalar
          (simplify (partDiff (p "x") ((sinh x) ^^ b)))
          (subs ([("x", seventyDouble n)], [], [], [], []))
    , evalScalar
          (simplify ((sinh x) ^^ (b - 1) * (fromIntegral b) * (cosh x)))
          (subs ([("x", seventyDouble n)], [], [], [], []))
    ]

{-

$$\frac{d}{dx}cosh^bx = bcosh^{b-1}xsinhx$$
I'm not sure why the type needs to be a Bool instead of a Property, but it works.
-}
partDiffTest0_8 :: FiveRange -> SeventyRange -> Bool
partDiffTest0_8 b n =
    (evalScalar
         (simplify
              (partDiff (p "x") ((cosh x) ^^ b) -
               (cosh x) ^^ (b - 1) * (fromIntegral b) * (sinh x)))
         (subs ([("x", seventyDouble n)], [], [], [], []))) <=
    0.0001 *
    abs (evalScalar
             (simplify ((cosh x) ^^ (b - 1) * (fromIntegral b) * (sinh x)))
             (subs ([("x", seventyDouble n)], [], [], [], [])))

partDiffTest0_8_1 b n =
    [ evalScalar
          (simplify (partDiff (p "x") ((cosh x) ^^ b)))
          (subs ([("x", n)], [], [], [], []))
    , evalScalar
          (simplify ((cosh x) ^^ (b - 1) * (fromIntegral b) * (sinh x)))
          (subs ([("x", n)], [], [], [], []))
    ]

{-

$$\frac{d}{dx}tanh^bx = \frac{btanh^bx}{cosh^2x}$$
-}
partDiffTest0_9 :: FiveRange -> Double -> Property
partDiffTest0_9 b n =
    classify
        (n < -325 || n > 325 || (n == 0 && b <= 0))
        "trivial"
        (if (n < -325 || n > 325 || (n == 0 && b <= 0))
             then True
             else abs (evalScalar
                           (simplify (partDiff (p "x") ((tanh x) ^^ b)))
                           (subs ([("x", n)], [], [], [], [])) -
                       evalScalar
                           (simplify
                                ((tanh x) ^^ (b - 1) * (fromIntegral b) /
                                 (cosh x) ^ 2))
                           (subs ([("x", n)], [], [], [], []))) <=
                  0.0001 *
                  abs
                      (evalScalar
                           (simplify
                                ((tanh x) ^^ (b - 1) * (fromIntegral b) /
                                 (cosh x) ^ 2))
                           (subs ([("x", n)], [], [], [], []))))

partDiffTest0_9_1 b n =
    [ evalScalar
          (simplify (partDiff (p "x") ((tanh x) ^^ b)))
          (subs ([("x", n)], [], [], [], []))
    , evalScalar
          (simplify ((tanh x) ^^ (b - 1) * (fromIntegral b) / (cosh x) ^ 2))
          (subs ([("x", n)], [], [], [], []))
    ]

{-

$$\frac{d}{dx}\frac{log(x)(x^4-1)^3}{3x^2} = \frac{(x^4-1)^2(x^4+(10x^4+2)log(x)-1)}{3x^3}$$
-}
partDiffTest1_2 (Positive n) =
    abs ((evalScalar
              (simplify
                   (partDiff
                        (p "x")
                        (((log x) * (((x ^ 4) - 1) ^ 3)) / (3 * (x ^ 2)))))
              (subs ([("x", n)], [], [], [], []))) -
         (evalScalar
              (simplify
                   (((((x ^ 4) - 1) ^ 2) *
                     ((x ^ 4) + ((10 * (x ^ 4) + 2) * (log x)) - 1)) /
                    (3 * (x ^ 3))))
              (subs ([("x", n)], [], [], [], [])))) <=
    0.001 *
    ((abs n) +
     (abs (evalScalar
               (simplify
                    (partDiff
                         (p "x")
                         (((log x) * (((x ^ 4) - 1) ^ 3)) / (3 * (x ^ 2)))))
               (subs ([("x", n)], [], [], [], [])))) +
     1)

partDiffTest1_2_1 n =
    [ (evalScalar
           (simplify
                (partDiff
                     (p "x")
                     (((log x) * (((x ^ 4) - 1) ^ 3)) / (3 * (x ^ 2)))))
           (subs ([("x", n)], [], [], [], [])))
    , (evalScalar
           (simplify
                (((((x ^ 4) - 1) ^ 2) *
                  ((x ^ 4) + ((10 * (x ^ 4) + 2) * (log x)) - 1)) /
                 (3 * (x ^ 3))))
           (subs ([("x", n)], [], [], [], [])))
    ]

{-
$$\frac{d}{dx}\frac{x^4+x^3}{x^2-x} = \frac{2x(x^2-x-1)}{(x-1)^2}$$
-}
partDiffTest1_3 n
    | n == 0 = True
    | n == 1 = True
    | otherwise =
        abs
            ((evalScalar
                  (simplify
                       (partDiff (p "x") (((x ^ 4) + (x ^ 3)) / ((x ^ 2) - x))))
                  (subs ([("x", n)], [], [], [], []))) -
             (evalScalar
                  (simplify ((2 * x * ((x ^ 2) - x - 1)) / ((x - 1) ^ 2)))
                  (subs ([("x", n)], [], [], [], [])))) <=
        0.01 *
        ((abs n) +
         (abs (evalScalar
                   (simplify
                        (partDiff (p "x") (((x ^ 4) + (x ^ 3)) / ((x ^ 2) - x))))
                   (subs ([("x", n)], [], [], [], [])))) +
         1)

partDiffTest1_3_1 n =
    [ (evalScalar
           (simplify (partDiff (p "x") (((x ^ 4) + (x ^ 3)) / ((x ^ 2) - x))))
           (subs ([("x", n)], [], [], [], [])))
    , (evalScalar
           (simplify ((2 * x * ((x ^ 2) - x - 1)) / ((x - 1) ^ 2)))
           (subs ([("x", n)], [], [], [], [])))
    ]

{-
$$\frac{d}{dx}\frac{x^3}{x^2-x} = \frac{x^2-2x}{(x-1)^2}$$
-}
partDiffTest1_4 (NonZero n) =
    abs ((evalScalar
              (simplify (partDiff (p "x") (((x ^ 3)) / ((x ^ 2) - x))))
              (subs ([("x", n)], [], [], [], []))) -
         (evalScalar
              (simplify ((x ^ 2 - 2 * x) / ((x - 1) ^ 2)))
              (subs ([("x", n)], [], [], [], [])))) <=
    0.001 *
    ((abs n) +
     (abs (evalScalar
               (simplify (partDiff (p "x") (((x ^ 3)) / ((x ^ 2) - x))))
               (subs ([("x", n)], [], [], [], [])))) +
     1)

{-
$$\frac{d}{dx}\frac{sin^4x-cos^3x}{3x^2} = \frac{(cos(x)-sin^4x)^2(-2sin^4x+3xsin(x)+2cos(x)(6xsin^3x+1))}{3x^3}$$
-}
partDiffTest1_5 (NonZero n) =
    abs ((evalScalar
              (simplify
                   (partDiff
                        (p "x")
                        ((((((sin x) ^ 4) - (cos x)) ^ 3)) / (3 * (x ^ 2)))))
              (subs ([("x", n)], [], [], [], []))) -
         (evalScalar
              (simplify
                   (((((cos x) - ((sin x) ^ 4)) ^ 2) *
                     ((-2 * (sin x) ^ 4) + (3 * x * (sin x)) +
                      (2 * (cos x) * ((6 * x * ((sin x) ^ 3)) + 1)))) /
                    (3 * (x ^ 3))))
              (subs ([("x", n)], [], [], [], [])))) <=
    0.001 *
    ((abs n) +
     (abs (evalScalar
               (simplify
                    (partDiff
                         (p "x")
                         ((((((sin x) ^ 4) - (cos x)) ^ 3)) / (3 * (x ^ 2)))))
               (subs ([("x", n)], [], [], [], [])))) +
     1)

{-

$$\frac{d}{dx}\frac{log(x)sin^4x}{3x^2} = \frac{sin^3x(sin(x)-2log(x)sin(x)+4xlog(x)cos(x))}{3x^3}$$
-}
partDiffTest1_6 (Positive n) =
    abs ((evalScalar
              (simplify
                   (partDiff (p "x") (((log x) * ((sin x) ^ 4)) / (3 * (x ^ 2)))))
              (subs ([("x", n)], [], [], [], []))) -
         (evalScalar
              (simplify
                   ((((sin x) ^ 3) *
                     ((sin x) - (2 * (log x) * (sin x)) +
                      (4 * x * (log x) * (cos x)))) /
                    (3 * (x ^ 3))))
              (subs ([("x", n)], [], [], [], [])))) <=
    0.001 *
    ((abs n) +
     (abs (evalScalar
               (simplify
                    (partDiff
                         (p "x")
                         (((log x) * ((sin x) ^ 4)) / (3 * (x ^ 2)))))
               (subs ([("x", n)], [], [], [], [])))) +
     1)

partDiffTest1_6_1 n =
    [ (evalScalar
           (simplify
                (partDiff (p "x") (((log x) * ((sin x) ^ 4)) / (3 * (x ^ 2)))))
           (subs ([("x", n)], [], [], [], [])))
    , (evalScalar
           (simplify
                ((((sin x) ^ 3) *
                  ((sin x) - (2 * (log x) * (sin x)) +
                   (4 * x * (log x) * (cos x)))) /
                 (3 * (x ^ 3))))
           (subs ([("x", n)], [], [], [], [])))
    ]

{-

$$\frac{d}{dx}\frac{e^{4x(x+3)}}{log(4x^4)} = \frac{4e^{4x(x+3)}(x(2x+3)log(4x^4)-1)}{(log(4x^4))^2} $$

There used to be a condition on this test saying that if the result was NaN, then it was true.  This happens when n is greater than 11.8156233442122, which is approx. 80\% of the time.
-}
partDiffTest1_7 (Positive n) =
    classify
        (n >= 11.8156233442122)
        "trivial"
        (if n >= 11.8156233442122
             then True
             else abs ((evalScalar
                            (simplify
                                 (partDiff
                                      (p "x")
                                      ((exp (4 * x * (x + 3))) /
                                       (log (4 * (x ^ 4))))))
                            (subs ([("x", n)], [], [], [], []))) -
                       (evalScalar
                            (simplify
                                 (((4 * exp (4 * x * (x + 3))) *
                                   ((x * ((2 * x) + 3) * (log (4 * (x ^ 4)))) -
                                    1)) /
                                  (x * ((log (4 * (x ^ 4)) ^ 2)))))
                            (subs ([("x", n)], [], [], [], [])))) <=
                  0.001 *
                  ((abs n) +
                   (abs (evalScalar
                             (simplify
                                  (partDiff
                                       (p "x")
                                       ((exp (4 * x * (x + 3))) /
                                        (log (4 * (x ^ 4))))))
                             (subs ([("x", n)], [], [], [], [])))) +
                   1))

partDiffTest1_7_1 n =
    [ (evalScalar
           (simplify
                (partDiff
                     (p "x")
                     ((exp (4 * x * (x + 3))) / (log (4 * (x ^ 4))))))
           (subs ([("x", n)], [], [], [], [])))
    , (evalScalar
           (simplify
                (((4 * exp (4 * x * (x + 3))) *
                  ((x * ((2 * x) + 3) * (log (4 * (x ^ 4)))) - 1)) /
                 (x * ((log (4 * (x ^ 4)) ^ 2)))))
           (subs ([("x", n)], [], [], [], [])))
    ]

{-

%funGrad (x1<.>x1) [p "X1"]
Declare a dvar.
-}
dx1 = dvar1d 4 "X1"

{-

$$\nabla x1\bullet x1 = dx1 \bullet 2x1$$
-}
gradDiffTest0_0 = simplify (diff [p "X1"] (x1 <.> x1 + x)) == dx1 <.> x1 * 2

{-

$$\nabla sin(x1\bullet x1) = cos(x1\bullet x1)dx1 \bullet 2x1$$
-}
gradDiffTest0_1 =
    simplify (diff [p "X1"] (sin (x1 <.> x1))) ==
    simplify (cos (x1 <.> x1) * dx1 <.> x1 * 2)

{-

$$\nabla (x1\bullet x1+y1\bullet y1) = dx1 \bullet 2x1$$
-}
gradDiffTest0_2 =
    simplify (diff [p "X1"] (x1 <.> x1 + y1 <.> y1)) == dx1 <.> x1 * 2

{-


-}
gradDiffTest0_3 =
    simplify (diff [p "X1"] (x1 <.> x1 * sin (x1 <.> x1))) ==
    simplify
        (sin (x1 <.> x1) * dx1 <.> x1 * 2 +
         cos (x1 <.> x1) * x1 <.> x1 * dx1 <.> x1 * 2)

gradDiffTest0_4 = simplify (diff [p "X1"] x) == 0

gradDiffTest0_5 = simplify (diff [p "X1"] (y1 <.> y1)) == 0

gradDiffTest0_6 =
    simplify (diff [p "X1"] ((sin (x1 <.> x1)) * (cos (x1 <.> x1))))

{-

\section{Topological Sort}

This is from HashedExamples, but HashedExamples isn't compiling, so we'll just put it here for now.

Things which are fixed:
-}
scalarTopSort (Scalar (Expression n e)) =
    let ts = topSort e n
     in putStrLn $
        unlines $
        (show e) :
        (show ts) : (map (\n -> (show n) ++ " : " ++ pretty' (e, n)) ts)

threeDCTopSort (ThreeDC (Expression n e)) =
    let ts = topSort e n
     in putStrLn $
        unlines $
        (show e) :
        (show ts) : (map (\n -> (show n) ++ " : " ++ pretty' (e, n)) ts)
{-
-}
