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
    sequence [regressionTestPartDiff]


--                          , putStrLn "evalTest2_5", quickCheck evalTest2_5
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
