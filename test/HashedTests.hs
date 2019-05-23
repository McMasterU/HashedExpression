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
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HashedTests where

import           HashedDerivative
import           HashedExpression
import           HashedInstances
import           HashedInterp

-- import HashedComplexInstances
import           HashedSimplify

-- import HashedMRI --for the R2Star tests
import           HashedConvZip
import           HashedDot
import qualified Polynomials           as P

-- import R2Star
--import Maybe (isJust)
--import Data.IntMap (IntMap)
import           Control.Monad
import           Data.Array.Unboxed    as U
import qualified Data.ByteString.Char8 as C
import           Data.Complex
import qualified Data.IntMap           as I
import qualified Data.List             as List
import qualified Data.Map              as Map
import qualified Data.Map.Strict       as M
import           Data.Maybe
import           Test.QuickCheck       hiding (scale)

--import System.IO.Unsafe
--import HashedExamples
import           Debug.Trace

{-
\end{comment}

\section{Simplify Tests}
Expressions can simplify in:
\begin{enumerate}
\item the instance of an expression in HashedInstances.lhs
\item a matching rule in simp1 in HashedSimplify.lhs
\item a matching rule below simp1 in HashedSimplify.lhs
\end{enumerate}

So far, only a few simplify rules have been implemented in their instances.  Most rules are in simp1 (WithHoles), but some more complicated cases do not work using only these rules, so there are the original, more complicated rules below simp1.

Some tests are commented out because otherwise they will not compile.

\begin{description}
\item[simpTest0] Scalar multiplication, with simplifications for products containing zero or one
\item[simpTest1] Scalar addition, with simplifications for sums containing zero
\item[simpTest2] Scalar addition and multiplication together, with simplifications for repeated values
\item[simpTest3] Exponents and logarithms
\item[simpTest4] Division, rule for zero divided by non-zero, which is not implemented yet
\item[simpTest5] One, two and three dimensional vectors, with simplifications for addition, dot products and scaling
\item[simpTest6] Distribution for scalars and vectors
\item[simpTest7] Complex scalars and vectors, with simplifications for extracting real and imaginary and parts and addition
\item[simpTest8] Complex scalars and vectors, with simplifications for scalar  multiplication and dot products
\item[simpTest9] Scaling complex scalars and vectors
\item[simpTest10] Projections, injections and Fourier transforms
\end{description}

\subsection{simpTest0}Multiplication

Multiplication with (*)
\begin{itemize}
\item defined in GHC.Num
\item precedence 7
\item left associative
\item takes two instances of the Num class
\end{itemize}

Multiplication with product
\begin{itemize}
\item defined in Data.List
\item takes a list of instances of the Num class
\end{itemize}

Begin by defining some variables of type Scalar.
-}
[x, y, z, u, v, w] = map var ["x", "y", "z", "u", "v", "w"]

{-

$x*1$ and $1*x$ do not simplify in the instance, but will simplify using either simp1 or collapsing of a product containing a one.
-}
simpTest0_0 = simplify (x * 1) == x

simpTest0_1 = simplify (1 * x) == x

{-

$x*0$ and $0*x$ follow the same pattern as $x*1$ and $1*x$.
-}
simpTest0_2 = simplify (x * 0) == 0

simpTest0_3 = simplify (0 * x) == 0

{-

These tests show how the instances of (*) and product work.
-}
simpTest0_4 = product [x, y, z] /= (x * y * z)

simpTest0_5 = product [x, y, z] == (1 * x * y * z)

simpTest0_6 = pretty (unScalar (product [x, y, z])) == "(((1.0*x)*y)*z)"

simpTest0_7 = pretty (unScalar (x * y * z)) == "((x*y)*z)"

{-

Now looking at some simplifications.
-}
simpTest0_8 = pretty (unScalar (simplify ((x * y) * z))) == "(x*y*z)"

simpTest0_9 = pretty (unScalar (simplify (product [x, y, z]))) == "(x*y*z)"

{-

Use applyOne here.
-}
simpTest0_10 = simplify ((x * 0) * y) == 0

simpTest0_11 = simplify (y * (x * 0)) == 0

simpTest0_12 = simplify (0 * (x * 1)) == 0

simpTest0_13 = simplify (0 * x * 1) == 0

simpTest0_14 = simplify (0 * (x * y)) == 0

simpTest0_15 = simplify ((x * y) * 0) == 0

simpTest0_16 = simplify ((x * 0) * 1) == 0

simpTest0_17 = simplify ((x * y) * 1) == (x * y)

simpTest0_18 = simplify (x * y * z * 1) == simplify (x * y * z)

{-

The simp1 rules only match inside $x*y$. For $x*y*z$, there are other rules for removing a one from a product and collapsing a product containing a zero.  When the rule for $(x*y)*z = x*y*z$ is not used, the simp1 rules can simplify larger products, because they are really nested smaller products, not larger products.  (i.e. $x*y*z$ makes an instance of $(x*y)*z$)

\subsection{simpTest1} Addition

Addition follows the same pattern as multiplication.

Addition with (+)
\begin{itemize}
\item defined in GHC.Num
\item precedence 6
\item left associative
\item takes two instances of the Num class
\end{itemize}

Addition with sum
\begin{itemize}
\item defined in Data.List
\item takes a list of instances of the Num class
\end{itemize}

-}
simpTest1_0 = simplify (x + 0) == x

simpTest1_1 = simplify (0 + x) == x

{-

Here are some examples showing what the instance of (+) and sum do.
-}
simpTest1_2 = sum [x, y, z] /= (x + y + z)

simpTest1_3 = sum [x, y, z] == (0 + x + y + z)

simpTest1_4 = pretty (unScalar (sum [x, y, z])) == "(((0.0+x)+y)+z)"

{-

The instance of sum means that there will not be a sum containing only one term.
-}
simpTest1_5 = (sum [x]) == 0 + x

simpTest1_6 = simplify (sum [x]) == x

{-

GHC.Num will do some calculations, but not all
-}
simpTest1_7 = sum [2, 3, 4] == 9

simpTest1_8 = sum [2, 3, x] == (0 + 2 + 3 + x)

{-

As expected, the rule for collapsing a sum within a sum this first example will still work.  The second, however, does not.
-}
simpTest1_9 = simplify (sum [2, 3, x]) == x + 5

simpTest1_9_1 = [simplify (sum [2, 3, x]), x + 5]

simpTest1_10 = simplify (sum [2, x, 3]) == x + 5

{-

\subsection{simpTest2}Multiplication and Addition

I tried making rules for these simplifications in simp1, but it didn't work. $x+x$ became $2*x$, but $x+x+x$ became $4*x$
-}
simpTest2_0 = simplify (x + x) == x * 2

simpTest2_1 = simplify (x + x + x + x) == x * 4

simpTest2_2 = simplify (x + x - x + x + x) == x * 3

simpTest2_2_1 = [simplify (x + x - x + x + x), x * 3]

simpTest2_3 = simplify (4 * x + 2 * x) == x * 6

simpTest2_4 = simplify (4 * x - 2 * x) == x * 2

simpTest2_5 = simplify ((x * y) + 0) == (x * y)

simpTest2_6 = simplify (x * y + 0) == (x * y)

simpTest2_7 = simplify (x * 0 + z) == z

simpTest2_8 = simplify (x + 0 * y) == x

{-

\subsection{simpTest3} Exponentiation and logarithms

$e^x$ with exp
\begin{itemize}
\item defined in GHC.Float
\item takes an instance of the Fractional class
\end{itemize}

$e^0$ evaluates right away to 1.
-}
simpTest3_0 = exp 0 == 1

{-

However, there are cases when a rule for $e^0$ is necessary.  This rule is only in simp1.
-}
simpTest3_1 = simplify (exp (x - x)) == 1

{-

Passing the one to the simplify function causes an error because there is no instance of simplify for it.
-}
--simpTest3_2 = simplify(exp 0) == 1
{-

This won't run, but it will compile.
-}
simpTest3_3 = simplify (1) -- == 1

{-
This won't even compile.
-}
--simpTest3_4 = simplify(1::Double)
{-
But these work.  Simplify needs something in the Transformable class and a literal is too ambiguous for that.
-}
simpTest3_5 = simplify (1 :: Scalar) == 1

simpTest3_6 = simplify (1 :: ScalarC) == 1 +: 0

{-

Let's try this.
-}
simpTest3_7 = simplify (x - x) == 0.0

--simpTest3_8 = simplify (0.0)
simpTest3_9 = simplify (simplify (x - x)) == 0.0

{-

FIXME add the rule for log(exp x) but for exp(log x) we need undefined and piecewise functions
-}
simpTest3_10 = simplify (log (exp x)) == x

simpTest3_11 = simplify (exp (log x)) == x

{-

The instance of  is as multiplication.  There are no rules for simplifying a product containing repeated values.
Exponents with \^{}
\begin{itemize}
\item Instance as repeated multiplication
\item Defined in GHC.Real
\item Right associative
\item precedence 8
\item takes an instance of the Num class and the Integral class
\item does not take negative exponents
\item only takes integer exponents
\end{itemize}

Exponents with ^^
\begin{itemize}
\item Instance as repeated multiplication or division
\item Defined in GHC.Real
\item Right associative
\item precedence 8
\item takes an instance of the Fractional class and the Integral class
\item only takes integer exponents
\end{itemize}

Exponents with **
\begin{itemize}
\item Instance as exp(log(base)*power)
\item Defiend in GHC.Float
\item \item Right associative
\item precedence 8
\item takes two instances of the Floating class
\end{itemize}

-}
simpTest3_12 = x ^ 2 == x * x

{-

The Scalar x cannot be interpreted as being in the Integral class, so these raise type errors.
-}
--simpTest3_13 = x^(x-x)
--simpTest3_14 = x^^(x-x)
{-

This does work.
-}
simpTest3_15 = simplify (x ** (x - x)) == 1

{-

It looks like there's no support for vector exponents, which is fine, we don't really ever multiply vectors anyways.  The first example raises an exception: can't multiply 1d vectors
-}
simpTest3_16 = x1 ^ 2

simpTest3_17 = x2 ^ 2

simpTest3_18 = x3 ^ 2

{-

And these won't compile.
-}
--simpTest3_19 = x1^^2
--simpTest3_20 = x2^^2
--simpTest3_21 = x3^^2
--simpTest3_22 = x1**2
--simpTest3_23 = x2**2
--simpTest3_24 = x3**2
{-

\subsection{simpTest4} Division

A rule for simplifying $\frac{0}{x}$ could be dangerous if x is zero.  The rule is currently in simp1 (18/06/2015), but commented out until we can prove that x is not zero.
-}
simpTest4_0 = simplify (0 / x) == 0

{-

This, for example, should be safe to simplify.
-}
simpTest4_1 = simplify (0 / (x ^ 2 + 1)) == 0

{-

Another random thing involving division; it associates to the left and has the same precedence as multiplication, which is no surprise to anyone.
-}
simpTest4_2 = simplify (x / y * 0) == 0

{-

\subsection{simpTest5} One, Two and Three dimensional vectors

Begin by defining variables of type OneD, TwoD and ThreeD, and the corresponding zero vectors

-}
[x1, y1, z1, u1, v1, w1] = map (var1d 4) ["X1", "Y1", "Z1", "U1", "V1", "W1"]

[x2, y2, z2, u2, v2, w2] =
    map (var2d (4, 4)) ["X2", "Y2", "Z2", "U2", "V2", "W2"]

[x3, y3, z3, u3, v3, w3] =
    map (var3d (4, 4, 4)) ["X3", "Y3", "Z3", "U3", "V3", "W3"]

zero1 = simplify $ x1 - x1

zero2 = simplify $ x2 - x2

zero3 = simplify $ x3 - x3

{-

And a test to check the zero vector.
-}
simpTest5_0 = show (simplify $ x1 - x1) == "0.0(4)"

{-

Adding the zero vectors follows the same pattern as adding the zero scalar.  It uses the same simplify rules.
-}
simpTest5_1 = simplify (x1 + zero1) == x1

{-

Dot product using (<.>)
\begin{itemize}
\item infix for dot
\item Mac Option 8; Windows Alt 7; Linux Ctrl Shift u 2022
\item defined in HashedExpression.lhs
\item precedence 8
\item left associative
\item takes two instances of the Rectangular Class
\item defined in HashedExpression.lhs
\end{itemize}

Dot products with the zero vector simplify in simp1.
-}
simpTest5_2 = simplify (zero1 <.> x1) == 0

simpTest5_3 = simplify (x1 <.> zero1) == 0

simpTest5_4 = simplify (x2 <.> zero2) == 0

simpTest5_5 = simplify (zero2 <.> x2) == 0

simpTest5_6 = simplify (x3 <.> zero3) == 0

simpTest5_7 = simplify (zero3 <.> x3) == 0

{-

Scaling a vector using (*.)
\begin{itemize}
\item infix for scale
\item precedence 8
\item left associative
\item takes an instance of the Scalar class and the Rectangular class
\item defined in HashedExpression.lhs
\end{itemize}

These tests simplify with simp1
-}
simpTest5_8 = simplify (1 *. x1) == x1

simpTest5_9 = simplify (0 *. x1) == zero1

simpTest5_10 = simplify (x *. zero1) == zero1

{-

Addition and scaling together.  The same patterns applies as for scalar addition and multiplication.
-}
simpTest5_11 = simplify (x1 + x1 - x1 + x1 + x1) == 3 *. x1

simpTest5_12 =
    show (simplify $ x1 + 7 *. x1) ==
    ("(" ++ show (7 + 1 :: Double) ++ "*.X1(4))")

simpTest5_13 = simplify (x2 - 2 *. x2) == simplify (-1 *. x2)

{-

Scaling and dot product together simplify with simp1.
-}
simpTest5_14 = simplify ((x *. x1) <.> y1) == (x1 <.> y1) * x

simpTest5_15 = simplify (y1 <.> (x *. x1)) == (x1 <.> y1) * x

{-

Dot product and scaling both have precedence 8 and are left associative, so they can sometimes mix without parentheses.
-}
--simpTest5_16 = simplify(y1 <.> x*.x1) == (x1 <.> y1)*x
simpTest5_17 = simplify (x *. x1 <.> y1) == (x1 <.> y1) * x

simpTest5_18 = simplify (x *. (y *. z1)) == ((x * y) *. z1)

simpTest5_19 = simplify (x *. (y *. (z *. z1))) == simplify ((x * y * z) *. z1)

{-

Putting different things together, it works.
-}
simpTest5_20 = simplify ((x1 <.> zero1) * x) == 0

simpTest5_21 = simplify ((zero1 <.> x1) * x) == 0

simpTest5_22 = simplify (x * (x1 <.> zero1)) == 0

simpTest5_23 = simplify (x * (zero1 <.> x1)) == 0

simpTest5_24 = simplify ((x1 <.> y1) * 1) == (x1 <.> y1)

simpTest5_25 = simplify (x1 <.> y1 * 1) == (x1 <.> y1)

simpTest5_26 = simplify (1 * x1 <.> y1) == (x1 <.> y1)

simpTest5_27 = simplify ((x1 <.> y1) + 0) == (x1 <.> y1)

simpTest5_28 = simplify (x1 <.> y1 + 0) == (x1 <.> y1)

simpTest5_29 = simplify (0 + x1 <.> y1) == (x1 <.> y1)

{-

\subsection{simpTest6} Distribution

Nothing simplifies in the instances, just using simp1
-}
simpTest6_0 = simplify (x * (y + z)) == (x * y + x * z)

simpTest6_1 =
    pretty (unScalar (simplify (x * (y + z + w)))) == "((w*x)+(x*y)+(x*z))"

simpTest6_2 =
    pretty (unScalar (simplify ((y + z + w) * x))) == "((w*x)+(x*y)+(x*z))"

{-

The instance of subtraction is addition of the negative.  This is defined in GHC.Num.  These still simplify only using simp1.
-}
simpTest6_3 = pretty (unScalar (x * (y + z - w))) == "(x*((y+z)+((-1.0)*w)))"

simpTest6_3_1 = [pretty (unScalar (x * (y + z - w))), "(x*((y+z)+((-1.0)*w)))"]

simpTest6_4 =
    pretty (unScalar (simplify (x * (y + z - w)))) ==
    "((w*x*(-1.0))+(x*y)+(x*z))"

simpTest6_4_1 =
    [ pretty (unScalar (simplify (x * (y + z - w))))
    , "((w*x*(-1.0))+(x*y)+(x*z))"
    ]

simpTest6_5 =
    pretty (unScalar (simplify ((y + z - w) * x))) ==
    "((w*x*(-1.0))+(x*y)+(x*z))"

simpTest6_5_1 =
    [ pretty (unScalar (simplify ((y + z - w) * x)))
    , "((w*x*(-1.0))+(x*y)+(x*z))"
    ]

{-

This still uses simp1
-}
simpTest6_6 =
    pretty (unScalar (simplify ((x + y) * (w + z)))) ==
    "((w*x)+(w*y)+(x*z)+(y*z))"

{-

But this one only gets to $((((w*x)+(w*y)+(x*z)+(y*z))*u)+(((w*x)+(w*y)+(x*z)+(y*z))*v))$, using simp1
-}
simpTest6_7 =
    pretty (unScalar (simplify ((x + y) * (w + z) * (u + v)))) ==
    "((u*w*x)+(u*w*y)+(u*x*z)+(u*y*z)+(v*w*x)+(v*w*y)+(v*x*z)+(v*y*z))"

{-
Things get more interesting though, because running simplify (with only simp1) on the expression again gives a different answer.
-}
simpTest6_8 =
    pretty
        (unScalar
             (simplify
                  ((((w * x) + (w * y) + (x * z) + (y * z)) * u) +
                   (((w * x) + (w * y) + (x * z) + (y * z)) * v)))) ==
    "((u*w*x)+(u*w*y)+(u*x*z)+(u*y*z)+(v*w*x)+(v*w*y)+(v*x*z)+(v*y*z))"

{-
Which means this should work, right? No.
-}
simpTest6_9 =
    pretty (unScalar (simplify (simplify ((x + y) * (w + z) * (u + v))))) ==
    "((u*w*x)+(u*w*y)+(u*x*z)+(u*y*z)+(v*w*x)+(v*w*y)+(v*x*z)+(v*y*z))"

{-
How about this? No.
-}
simpTest6_10 =
    pretty (simplifyE "" (unScalar (simplify ((x + y) * (w + z) * (u + v))))) ==
    "((u*w*x)+(u*w*y)+(u*x*z)+(u*y*z)+(v*w*x)+(v*w*y)+(v*x*z)+(v*y*z))"

{-
Or this? No.
-}
simpTest6_11 =
    pretty
        (simplifyE "" (simplifyE "" (unScalar (((x + y) * (w + z) * (u + v)))))) ==
    "((u*w*x)+(u*w*y)+(u*x*z)+(u*y*z)+(v*w*x)+(v*w*y)+(v*x*z)+(v*y*z))"

{-

Let's move on to vectors.
-}
simpTest6_12 = simplify (x1 <.> (y1 + z1)) == ((x1 <.> y1) + (x1 <.> z1))

simpTest6_13 = simplify ((y1 + z1) <.> x1) == ((x1 <.> y1) + (x1 <.> z1))

{-
We need to use simplify in second arguments because the instance of sum groups the sums
-}
simpTest6_14 =
    simplify (x1 <.> (y1 + z1 + w1)) ==
    simplify ((x1 <.> w1) + (x1 <.> y1) + (x1 <.> z1))

simpTest6_15 =
    simplify ((y1 + z1 + w1) <.> x1) ==
    simplify ((x1 <.> w1) + (x1 <.> y1) + (x1 <.> z1))

simpTest6_16 =
    simplify ((y1 + z1) <.> (x1 + w1)) ==
    simplify ((w1 <.> y1) + (w1 <.> z1) + (x1 <.> y1) + (x1 <.> z1))

simpTest6_17 =
    simplify ((x1 <.> y1) *. (z1 + w1)) ==
    (((x1 <.> y1) *. w1) + ((x1 <.> y1) *. z1))

{-

At what point is distribution no longer a simplification?  I don't know; I'm just asking. TB
Both the rule in simp1 and the more complicated one below will distribute this.
-}
simpTest6_18 =
    simplify (((x3 <.> y3) + (z3 <.> w3)) *. (x1 + y1 + z1 + w1 + v1 + u1)) ==
    simplify
        ((((w3 <.> z3) + (x3 <.> y3)) *. u1) +
         (((w3 <.> z3) + (x3 <.> y3)) *. v1) +
         (((w3 <.> z3) + (x3 <.> y3)) *. w1) +
         (((w3 <.> z3) + (x3 <.> y3)) *. x1) +
         (((w3 <.> z3) + (x3 <.> y3)) *. y1) +
         (((w3 <.> z3) + (x3 <.> y3)) *. z1))

{-

\subsection{simpTest7} Complex scalars and vectors

Forming complex numbers with (+:)  Note that this is different from (:+), which is defined in Data.Complex and does a similar thing
\begin{itemize}
\item defined in HashedExpression.lhs
\item predecence 6
\end{itemize}

Extracting the real or imaginary part of a complex number.  This does not happen in HashedComplexInstances.lhs, but in simp1 in HashedSimplify.lhs
-}
simpTest7_0 = simplify (xRe (x +: y)) == x

simpTest7_1 = simplify (xIm (x +: y)) == y

{-

Addition works, and simplifies nicely.
-}
simpTest7_2 = simplify ((x +: y) + (0 +: 0)) == (x +: y)

simpTest7_3 = simplify ((x +: y) + (1 +: 0)) == ((x + 1) +: y)

simpTest7_3_1 = [simplify ((x +: y) + (1 +: 0)), ((x + 1) +: y)]

simpTest7_4 = simplify ((x +: y) + (0 +: z)) == (x +: (y + z))

simpTest7_5 = simplify ((x +: y) + (w +: z)) == ((w + x) +: (y + z))

simpTest7_6 = simplify ((x +: y) + (x +: y)) == (x * 2) +: (y * 2)

simpTest7_7 = simplify ((x1 +: y1) + (x1 +: y1)) == (2 *. x1) +: (2 *. y1)

{-

This does too, because a number is in the Num class, and ScalarC is an instance of the Num class.  From the context the type of the number is inferred as ScalarC.
-}
simpTest7_8 = simplify (0 + (x +: y)) == (x +: y)

simpTest7_9 = simplify (1 + (x +: y)) == ((x + 1) +: y)

simpTest7_10 = simplify (1.5 + (x +: y)) == ((x + 1.5) +: y)

{-

But these don't, because z has type Scalar and the type does not change for the context.
-}
--simpTest7_11 = simplify(z+(3 +: 4)) == ((z+3) +: 4)
--simpTest7_12 = simplify(z+(x +: y)) == ((x+z) +: y)
--simpTest7_13 = simplify(x1 + (y1 +: z1))
{-

Putting a number of rules together.  Everything seems to work so far
-}
simpTest7_14 = simplify (xRe ((x1 <.> zero1) +: y)) == 0

simpTest7_15 = simplify (xRe ((x + y) +: (z + w))) == (x + y)

simpTest7_16 =
    simplify (xRe ((x + y + u) +: (z + w + v))) == simplify (x + y + u)

simpTest7_17 =
    simplify (xRe ((x1 + y1 + u1) +: (z1 + w1 + v1))) == simplify (x1 + y1 + u1)

simpTest7_18 =
    simplify (xRe ((x1 +: y1) + (z1 +: w1)) +: (xIm ((x1 +: y1) + (z1 +: w1)))) ==
    ((x1 + z1) +: (w1 + y1))

simpTest7_19 =
    simplify
        (xRe ((x1 +: y1) + (z1 +: w1) + (u1 +: v1)) +:
         (xIm ((x1 +: y1) + (z1 +: w1) + (u1 +: v1)))) ==
    simplify ((x1 + z1 + u1) +: (w1 + y1 + v1))

{-

The simplify in the second part is to avoid difficulties with parentheses.  See the note above about the instance of sum.
-}
simpTest7_20 =
    simplify ((x +: y) + (w +: z) + (u +: v)) ==
    simplify ((u + w + x) +: (v + y + z))

simpTest7_21 = simplify ((x +: 0) + (w +: z) + (0 +: v)) == ((w + x) +: (v + z))

simpTest7_22 =
    simplify ((x +: z) + (w +: z) + (0 +: v)) ==
    simplify ((w + x) +: (v + 2 * z))

simpTest7_23 = simplify (xRe (x +: y) + 0) == x

simpTest7_24 = simplify (xIm (x +: y) * 1) == y

simpTest7_25 = simplify (xIm (x +: 0) * x) == 0

simpTest7_26 = simplify (xRe (x +: 0) * y) == (x * y)

simpTest7_27 = simplify (xRe (x +: 0) * y + 0) == (x * y)

simpTest7_28 = simplify (1 * xRe (x +: 0) * y + 0) == (x * y)

simpTest7_29 = simplify ((((x1 <.> zero1) *. y1) <.> z1) + x) == x

simpTest7_30 = simplify (1 *. (xRe (x1 +: y1) + xIm (z1 +: zero1))) == x1

{-
Just messing around; this should not work.
-}
--simpTest7_31 = simplify((3 +: 4) +: z) == (3 +: (4+z))
{-

Back to sanity.
-}
simpTest7_32 = simplify (xRe ((x * 0 * y * z) +: w)) == 0

{-

Fixing topSort made this work.
-}
simpTest7_33 = simplify ((x + 0 * y) +: x * y) == (x +: x * y)

simpTest7_33_1 = [simplify ((x + 0 * y) +: x * y), (x +: x * y)]

{-
-}
simpTest7_34 =
    simplify (ft (y3 +: x3) <.> ft (z3 +: y3) + (y3 <.> zero3)) ==
    ft (y3 +: x3) <.> ft (z3 +: y3)

simpTest7_35 =
    simplify (x * (x + zero1 <.> z1) + (z1 <.> zero1) + x) == x * x + x

{-

These can simplify using simp1 only
-}
simpTest7_36 = simplify (x *. (y1 + z1)) == (x *. y1 + x *. z1)

simpTest7_37 =
    pretty (unOneD (simplify (x *. (y1 + z1 + w1)))) ==
    "((x*.W1(4))+(x*.Y1(4))+(x*.Z1(4)))"

simpTest7_38 =
    pretty (unOneD (simplify (x *. (y1 + z1 - w1)))) ==
    "(((-1.0*x)*.W1(4))+(x*.Y1(4))+(x*.Z1(4)))"

{-



\subsection{simpTest8} Complex scalar and vectors cont.

This works, the rule is in simp1
-}
simpTest8_0 =
    simplify ((x +: y) * (z +: w)) ==
    simplify ((-y * w + x * z) +: (x * w + y * z))

simpTest8_1 = simplify ((x +: y) * (0 +: w)) == simplify ((-y * w) +: (x * w))

simpTest8_2 =
    simplify ((x +: y) * (1 +: w)) == simplify ((-y * w + x) +: (x * w + y))

{-

This does work but it is annoying to check because of all the parentheses and negatives.
-}
simpTest8_3 =
    simplify ((x +: y) * (z +: w) * (u +: v)) ==
    (((-((u * w * y) + (v * w * x))) + ((-(v * y)) * z) + (u * x * z)) +:
     (((-(v * y)) * w) + (u * w * x) + (u * y * z) + (v * x * z)))

simpTest8_4 =
    pretty (unScalarC (simplify ((x +: y) * (z +: w) * (u +: v)))) ==
    "(((-((u*w*y)+(v*w*x)))+((-(v*y))*z)+(u*x*z))+:(((-(v*y))*w)+(u*w*x)+(u*y*z)+(v*x*z)))"

{-

Dot products of complex vectors.  Some things happen in HashedComplexInstances.lhs
-}
simpTest8_5 =
    ((x1 +: y1) <.> (z1 +: w1)) ==
    ((xRe (x1 +: y1) <.> xRe (z1 +: w1)) + (xIm (x1 +: y1) <.> xIm (z1 +: w1)))

{-

Simplifying does more.
-}
simpTest8_6 =
    simplify ((x1 +: y1) <.> (z1 +: w1)) == ((w1 <.> y1) + (x1 <.> z1))

simpTest8_7 = simplify ((x1 +: y1) <.> (zero1 +: w1)) == (w1 <.> y1)

{-

-}
simpTest8_8 =
    simplify ((x1 +: w1) <.> ((y1 +: v1) + (z1 +: u1))) ==
    simplify (((u1 <.> w1) + (v1 <.> w1) + (x1 <.> y1) + (x1 <.> z1)))

simpTest8_9 =
    pretty (unScalar (simplify ((x1 + y1) <.> (z1 + w1)))) ==
    "((W1(4)<.>X1(4))+(W1(4)<.>Y1(4))+(X1(4)<.>Z1(4))+(Y1(4)<.>Z1(4)))"

simpTest8_10 =
    simplify ((y1 + z1 + u1) <.> (x1 + w1 + v1)) ==
    simplify
        ((u1 <.> v1) + (u1 <.> w1) + (u1 <.> x1) + (v1 <.> y1) + (v1 <.> z1) +
         (w1 <.> y1) +
         (w1 <.> z1) +
         (x1 <.> y1) +
         (x1 <.> z1))

{-

\subsection{simpTest9}Scaling complex scalars and vectors

A complex scalar can only be scaled using (*).  Since (*) requires two of the same type, there is no multiplying Scalar by ScalarC. See simpTest8 for more examples.

This works because 2 is an instance of the Num class, and from the context, it is inferred to be ScalarC, which is a type in the Num class.  However, z is of type Scalar, and does not convert to ScalarC.
-}
simpTest9_0 = simplify (2 * (x +: y)) == (x * 2 +: y * 2)

--simpTest9_1 = simplify(z*(3 +: 4)) == (3*z +: 4*z)
--simpTest9_2 = simplify(z*(x +: y)) == (x*z +: y*z)
simpTest9_3 =
    simplify (2 * ((x +: y) + (z +: w) + (v +: u))) ==
    simplify (((2 * x) + (2 * z) + (2 * v)) +: ((2 * y) + (2 * w) + (2 * u)))

simpTest9_3_1 =
    [ simplify (2 * ((x +: y) + (z +: w) + (v +: u)))
    , simplify (((2 * x) + (2 * z) + (2 * v)) +: ((2 * y) + (2 * w) + (2 * u)))
    ]

{-

Scale a complex vector using (*.)
-}
simpTest9_4 = simplify (x *. (x1 +: y1)) == ((x *. x1) +: (x *. y1))

simpTest9_5 =
    (x *. (x1 +: y1)) == ((x *. xRe (x1 +: y1)) +: (x *. xIm (x1 +: y1)))

simpTest9_6 =
    simplify (2 *. ((x1 +: y1) + (z1 +: w1))) ==
    (2 *. x1 + 2 *. z1) +: (2 *. w1 + 2 *. y1)

simpTest9_7 =
    simplify (2 *. ((x1 +: y1) + (z1 +: w1) + (u1 +: v1))) ==
    simplify ((2 *. u1 + 2 *. x1 + 2 *. z1) +: (2 *. v1 + 2 *. w1 + 2 *. y1))

{-

FIXME This simplifies, but the result causes a type error.
-}
simpTest9_8 = simplify (2 .*: (x1 +: y1)) -- == ((2 +: 0).*x1 +: (2 +: 0).*y1)

simpTest9_9 =
    pretty (unOneDC (simplify (2 .*: (x1 +: y1)))) ==
    "(((2.0+:0.0)*.X1(4))+:((2.0+:0.0)*.Y1(4)))"

simpTest9_10 = simplify ((x +: 0) .*: (x1 +: y1)) -- == ((x +: 0)*.x1 +: (x +: 0)*.y1)

simpTest9_11 =
    pretty (unOneDC (simplify ((x +: 0) .*: (x1 +: y1)))) ==
    "(((x+:0.0)*.X1(4))+:((x+:0.0)*.Y1(4)))"

{-

scaleR does not have an infix operator; but its instance is as (*.); so it does the same thing as scale
-}
simpTest9_12 = pretty (unOneDC (scaleR x (x1 +: y1))) == "(x*.(X1(4)+:Y1(4)))"

simpTest9_12_1 = [pretty (unOneDC (scaleR x (x1 +: y1))), "(x*.(X1(4)+:Y1(4)))"]

simpTest9_13 =
    pretty (unOneDC (scale x (x1 +: y1))) ==
    "((x*.(Re(X1(4)+:Y1(4))))+:(x*.(Im(X1(4)+:Y1(4)))))"

simpTest9_14 = simplify (scaleR x (x1 +: y1)) == ((x *. x1) +: (x *. y1))

{-
FIXME This doesn't work because there is no instance for it.  There should be an instance for this in HashedInstances (see 600-800); unless we scrap scaleR altogether
-}
--simpTest9_15 = scaleR x (x3 +: y3)
{-

(.*:) = scaleC
The instance of scaleC makes a scale (*.), like this, but that raises a type error.  FIXME the results of a simplify should probably be a valid expression.
-}
simpTest9_16 = (x +: y) .*: (x1 +: y1) -- == ((x +: y)*.(x1 +: y1))

simpTest9_17 =
    pretty (unOneDC ((x +: y) .*: (x1 +: y1))) == "((x+:y)*.(X1(4)+:Y1(4)))"

{-

Anyways, the rule under simp1 for scaling a complex vector using *. simplifies this expression
-}
simpTest9_18 = simplify ((x +: y) .*: (x1 +: y1)) -- == (((x +: y)*.x1)+:((x +: y)*.y1))

simpTest9_19 =
    pretty (unOneDC (simplify ((x +: y) .*: (x1 +: y1)))) ==
    "(((x+:y)*.X1(4))+:((x+:y)*.Y1(4)))"

simpTest9_20 =
    pretty (unOneDC (simplify ((x +: y) .*: ((x1 + w1) +: (y1 + z1))))) ==
    "((((x+:y)*.W1(4))+((x+:y)*.X1(4)))+:(((x+:y)*.Y1(4))+((x+:y)*.Z1(4))))"

simpTest9_21 =
    pretty (unOneDC (simplify ((x +: y) .*: ((z +: w) .*: (x1 +: y1))))) ==
    "(((((w*y*(-1.0))+(x*z))+:((w*x)+(y*z)))*.X1(4))+:((((w*y*(-1.0))+(x*z))+:((w*x)+(y*z)))*.Y1(4)))"

simpTest9_21_1 =
    [ pretty (unOneDC (simplify ((x +: y) .*: ((z +: w) .*: (x1 +: y1)))))
    , "(((((w*y*(-1.0))+(x*z))+:((w*x)+(y*z)))*.X1(4))+:((((w*y*(-1.0))+(x*z))+:((w*x)+(y*z)))*.Y1(4)))"
    ]

{-

\subsection{simpTest10} Projections, injections and Fourier transforms

Here are a few subspaces for testing.   FIXME if you can't turn it into an expression, is probably isn't valid.
-}
subA = SSNyquist [(3, (4, 5))]

subB = SSNyquist [(6, (7, 8))]

subM = SSNyquist [(3, (4, 5)), (6, (7, 8))]

subN = SSNyquist [(3, (4, 5)), (6, (7, 8)), (9, (10, 11))]

subX = (SSCrop [(1, 3), (0, 3)] [4, 4])

subY = (SSCrop [(2, 3), (2, 3)] [4, 4]) --trying to make a degenerate crop; this still makes a Dim2 (2,2); which means it's (probably?) not possible to make a degenerate crop

{-
and a few types of subspaces which are not properly defined, so we can't use them
-}
subC = SSUnion [subA, subB]

subD = SSInter [subA, subB]

subE = SSComplement subA

subF = SSCoord [Just 3, Nothing]

simpTest10_0 = simplify (xRe (projSS subA (x1 +: y1))) == projSS subA x1

simpTest10_1 = simplify (xIm (projSS subA (x1 +: y1))) == projSS subA y1

{-

-}
simpTest10_2 = simplify (xRe (projSS subM (x2 +: y2))) == projSS subM x2

simpTest10_3 = simplify (xIm (projSS subM (x2 +: y2))) == projSS subM y2

simpTest10_4 = simplify (xRe (projSS subN (x3 +: y3))) == projSS subN x3

simpTest10_5 = simplify (xIm (projSS subN (x3 +: y3))) == projSS subN y3

simpTest10_6 =
    simplify (xRe (projSS subA ((x1 + z1) +: y1))) == projSS subA (x1 + z1)

simpTest10_7 =
    simplify (xRe (projSS subA ((x1 + z1) +: y1))) == projSS subA (x1 + z1)

simpTest10_8 = simplify (xRe (injectSS subA (x1 +: y1))) == injectSS subA x1

simpTest10_9 = simplify (xIm (injectSS subA (x1 +: y1))) == injectSS subA y1

simpTest10_10 = simplify (xRe (injectSS subM (x2 +: y2))) == injectSS subM x2

simpTest10_11 = simplify (xIm (injectSS subM (x2 +: y2))) == injectSS subM y2

simpTest10_12 = simplify (xRe (injectSS subN (x3 +: y3))) == injectSS subN x3

simpTest10_13 = simplify (xIm (injectSS subN (x3 +: y3))) == injectSS subN y3

simpTest10_14 =
    simplify (xRe (projSS subA (x *. x1 +: y1))) == x *. projSS subA x1

--instance ~~> (Re((Proj_{[(3,(4,5))]}((Re((x*.X1(4))+:Y1(4)))))+:(Proj_{[(3,(4,5))]}((Im((x*.X1(4))+:Y1(4)))))))
--simp1 only ~~> (Proj_{[(3,(4,5))]}((x*.X1(4))))
--plus simplify rule for scaling a linear operation ~~> (x*.(Proj_{[(3,(4,5))]}(X1(4))))
{-

ft and invFt
\begin{itemize}
\item defined in HashedExpression.lhs
\item take an instance of the Rectangular class; instances are only defined for complex Rectangular
\item invFt automatically scales by the inverse of the size of the array FIXME ?
\end{itemize}

The factor for inverse fourier transform comes in HashedInstances.lhs 1159; I changed it but I'm not sure if that's a good idea, so I'll just leave the tests as they are.
-}
simpTest10_15 = simplify (ft (invFt (x1 +: y1))) == (0.25 *. x1 +: 0.25 *. y1)

simpTest10_16 = simplify (invFt (ft (x1 +: y1))) == (0.25 *. x1 +: 0.25 *. y1)

simpTest10_17 =
    simplify (ft (invFt (x2 +: y2))) == (0.0625 *. x2 +: 0.0625 *. y2)

simpTest10_18 =
    simplify (invFt (ft (x2 +: y2))) == (0.0625 *. x2 +: 0.0625 *. y2)

simpTest10_19 =
    simplify (ft (invFt (x3 +: y3))) == (0.015625 *. x3 +: 0.015625 *. y3)

simpTest10_20 =
    simplify (invFt (ft (x3 +: y3))) == (0.015625 *. x3 +: 0.015625 *. y3)

{-

FIXME sin (asin x) == x

\subsection{applyOne}
Previous testing used all of simplify.  Now we want to check which rules do what.

First, applyOne, the function which applies the simp1 rules.  It only applies simplifications to the top node so far.
-}
scalarApplyOne (Scalar (Expression n e)) =
    case applyOne (e, n) simp1 of
        Nothing -> pretty $ Expression n e
        _       -> pretty $ fromJust $ applyOne (e, n) simp1

{-

Now some tests.
-}
--simpTest11_0 = scalarApplyOne      something else to try--simplify (czZip (\x y -> x*y) (xRe (x1+:y1)) (xIm (x1 +: y1)))
{-

For testing multiple tests at a time, I followed the same format as evalTest and diffTests.  The test prints true or false, however, I leave quickCheck for tests which produce exceptions because otherwise the test gets stuck at the exception.
-}
--
regressionTestSimp =
    sequence
        [ regressionTestSimp0
        , regressionTestSimp1
        , regressionTestSimp2
        , regressionTestSimp3
        , regressionTestSimp4
        , regressionTestSimp5
        , regressionTestSimp6
        , regressionTestSimp7
        , regressionTestSimp8
        , regressionTestSimp9
        , regressionTestSimp10
        ]

regressionTestSimp0 =
    sequence
        [ putStrLn "simpTest0_0"
        , print simpTest0_0
        , putStrLn "simpTest0_1"
        , print simpTest0_1
        , putStrLn "simpTest0_2"
        , print simpTest0_2
        , putStrLn "simpTest0_3"
        , print simpTest0_3
        , putStrLn "simpTest0_4"
        , print simpTest0_4
        , putStrLn "simpTest0_5"
        , print simpTest0_5
        , putStrLn "simpTest0_6"
        , print simpTest0_6
        , putStrLn "simpTest0_7"
        , print simpTest0_7
        , putStrLn "simpTest0_8"
        , print simpTest0_8
        , putStrLn "simpTest0_9"
        , print simpTest0_9
        , putStrLn "simpTest0_10"
        , print simpTest0_10
        , putStrLn "simpTest0_11"
        , print simpTest0_11
        , putStrLn "simpTest0_12"
        , print simpTest0_12
        , putStrLn "simpTest0_13"
        , print simpTest0_13
        , putStrLn "simpTest0_14"
        , print simpTest0_14
        , putStrLn "simpTest0_15"
        , print simpTest0_15
        , putStrLn "simpTest0_16"
        , print simpTest0_16
        , putStrLn "simpTest0_17"
        , print simpTest0_17
        , putStrLn "simpTest0_18"
        , print simpTest0_18
        ]

regressionTestSimp1 =
    sequence
        [ putStrLn "simpTest1_0"
        , print simpTest1_0
        , putStrLn "simpTest1_1"
        , print simpTest1_1
        , putStrLn "simpTest1_2"
        , print simpTest1_2
        , putStrLn "simpTest1_3"
        , print simpTest1_3
        , putStrLn "simpTest1_4"
        , print simpTest1_4
        , putStrLn "simpTest1_5"
        , print simpTest1_5
        , putStrLn "simpTest1_6"
        , print simpTest1_6
        , putStrLn "simpTest1_7"
        , print simpTest1_7
        , putStrLn "simpTest1_8"
        , print simpTest1_8
        , putStrLn "simpTest1_9"
        , print simpTest1_9
        , putStrLn "simpTest1_10"
        , print simpTest1_10
        ]

regressionTestSimp2 =
    sequence
        [ putStrLn "simpTest2_0"
        , print simpTest2_0
        , putStrLn "simpTest2_1"
        , print simpTest2_1
        , putStrLn "simpTest2_2"
        , print simpTest2_2
        , putStrLn "simpTest2_3"
        , print simpTest2_3
        , putStrLn "simpTest2_4"
        , print simpTest2_4
        , putStrLn "simpTest2_5"
        , print simpTest2_5
        , putStrLn "simpTest2_6"
        , print simpTest2_6
        , putStrLn "simpTest2_7"
        , print simpTest2_7
        , putStrLn "simpTest2_8"
        , print simpTest2_8
        ]

regressionTestSimp3 =
    sequence
        [ putStrLn "simpTest3_0"
        , print simpTest3_0
        , putStrLn "simpTest3_1"
        , print simpTest3_1
        --, putStrLn "simpTest3_2", print simpTest3_2
        --, putStrLn "simpTest3_3", print simpTest3_3
        --, putStrLn "simpTest3_4", print simpTest3_4
        , putStrLn "simpTest3_5"
        , print simpTest3_5
        , putStrLn "simpTest3_6"
        , print simpTest3_6
        , putStrLn "simpTest3_7"
        , print simpTest3_7
        --, putStrLn "simpTest3_8", print simpTest3_8
        , putStrLn "simpTest3_9"
        , print simpTest3_9
        , putStrLn "simpTest3_10"
        , print simpTest3_10
        , putStrLn "simpTest3_11"
        , print simpTest3_11
        , putStrLn "simpTest3_12"
        , print simpTest3_12
        --, putStrLn "simpTest3_13", print simpTest3_13
        --, putStrLn "simpTest3_14", print simpTest3_14
        ]

regressionTestSimp4 =
    sequence
        [ putStrLn "simpTest4_0"
        , print simpTest4_0
        , putStrLn "simpTest4_1"
        , print simpTest4_1
        , putStrLn "simpTest4_2"
        , print simpTest4_2
        ]

regressionTestSimp5 =
    sequence
        [ putStrLn "simpTest5_0"
        , print simpTest5_0
        , putStrLn "simpTest5_1"
        , print simpTest5_1
        , putStrLn "simpTest5_2"
        , print simpTest5_2
        , putStrLn "simpTest5_3"
        , print simpTest5_3
        , putStrLn "simpTest5_4"
        , print simpTest5_4
        , putStrLn "simpTest5_5"
        , print simpTest5_5
        , putStrLn "simpTest5_6"
        , print simpTest5_6
        , putStrLn "simpTest5_7"
        , print simpTest5_7
        , putStrLn "simpTest5_8"
        , print simpTest5_8
        , putStrLn "simpTest5_9"
        , print simpTest5_9
        , putStrLn "simpTest5_10"
        , print simpTest5_10
        , putStrLn "simpTest5_11"
        , print simpTest5_11
        , putStrLn "simpTest5_12"
        , print simpTest5_12
        , putStrLn "simpTest5_13"
        , print simpTest5_13
        , putStrLn "simpTest5_14"
        , print simpTest5_14
        , putStrLn "simpTest5_15"
        , print simpTest5_15
        --, putStrLn "simpTest5_16", print simpTest5_16
        , putStrLn "simpTest5_17"
        , print simpTest5_17
        , putStrLn "simpTest5_18"
        , print simpTest5_18
        , putStrLn "simpTest5_19"
        , print simpTest5_19
        , putStrLn "simpTest5_20"
        , print simpTest5_20
        , putStrLn "simpTest5_21"
        , print simpTest5_21
        , putStrLn "simpTest5_22"
        , print simpTest5_22
        , putStrLn "simpTest5_23"
        , print simpTest5_23
        , putStrLn "simpTest5_24"
        , print simpTest5_24
        , putStrLn "simpTest5_25"
        , print simpTest5_25
        , putStrLn "simpTest5_26"
        , print simpTest5_26
        , putStrLn "simpTest5_27"
        , print simpTest5_27
        , putStrLn "simpTest5_28"
        , print simpTest5_28
        , putStrLn "simpTest5_29"
        , print simpTest5_29
        ]

regressionTestSimp6 =
    sequence
        [ putStrLn "simpTest6_0"
        , print simpTest6_0
        , putStrLn "simpTest6_1"
        , print simpTest6_1
        , putStrLn "simpTest6_2"
        , print simpTest6_2
        , putStrLn "simpTest6_3"
        , print simpTest6_3
        , putStrLn "simpTest6_4"
        , print simpTest6_4
        , putStrLn "simpTest6_5"
        , print simpTest6_5
        , putStrLn "simpTest6_6"
        , print simpTest6_6
        , putStrLn "simpTest6_7"
        , print simpTest6_7
        , putStrLn "simpTest6_8"
        , print simpTest6_8
        , putStrLn "simpTest6_9"
        , print simpTest6_9
        , putStrLn "simpTest6_10"
        , print simpTest6_10
        , putStrLn "simpTest6_11"
        , print simpTest6_11
        , putStrLn "simpTest6_12"
        , print simpTest6_12
        , putStrLn "simpTest6_13"
        , print simpTest6_13
        , putStrLn "simpTest6_14"
        , print simpTest6_14
        , putStrLn "simpTest6_15"
        , print simpTest6_15
        , putStrLn "simpTest6_16"
        , print simpTest6_16
        , putStrLn "simpTest6_17"
        , print simpTest6_17
        , putStrLn "simpTest6_18"
        , print simpTest6_18
        ]

regressionTestSimp7 =
    sequence
        [ putStrLn "simpTest7_0"
        , print simpTest7_0
        , putStrLn "simpTest7_1"
        , print simpTest7_1
        , putStrLn "simpTest7_2"
        , print simpTest7_2
        , putStrLn "simpTest7_3"
        , print simpTest7_3
        , putStrLn "simpTest7_4"
        , print simpTest7_4
        , putStrLn "simpTest7_5"
        , print simpTest7_5
        , putStrLn "simpTest7_6"
        , print simpTest7_6
        , putStrLn "simpTest7_7"
        , print simpTest7_7
        , putStrLn "simpTest7_8"
        , print simpTest7_8
        , putStrLn "simpTest7_9"
        , print simpTest7_9
        , putStrLn "simpTest7_10"
        , print simpTest7_10
        --, putStrLn "simpTest7_11", print simpTest7_11
        --, putStrLn "simpTest7_12", print simpTest7_12
        --, putStrLn "simpTest7_13", print simpTest7_13
        , putStrLn "simpTest7_14"
        , print simpTest7_14
        , putStrLn "simpTest7_15"
        , print simpTest7_15
        , putStrLn "simpTest7_16"
        , print simpTest7_16
        , putStrLn "simpTest7_17"
        , print simpTest7_17
        , putStrLn "simpTest7_18"
        , print simpTest7_18
        , putStrLn "simpTest7_19"
        , print simpTest7_19
        , putStrLn "simpTest7_20"
        , print simpTest7_20
        , putStrLn "simpTest7_21"
        , print simpTest7_21
        , putStrLn "simpTest7_22"
        , print simpTest7_22
        , putStrLn "simpTest7_23"
        , print simpTest7_23
        , putStrLn "simpTest7_24"
        , print simpTest7_24
        , putStrLn "simpTest7_25"
        , print simpTest7_25
        , putStrLn "simpTest7_26"
        , print simpTest7_26
        , putStrLn "simpTest6_27"
        , print simpTest7_27
        , putStrLn "simpTest7_28"
        , print simpTest7_28
        , putStrLn "simpTest7_29"
        , print simpTest7_29
        , putStrLn "simpTest7_30"
        , print simpTest7_30
        --, putStrLn "simpTest7_31", print simpTest7_31
        , putStrLn "simpTest7_32"
        , print simpTest7_32
        , putStrLn "simpTest7_33"
        , print simpTest7_33
        , putStrLn "simpTest7_34"
        , print simpTest7_34
        , putStrLn "simpTest7_35"
        , print simpTest7_35
        , putStrLn "simpTest7_36"
        , print simpTest7_36
        , putStrLn "simpTest7_37"
        , print simpTest7_37
        ]

regressionTestSimp8 =
    sequence
        [ putStrLn "simpTest8_0"
        , print simpTest8_0
        , putStrLn "simpTest8_1"
        , print simpTest8_1
        , putStrLn "simpTest8_2"
        , print simpTest8_2
        , putStrLn "simpTest8_3"
        , print simpTest8_3
        , putStrLn "simpTest8_4"
        , print simpTest8_4
        , putStrLn "simpTest8_5"
        , print simpTest8_5
        , putStrLn "simpTest8_6"
        , print simpTest8_6
        , putStrLn "simpTest8_7"
        , print simpTest8_7
        , putStrLn "simpTest8_8"
        , print simpTest8_8
        , putStrLn "simpTest8_9"
        , print simpTest8_9
        , putStrLn "simpTest8_10"
        , print simpTest8_10
        ]

regressionTestSimp9 =
    sequence
        [ putStrLn "simpTest9_0"
        , print simpTest9_0
        --, putStrLn "simpTest9_1", print simpTest9_1
        --, putStrLn "simpTest9_2", print simpTest9_2
        , putStrLn "simpTest9_3"
        , print simpTest9_3
        , putStrLn "simpTest9_4"
        , print simpTest9_4
        , putStrLn "simpTest9_5"
        , print simpTest9_5
        , putStrLn "simpTest9_6"
        , print simpTest9_6
        , putStrLn "simpTest9_7"
        , print simpTest9_7
        , putStrLn "simpTest9_8"
        , print simpTest9_8
        , putStrLn "simpTest9_9"
        , print simpTest9_9
        , putStrLn "simpTest9_10"
        , print simpTest9_10
        , putStrLn "simpTest9_11"
        , print simpTest9_11
        , putStrLn "simpTest9_12"
        , print simpTest9_12
        , putStrLn "simpTest9_13"
        , print simpTest9_13
        , putStrLn "simpTest9_14"
        , print simpTest9_14
        --, putStrLn "simpTest9_15", print simpTest9_15
        , putStrLn "simpTest9_16"
        , print simpTest9_16
        , putStrLn "simpTest9_17"
        , print simpTest9_17
        , putStrLn "simpTest9_18"
        , print simpTest9_18
        , putStrLn "simpTest9_19"
        , print simpTest9_19
        , putStrLn "simpTest9_20"
        , print simpTest9_20
        , putStrLn "simpTest9_21"
        , print simpTest9_21
        ]

regressionTestSimp10 =
    sequence
        [ putStrLn "simpTest10_0"
        , print simpTest10_0
        , putStrLn "simpTest10_1"
        , print simpTest10_1
        , putStrLn "simpTest10_2"
        , print simpTest10_2
        , putStrLn "simpTest10_3"
        , print simpTest10_3
        , putStrLn "simpTest10_4"
        , print simpTest10_4
        , putStrLn "simpTest10_5"
        , print simpTest10_5
        , putStrLn "simpTest10_6"
        , print simpTest10_6
        , putStrLn "simpTest10_7"
        , print simpTest10_7
        , putStrLn "simpTest10_8"
        , print simpTest10_8
        , putStrLn "simpTest10_9"
        , print simpTest10_9
        , putStrLn "simpTest10_10"
        , print simpTest10_10
        ]

--Run all QuickCheck Tests through regressionTest
regressionTestAll =
    sequence [regressionTestSimp, regressionTestEval, regressionTestPartDiff]

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
replist i l  = l ++ (replist (i - 1) l)

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
