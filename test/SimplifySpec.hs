module SimplifySpec where

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

{- Expressions can simplify in:
- the instance of an expression in HashedInstances.lhs
- a matching rule in simp1 in HashedSimplify.lhs
- a matching rule below simp1 in HashedSimplify.lhs
So far, only a few simplify rules have been implemented in their instances.  Most rules are in simp1 (WithHoles), but some more complicated cases do not work using only these rules, so there are the original, more complicated rules below simp1.

Some tests are commented out because otherwise they will not compile.

-[simpTest0] Scalar multiplication, with simplifications for products containing zero or one
-[simpTest1] Scalar addition, with simplifications for sums containing zero
-[simpTest2] Scalar addition and multiplication together, with simplifications for repeated values
-[simpTest3] Exponents and logarithms
-[simpTest4] Division, rule for zero divided by non-zero, which is not implemented yet
-[simpTest5] One, two and three dimensional vectors, with simplifications for addition, dot products and scaling
-[simpTest6] Distribution for scalars and vectors
-[simpTest7] Complex scalars and vectors, with simplifications for extracting real and imaginary and parts and addition
-[simpTest8] Complex scalars and vectors, with simplifications for scalar  multiplication and dot products
-[simpTest9] Scaling complex scalars and vectors
-[simpTest10] Projections, injections and Fourier transforms
\subsection{simpTest0}Multiplication

Multiplication with (*)

- defined in GHC.Num
- precedence 7
- left associative
- takes two instances of the Num class
Multiplication with product

- defined in Data.List
- takes a list of instances of the Num class
Begin by defining some variables of type Scalar.
-}
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

shouldCompile :: a -> Expectation
shouldCompile _ = return ()

spec :: Spec
spec =
    describe "simplify expression tests" $ do
        specify "TODO" $ do
            pendingWith
                "These tests belows fails, need fix or add implementations"
            {- FIXME add the rule for log(exp x) but for exp(log x) we need undefined and piecewise functions -}
            simplify (log (exp x)) `shouldBe` x
            simplify (exp (log x)) `shouldBe` x
            {- \subsection{simpTest4} Division
            A rule for simplifying $\frac{0}{x}$ could be dangerous if x is zero.  The rule is currently in simp1 (18/06/2015), but commented out until we can prove that x is not zero. -}
            simplify (0 / x) `shouldBe` 0
            {- This, for example, should be safe to simplify. -}
            simplify (0 / (x ^ 2 + 1)) `shouldBe` 0
            {- FIXME This simplifies, but the result causes a type error. -}
--            simplify (2 .*: (x1 +: y1)) `shouldBe` ((2 +: 0).*x1 +: (2 +: 0).*y1)
--            simplify ((x +: 0) .*: (x1 +: y1)) -- `shouldBe` ((x +: 0)*.x1 +: (x +: 0)*.y1)
--            (x +: y) .*: (x1 +: y1) -- `shouldBe` ((x +: y)*.(x1 +: y1))
--            simplify ((x +: y) .*: (x1 +: y1)) -- `shouldBe` (((x +: y)*.x1)+:((x +: y)*.y1
            {- This does work but it is annoying to check because of all the parentheses and negatives. -}
            simplify ((x +: y) * (z +: w) * (u +: v)) `shouldBe`
                (((-((u * w * y) + (v * w * x))) + ((-(v * y)) * z) +
                  (u * x * z)) +:
                 (((-(v * y)) * w) + (u * w * x) + (u * y * z) + (v * x * z)))
            simplify (ft (invFt (x1 +: y1))) `shouldBe`
                (0.25 *. x1 +: 0.25 *. y1)
            simplify (invFt (ft (x1 +: y1))) `shouldBe`
                (0.25 *. x1 +: 0.25 *. y1)
        specify "simplify 0" $ do
            simplify (x * 1) `shouldBe` x
            simplify (1 * x) `shouldBe` x
            {- x*0 and 0*x follow the same pattern as x*1 and 1*x -}
            simplify (x * 0) `shouldBe` 0
            simplify (0 * x) `shouldBe` 0
            {- These tests show how the instances of (*) and product work. -}
            product [x, y, z] `shouldNotBe` (x * y * z)
            product [x, y, z] `shouldBe` (1 * x * y * z)
            pretty (unScalar (product [x, y, z])) `shouldBe` "(((1.0*x)*y)*z)"
            pretty (unScalar (x * y * z)) `shouldBe` "((x*y)*z)"
            {- Now looking at some simplifications. -}
            pretty (unScalar (simplify ((x * y) * z))) `shouldBe` "(x*y*z)"
            pretty (unScalar (simplify (product [x, y, z]))) `shouldBe`
                "(x*y*z)"
            {- Use applyOne here. -}
            simplify ((x * 0) * y) `shouldBe` 0
            simplify (y * (x * 0)) `shouldBe` 0
            simplify (0 * (x * 1)) `shouldBe` 0
            simplify (0 * x * 1) `shouldBe` 0
            simplify (0 * (x * y)) `shouldBe` 0
            simplify ((x * y) * 0) `shouldBe` 0
            simplify ((x * 0) * 1) `shouldBe` 0
            simplify ((x * y) * 1) `shouldBe` (x * y)
            simplify (x * y * z * 1) `shouldBe` simplify (x * y * z)
        specify "simplify 1" $ do
            simplify (x + 0) `shouldBe` x
            simplify (0 + x) `shouldBe` x
            {- Here are some examples showing what the instance of (+) and sum do. -}
            sum [x, y, z] `shouldNotBe` (x + y + z)
            sum [x, y, z] `shouldBe` (0 + x + y + z)
            pretty (unScalar (sum [x, y, z])) `shouldBe` "(((0.0+x)+y)+z)"
            {- The instance of sum means that there will not be a sum containing only one term. -}
            sum [x] `shouldBe` 0 + x
            simplify (sum [x]) `shouldBe` x
            {- GHC.Num will do some calculations, but not all -}
            sum [2, 3, 4] `shouldBe` 9
            sum [2, 3, x] `shouldBe` (0 + 2 + 3 + x)
            {- As expected, the rule for collapsing a sum within a sum this first example will still work.
               The second, however, does not. -}
            simplify (sum [2, 3, x]) `shouldBe` x + 5
            simplify (sum [2, x, 3]) `shouldBe` x + 5
        specify "simplify 2" $ do
            simplify (x + x) `shouldBe` x * 2
            simplify (x + x + x + x) `shouldBe` x * 4
            simplify (x + x - x + x + x) `shouldBe` x * 3
            simplify (4 * x + 2 * x) `shouldBe` x * 6
            simplify (4 * x - 2 * x) `shouldBe` x * 2
            simplify ((x * y) + 0) `shouldBe` (x * y)
            simplify (x * y + 0) `shouldBe` (x * y)
            simplify (x * 0 + z) `shouldBe` z
            simplify (x + 0 * y) `shouldBe` x
        specify "simpily 3" $
            {- \subsection{simpTest3} Exponentiation and logarithms
            $e^x$ with exp

            - defined in GHC.Float
            - takes an instance of the Fractional class
$e^0$ evaluates right away to 1. -}
         do
            exp 0 `shouldBe` 1
            {- However, there are cases when a rule for $e^0$ is necessary.  This rule is only in simp1. -}
            simplify (exp (x - x)) `shouldBe` 1
            {- Passing the one to the simplify function causes an error because there is no instance of simplify for it. -}
            --simplify(exp 0) `shouldBe` 1
            {- This won't run, but it will compile. -}
    --        simplify (1) -- `shouldBe` 1
            {- This won't even compile. -}
            --simplify(1::Double)
            {- But these work.  Simplify needs something in the Transformable class and a literal is too ambiguous for that. -}
            simplify (1 :: Scalar) `shouldBe` 1
            simplify (1 :: ScalarC) `shouldBe` 1 +: 0
            {- Let's try this. -}
            simplify (x - x) `shouldBe` 0.0
            --simplify (0.0)
            simplify (simplify (x - x)) `shouldBe` 0.0
            {- The instance of  is as multiplication.  There are no rules for simplifying a product containing repeated values.
            Exponents with \^{}

            - Instance as repeated multiplication
            - Defined in GHC.Real
            - Right associative
            - precedence 8
            - takes an instance of the Num class and the Integral class
            - does not take negative exponents
            - only takes integer exponents
Exponents with ^^

            - Instance as repeated multiplication or division
            - Defined in GHC.Real
            - Right associative
            - precedence 8
            - takes an instance of the Fractional class and the Integral class
            - only takes integer exponents
Exponents with **

            - Instance as exp(log(base)*power)
            - Defiend in GHC.Float
            - - Right associative
            - precedence 8
            - takes two instances of the Floating class
-}
            x ^ 2 `shouldBe` x * x
            {- The Scalar x cannot be interpreted as being in the Integral class, so these raise type errors. -}
            --x^(x-x)
            --x^^(x-x)
            {- This does work. -}
            simplify (x ** (x - x)) `shouldBe` 1
            {- It looks like there's no support for vector exponents, which is fine, we don't really ever multiply vectors anyways.  The first example raises an exception: can't multiply 1d vectors -}
            shouldCompile $ x1 ^ 2
            shouldCompile $ x2 ^ 2
            shouldCompile $ x3 ^ 2
            {- And these won't compile. -}
            --x1^^2
            --x2^^2
            --x3^^2
            --x1**2
            --x2**2
            --x3**2
            {- Another random thing involving division; it associates to the left and has the same precedence as multiplication
            , which is no surprise to anyone. -}
        specify "simplify 4" $ do simplify (x / y * 0) `shouldBe` 0
        specify "simplify 5" $
            {- And a test to check the zero vector. -}
         do
            show (simplify $ x1 - x1) `shouldBe` "0.0(4)"
            {- Adding the zero vectors follows the same pattern as adding the zero scalar.  It uses the same simplify rules. -}
            simplify (x1 + zero1) `shouldBe` x1
            {- Dot product using (<.>)

            - infix for dot
            - Mac Option 8; Windows Alt 7; Linux Ctrl Shift u 2022
            - defined in HashedExpression.lhs
            - precedence 8
            - left associative
            - takes two instances of the Rectangular Class
            - defined in HashedExpression.lhs
Dot products with the zero vector simplify in simp1. -}
            simplify (zero1 <.> x1) `shouldBe` 0
            simplify (x1 <.> zero1) `shouldBe` 0
            simplify (x2 <.> zero2) `shouldBe` 0
            simplify (zero2 <.> x2) `shouldBe` 0
            simplify (x3 <.> zero3) `shouldBe` 0
            simplify (zero3 <.> x3) `shouldBe` 0
            {- Scaling a vector using (*.)

            - infix for scale
            - precedence 8
            - left associative
            - takes an instance of the Scalar class and the Rectangular class
            - defined in HashedExpression.lhs
These tests simplify with simp1 -}
            simplify (1 *. x1) `shouldBe` x1
            simplify (0 *. x1) `shouldBe` zero1
            simplify (x *. zero1) `shouldBe` zero1
            {- Addition and scaling together.  The same patterns applies as for scalar addition and multiplication. -}
            simplify (x1 + x1 - x1 + x1 + x1) `shouldBe` 3 *. x1
            show (simplify $ x1 + 7 *. x1) `shouldBe`
                ("(" ++ show (7 + 1 :: Double) ++ "*.X1(4))")
            simplify (x2 - 2 *. x2) `shouldBe` simplify (-1 *. x2)
            {- Scaling and dot product together simplify with simp1. -}
            simplify ((x *. x1) <.> y1) `shouldBe` (x1 <.> y1) * x
            simplify (y1 <.> (x *. x1)) `shouldBe` (x1 <.> y1) * x
            {- Dot product and scaling both have precedence 8 and are left associative, so they can sometimes mix without parentheses. -}
            --simplify(y1 <.> x*.x1) `shouldBe` (x1 <.> y1)*x
            simplify (x *. x1 <.> y1) `shouldBe` (x1 <.> y1) * x
            simplify (x *. (y *. z1)) `shouldBe` ((x * y) *. z1)
            simplify (x *. (y *. (z *. z1))) `shouldBe`
                simplify ((x * y * z) *. z1)
            {- Putting different things together, it works. -}
            simplify ((x1 <.> zero1) * x) `shouldBe` 0
            simplify ((zero1 <.> x1) * x) `shouldBe` 0
            simplify (x * (x1 <.> zero1)) `shouldBe` 0
            simplify (x * (zero1 <.> x1)) `shouldBe` 0
            simplify ((x1 <.> y1) * 1) `shouldBe` (x1 <.> y1)
            simplify (x1 <.> y1 * 1) `shouldBe` (x1 <.> y1)
            simplify (1 * x1 <.> y1) `shouldBe` (x1 <.> y1)
            simplify ((x1 <.> y1) + 0) `shouldBe` (x1 <.> y1)
            simplify (x1 <.> y1 + 0) `shouldBe` (x1 <.> y1)
            simplify (0 + x1 <.> y1) `shouldBe` (x1 <.> y1)
            {- \subsection{simpTest6} Distribution

            Nothing simplifies in the instances, just using simp1 -}
        specify "simplify 6" $ do
            simplify (x * (y + z)) `shouldBe` (x * y + x * z)
            pretty (unScalar (simplify (x * (y + z + w)))) `shouldBe`
                "((w*x)+(x*y)+(x*z))"
            pretty (unScalar (simplify ((y + z + w) * x))) `shouldBe`
                "((w*x)+(x*y)+(x*z))"
            {- The instance of subtraction is addition of the negative.  This is defined in GHC.Num.  These still simplify only using simp1. -}
            pretty (unScalar (x * (y + z - w))) `shouldBe`
                "(x*((y+z)+((-1.0)*w)))"
            pretty (unScalar (simplify (x * (y + z - w)))) `shouldBe`
                "((w*x*(-1.0))+(x*y)+(x*z))"
            pretty (unScalar (simplify ((y + z - w) * x))) `shouldBe`
                "((w*x*(-1.0))+(x*y)+(x*z))"
            {- This still uses simp1 -}
            pretty (unScalar (simplify ((x + y) * (w + z)))) `shouldBe`
                "((w*x)+(w*y)+(x*z)+(y*z))"
            {- But this one only gets to $((((w*x)+(w*y)+(x*z)+(y*z))*u)+(((w*x)+(w*y)+(x*z)+(y*z))*v))$, using simp1 -}
            pretty (unScalar (simplify ((x + y) * (w + z) * (u + v)))) `shouldBe`
                "((u*w*x)+(u*w*y)+(u*x*z)+(u*y*z)+(v*w*x)+(v*w*y)+(v*x*z)+(v*y*z))"
            {- Things get more interesting though, because running simplify (with only simp1) on the expression again gives a different answer. -}
            pretty
                (unScalar
                     (simplify
                          ((((w * x) + (w * y) + (x * z) + (y * z)) * u) +
                           (((w * x) + (w * y) + (x * z) + (y * z)) * v)))) `shouldBe`
                "((u*w*x)+(u*w*y)+(u*x*z)+(u*y*z)+(v*w*x)+(v*w*y)+(v*x*z)+(v*y*z))"
            {- Which means this should work, right? No. -}
            pretty
                (unScalar (simplify (simplify ((x + y) * (w + z) * (u + v))))) `shouldBe`
                "((u*w*x)+(u*w*y)+(u*x*z)+(u*y*z)+(v*w*x)+(v*w*y)+(v*x*z)+(v*y*z))"
            {- How about this? No. -}
            pretty
                (simplifyE
                     ""
                     (unScalar (simplify ((x + y) * (w + z) * (u + v))))) `shouldBe`
                "((u*w*x)+(u*w*y)+(u*x*z)+(u*y*z)+(v*w*x)+(v*w*y)+(v*x*z)+(v*y*z))"
            {- Or this? No. -}
            pretty
                (simplifyE
                     ""
                     (simplifyE "" (unScalar (((x + y) * (w + z) * (u + v)))))) `shouldBe`
                "((u*w*x)+(u*w*y)+(u*x*z)+(u*y*z)+(v*w*x)+(v*w*y)+(v*x*z)+(v*y*z))"
            {- Let's move on to vectors. -}
            simplify (x1 <.> (y1 + z1)) `shouldBe` ((x1 <.> y1) + (x1 <.> z1))
            simplify ((y1 + z1) <.> x1) `shouldBe` ((x1 <.> y1) + (x1 <.> z1))
            {- We need to use simplify in second arguments because the instance of sum groups the sums -}
            simplify (x1 <.> (y1 + z1 + w1)) `shouldBe`
                simplify ((x1 <.> w1) + (x1 <.> y1) + (x1 <.> z1))
            simplify ((y1 + z1 + w1) <.> x1) `shouldBe`
                simplify ((x1 <.> w1) + (x1 <.> y1) + (x1 <.> z1))
            simplify ((y1 + z1) <.> (x1 + w1)) `shouldBe`
                simplify ((w1 <.> y1) + (w1 <.> z1) + (x1 <.> y1) + (x1 <.> z1))
            simplify ((x1 <.> y1) *. (z1 + w1)) `shouldBe`
                (((x1 <.> y1) *. w1) + ((x1 <.> y1) *. z1))
            {- At what point is distribution no longer a simplification?  I don't know; I'm just asking. TB
            Both the rule in simp1 and the more complicated one below will distribute this. -}
            simplify
                (((x3 <.> y3) + (z3 <.> w3)) *. (x1 + y1 + z1 + w1 + v1 + u1)) `shouldBe`
                simplify
                    ((((w3 <.> z3) + (x3 <.> y3)) *. u1) +
                     (((w3 <.> z3) + (x3 <.> y3)) *. v1) +
                     (((w3 <.> z3) + (x3 <.> y3)) *. w1) +
                     (((w3 <.> z3) + (x3 <.> y3)) *. x1) +
                     (((w3 <.> z3) + (x3 <.> y3)) *. y1) +
                     (((w3 <.> z3) + (x3 <.> y3)) *. z1))
        specify "simplify 7" $
            {- \subsection{simpTest7} Complex scalars and vectors

            Forming complex numbers with (+:)  Note that this is different from (:+), which is defined in Data.Complex and does a similar thing

            - defined in HashedExpression.lhs
            - predecence 6
Extracting the real or imaginary part of a complex number.  This does not happen in HashedComplexInstances.lhs, but in simp1 in HashedSimplify.lhs -}
         do
            simplify (xRe (x +: y)) `shouldBe` x
            simplify (xIm (x +: y)) `shouldBe` y
            {- Addition works, and simplifies nicely. -}
            simplify ((x +: y) + (0 +: 0)) `shouldBe` (x +: y)
            simplify ((x +: y) + (1 +: 0)) `shouldBe` ((x + 1) +: y)
            simplify ((x +: y) + (0 +: z)) `shouldBe` (x +: (y + z))
            simplify ((x +: y) + (w +: z)) `shouldBe` ((w + x) +: (y + z))
            simplify ((x +: y) + (x +: y)) `shouldBe` (x * 2) +: (y * 2)
            simplify ((x1 +: y1) + (x1 +: y1)) `shouldBe` (2 *. x1) +: (2 *. y1)
            {- This does too, because a number is in the Num class, and ScalarC is an instance of the Num class.  From the context the type of the number is inferred as ScalarC. -}
            simplify (0 + (x +: y)) `shouldBe` (x +: y)
            simplify (1 + (x +: y)) `shouldBe` ((x + 1) +: y)
            simplify (1.5 + (x +: y)) `shouldBe` ((x + 1.5) +: y)
            {- But these don't, because z has type Scalar and the type does not change for the context. -}
            --simplify(z+(3 +: 4)) `shouldBe` ((z+3) +: 4)
            --simplify(z+(x +: y)) `shouldBe` ((x+z) +: y)
            --simplify(x1 + (y1 +: z1))
            {- Putting a number of rules together.  Everything seems to work so far -}
            simplify (xRe ((x1 <.> zero1) +: y)) `shouldBe` 0
            simplify (xRe ((x + y) +: (z + w))) `shouldBe` (x + y)
            simplify (xRe ((x + y + u) +: (z + w + v))) `shouldBe`
                simplify (x + y + u)
            simplify (xRe ((x1 + y1 + u1) +: (z1 + w1 + v1))) `shouldBe`
                simplify (x1 + y1 + u1)
            simplify
                (xRe ((x1 +: y1) + (z1 +: w1)) +:
                 (xIm ((x1 +: y1) + (z1 +: w1)))) `shouldBe`
                ((x1 + z1) +: (w1 + y1))
            simplify
                (xRe ((x1 +: y1) + (z1 +: w1) + (u1 +: v1)) +:
                 (xIm ((x1 +: y1) + (z1 +: w1) + (u1 +: v1)))) `shouldBe`
                simplify ((x1 + z1 + u1) +: (w1 + y1 + v1))
            {- The simplify in the second part is to avoid difficulties with parentheses.  See the note above about the instance of sum. -}
            simplify ((x +: y) + (w +: z) + (u +: v)) `shouldBe`
                simplify ((u + w + x) +: (v + y + z))
            simplify ((x +: 0) + (w +: z) + (0 +: v)) `shouldBe`
                ((w + x) +: (v + z))
            simplify ((x +: z) + (w +: z) + (0 +: v)) `shouldBe`
                simplify ((w + x) +: (v + 2 * z))
            simplify (xRe (x +: y) + 0) `shouldBe` x
            simplify (xIm (x +: y) * 1) `shouldBe` y
            simplify (xIm (x +: 0) * x) `shouldBe` 0
            simplify (xRe (x +: 0) * y) `shouldBe` (x * y)
            simplify (xRe (x +: 0) * y + 0) `shouldBe` (x * y)
            simplify (1 * xRe (x +: 0) * y + 0) `shouldBe` (x * y)
            simplify ((((x1 <.> zero1) *. y1) <.> z1) + x) `shouldBe` x
            simplify (1 *. (xRe (x1 +: y1) + xIm (z1 +: zero1))) `shouldBe` x1
            {- Just messing around; this should not work. -}
            --simplify((3 +: 4) +: z) `shouldBe` (3 +: (4+z))
            {- Back to sanity. -}
            simplify (xRe ((x * 0 * y * z) +: w)) `shouldBe` 0
            {- Fixing topSort made this work. -}
            simplify ((x + 0 * y) +: x * y) `shouldBe` (x +: x * y)
            {- -}
            simplify (ft (y3 +: x3) <.> ft (z3 +: y3) + (y3 <.> zero3)) `shouldBe`
                ft (y3 +: x3) <.>
                ft (z3 +: y3)
            simplify (x * (x + zero1 <.> z1) + (z1 <.> zero1) + x) `shouldBe` x *
                x +
                x
            {- These can simplify using simp1 only -}
            simplify (x *. (y1 + z1)) `shouldBe` (x *. y1 + x *. z1)
            pretty (unOneD (simplify (x *. (y1 + z1 + w1)))) `shouldBe`
                "((x*.W1(4))+(x*.Y1(4))+(x*.Z1(4)))"
            pretty (unOneD (simplify (x *. (y1 + z1 - w1)))) `shouldBe`
                "(((x*(-1.0))*.W1(4))+(x*.Y1(4))+(x*.Z1(4)))"
        specify "simplify 8" $
            {- \subsection{simpTest8} Complex scalar and vectors cont.

            This works, the rule is in simp1 -}
         do
            simplify ((x +: y) * (z +: w)) `shouldBe`
                simplify ((-y * w + x * z) +: (x * w + y * z))
            simplify ((x +: y) * (0 +: w)) `shouldBe`
                simplify ((-y * w) +: (x * w))
            simplify ((x +: y) * (1 +: w)) `shouldBe`
                simplify ((-y * w + x) +: (x * w + y))
            pretty (unScalarC (simplify ((x +: y) * (z +: w) * (u +: v)))) `shouldBe`
                "(((-((u*w*y)+(v*w*x)))+((-(v*y))*z)+(u*x*z))+:(((-(v*y))*w)+(u*w*x)+(u*y*z)+(v*x*z)))"
            {- Dot products of complex vectors.  Some things happen in HashedComplexInstances.lhs -}
            ((x1 +: y1) <.> (z1 +: w1)) `shouldBe`
                ((xRe (x1 +: y1) <.> xRe (z1 +: w1)) +
                 (xIm (x1 +: y1) <.> xIm (z1 +: w1)))
            {- Simplifying does more. -}
            simplify ((x1 +: y1) <.> (z1 +: w1)) `shouldBe`
                ((w1 <.> y1) + (x1 <.> z1))
            simplify ((x1 +: y1) <.> (zero1 +: w1)) `shouldBe` (w1 <.> y1)
            {- -}
            simplify ((x1 +: w1) <.> ((y1 +: v1) + (z1 +: u1))) `shouldBe`
                simplify
                    (((u1 <.> w1) + (v1 <.> w1) + (x1 <.> y1) + (x1 <.> z1)))
            pretty (unScalar (simplify ((x1 + y1) <.> (z1 + w1)))) `shouldBe`
                "((W1(4)<.>X1(4))+(W1(4)<.>Y1(4))+(X1(4)<.>Z1(4))+(Y1(4)<.>Z1(4)))"
            simplify ((y1 + z1 + u1) <.> (x1 + w1 + v1)) `shouldBe`
                simplify
                    ((u1 <.> v1) + (u1 <.> w1) + (u1 <.> x1) + (v1 <.> y1) +
                     (v1 <.> z1) +
                     (w1 <.> y1) +
                     (w1 <.> z1) +
                     (x1 <.> y1) +
                     (x1 <.> z1))
        specify "simplify 9" $
            {- \subsection{simpTest9}Scaling complex scalars and vectors

            A complex scalar can only be scaled using (*).  Since (*) requires two of the same type, there is no multiplying Scalar by ScalarC. See simpTest8 for more examples.

            This works because 2 is an instance of the Num class, and from the context, it is inferred to be ScalarC, which is a type in the Num class.  However, z is of type Scalar, and does not convert to ScalarC. -}
         do
            simplify (2 * (x +: y)) `shouldBe` (x * 2 +: y * 2)
            --simplify(z*(3 +: 4)) `shouldBe` (3*z +: 4*z)
            --simplify(z*(x +: y)) `shouldBe` (x*z +: y*z)
            simplify (2 * ((x +: y) + (z +: w) + (v +: u))) `shouldBe`
                simplify
                    (((2 * x) + (2 * z) + (2 * v)) +:
                     ((2 * y) + (2 * w) + (2 * u)))
            {- Scale a complex vector using (*.) -}
            simplify (x *. (x1 +: y1)) `shouldBe` ((x *. x1) +: (x *. y1))
            (x *. (x1 +: y1)) `shouldBe`
                ((x *. xRe (x1 +: y1)) +: (x *. xIm (x1 +: y1)))
            simplify (2 *. ((x1 +: y1) + (z1 +: w1))) `shouldBe`
                (2 *. x1 + 2 *. z1) +:
                (2 *. w1 + 2 *. y1)
            simplify (2 *. ((x1 +: y1) + (z1 +: w1) + (u1 +: v1))) `shouldBe`
                simplify
                    ((2 *. u1 + 2 *. x1 + 2 *. z1) +:
                     (2 *. v1 + 2 *. w1 + 2 *. y1))
            pretty (unOneDC (simplify (2 .*: (x1 +: y1)))) `shouldBe`
                "(((2.0+:0.0)*.X1(4))+:((2.0+:0.0)*.Y1(4)))"
            pretty (unOneDC (simplify ((x +: 0) .*: (x1 +: y1)))) `shouldBe`
                "(((x+:0.0)*.X1(4))+:((x+:0.0)*.Y1(4)))"
            {- scaleR does not have an infix operator; but its instance is as (*.); so it does the same thing as scale -}
            pretty (unOneDC (scaleR x (x1 +: y1))) `shouldBe`
                "(x*.(X1(4)+:Y1(4)))"
            pretty (unOneDC (scale x (x1 +: y1))) `shouldBe`
                "((x*.(Re(X1(4)+:Y1(4))))+:(x*.(Im(X1(4)+:Y1(4)))))"
            simplify (scaleR x (x1 +: y1)) `shouldBe` ((x *. x1) +: (x *. y1))
            {- FIXME This doesn't work because there is no instance for it.  There should be an instance for this in HashedInstances (see 600-800); unless we scrap scaleR altogether -}
            --scaleR x (x3 +: y3)
            {- (.*:) = scaleC
            The instance of scaleC makes a scale (*.), like this, but that raises a type error.  FIXME the results of a simplify should probably be a valid expression. -}
            pretty (unOneDC ((x +: y) .*: (x1 +: y1))) `shouldBe`
                "((x+:y)*.(X1(4)+:Y1(4)))"
            {- Anyways, the rule under simp1 for scaling a complex vector using *. simplifies this expression -}
            pretty (unOneDC (simplify ((x +: y) .*: (x1 +: y1)))) `shouldBe`
                "(((x+:y)*.X1(4))+:((x+:y)*.Y1(4)))"
            pretty (unOneDC (simplify ((x +: y) .*: ((x1 + w1) +: (y1 + z1))))) `shouldBe`
                "((((x+:y)*.W1(4))+((x+:y)*.X1(4)))+:(((x+:y)*.Y1(4))+((x+:y)*.Z1(4))))"
            pretty (unOneDC (simplify ((x +: y) .*: ((z +: w) .*: (x1 +: y1))))) `shouldBe`
                "(((((w*y*(-1.0))+(x*z))+:((w*x)+(y*z)))*.X1(4))+:((((w*y*(-1.0))+(x*z))+:((w*x)+(y*z)))*.Y1(4)))"
        specify "simplify 10" $ do
            simplify (xRe (projSS subA (x1 +: y1))) `shouldBe` projSS subA x1
            simplify (xIm (projSS subA (x1 +: y1))) `shouldBe` projSS subA y1
            {- -}
            simplify (xRe (projSS subM (x2 +: y2))) `shouldBe` projSS subM x2
            simplify (xRe (projSS subN (x3 +: y3))) `shouldBe` projSS subN x3
            simplify (xIm (projSS subN (x3 +: y3))) `shouldBe` projSS subN y3
            simplify (xRe (projSS subA ((x1 + z1) +: y1))) `shouldBe`
                projSS subA (x1 + z1)
            simplify (xRe (projSS subA ((x1 + z1) +: y1))) `shouldBe`
                projSS subA (x1 + z1)
            simplify (xRe (injectSS subA (x1 +: y1))) `shouldBe`
                injectSS subA x1
            simplify (xIm (injectSS subA (x1 +: y1))) `shouldBe`
                injectSS subA y1
            simplify (xRe (injectSS subM (x2 +: y2))) `shouldBe`
                injectSS subM x2
            simplify (xIm (injectSS subM (x2 +: y2))) `shouldBe`
                injectSS subM y2
            simplify (xRe (injectSS subN (x3 +: y3))) `shouldBe`
                injectSS subN x3
            simplify (xIm (injectSS subN (x3 +: y3))) `shouldBe`
                injectSS subN y3
            simplify (xRe (projSS subA (x *. x1 +: y1))) `shouldBe` x *.
                projSS subA x1
            --instance ~~> (Re((Proj_{[(3,(4,5))]}((Re((x*.X1(4))+:Y1(4)))))+:(Proj_{[(3,(4,5))]}((Im((x*.X1(4))+:Y1(4)))))))
            --simp1 only ~~> (Proj_{[(3,(4,5))]}((x*.X1(4))))
            --plus simplify rule for scaling a linear operation ~~> (x*.(Proj_{[(3,(4,5))]}(X1(4))))
            {- ft and invFt
            - defined in HashedExpression.lhs
            - take an instance of the Rectangular class; instances are only defined for complex Rectangular
            - invFt automatically scales by the inverse of the size of the array FIXME ?
            The factor for inverse fourier transform comes in HashedInstances.lhs 1159; I changed it but I'm not sure if that's a good idea, so I'll just leave the tests as they are. -}
            simplify (ft (invFt (x2 +: y2))) `shouldBe`
                (0.0625 *. x2 +: 0.0625 *. y2)
            simplify (invFt (ft (x2 +: y2))) `shouldBe`
                (0.0625 *. x2 +: 0.0625 *. y2)
            simplify (ft (invFt (x3 +: y3))) `shouldBe`
                (0.015625 *. x3 +: 0.015625 *. y3)
            simplify (invFt (ft (x3 +: y3))) `shouldBe`
                (0.015625 *. x3 +: 0.015625 *. y3)
