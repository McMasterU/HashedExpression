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

[x, y, z, u, v, w] = map var ["x", "y", "z", "u", "v", "w"]

[x1, y1, z1, u1, v1, w1] = map (var1d 4) ["X1", "Y1", "Z1", "U1", "V1", "W1"]

[x2, y2, z2, u2, v2, w2] =
    map (var2d (4, 4)) ["X2", "Y2", "Z2", "U2", "V2", "W2"]

[x3, y3, z3, u3, v3, w3] =
    map (var3d (4, 4, 4)) ["X3", "Y3", "Z3", "U3", "V3", "W3"]

zero1 = simplify $ x1 - x1

zero2 = simplify $ x2 - x2

zero3 = simplify $ x3 - x3

shouldCompile :: a -> Expectation
shouldCompile _ = return ()

spec :: Spec
spec =
    describe "simplify expression tests" $ do
        specify "TODO" $ do
            pendingWith
                "Don't have below simplifying rules, need implementations"
            {-
            FIXME add the rule for log(exp x) but for exp(log x) we need undefined and piecewise functions
            -}
            simplify (log (exp x)) `shouldBe` x
            simplify (exp (log x)) `shouldBe` x
            {-
            \subsection{simpTest4} Division
            A rule for simplifying $\frac{0}{x}$ could be dangerous if x is zero.  The rule is currently in simp1 (18/06/2015), but commented out until we can prove that x is not zero.
            -}
            simplify (0 / x) `shouldBe` 0
            {-
            This, for example, should be safe to simplify.
            -}
            simplify (0 / (x ^ 2 + 1)) `shouldBe` 0
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
               The second, however, does not.
            -}
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
            {-

            \subsection{simpTest3} Exponentiation and logarithms

            $e^x$ with exp
            \begin{itemize}
            \item defined in GHC.Float
            \item takes an instance of the Fractional class
            \end{itemize}

            $e^0$ evaluates right away to 1.
            -}
            exp 0 `shouldBe` 1
            {-
            However, there are cases when a rule for $e^0$ is necessary.  This rule is only in simp1.
            -}
            simplify (exp (x - x)) `shouldBe` 1
            {-
            Passing the one to the simplify function causes an error because there is no instance of simplify for it.
            -}
            --simplify(exp 0) `shouldBe` 1
            {-

            This won't run, but it will compile.
            -}
    --        simplify (1) -- `shouldBe` 1
            {-
            This won't even compile.
            -}
            --simplify(1::Double)
            {-
            But these work.  Simplify needs something in the Transformable class and a literal is too ambiguous for that.
            -}
            simplify (1 :: Scalar) `shouldBe` 1
            simplify (1 :: ScalarC) `shouldBe` 1 +: 0
            {-

            Let's try this.
            -}
            simplify (x - x) `shouldBe` 0.0
            --simplify (0.0)
            simplify (simplify (x - x)) `shouldBe` 0.0
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
            x ^ 2 `shouldBe` x * x
            {-

            The Scalar x cannot be interpreted as being in the Integral class, so these raise type errors.
            -}
            --x^(x-x)
            --x^^(x-x)
            {-
            This does work.
            -}
            simplify (x ** (x - x)) `shouldBe` 1
            {-

            It looks like there's no support for vector exponents, which is fine, we don't really ever multiply vectors anyways.  The first example raises an exception: can't multiply 1d vectors
            -}
            shouldCompile $ x1 ^ 2
            shouldCompile $ x2 ^ 2
            shouldCompile $ x3 ^ 2
            {-

            And these won't compile.
            -}
            --x1^^2
            --x2^^2
            --x3^^2
            --x1**2
            --x2**2
            --x3**2
            {- Another random thing involving division; it associates to the left and has the same precedence as multiplication
            , which is no surprise to anyone.
            -}
            simplify (x / y * 0) `shouldBe` 0
