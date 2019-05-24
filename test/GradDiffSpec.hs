{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GradDiffSpec where

import Test.Hspec

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

%funGrad (x1<.>x1) [p "X1"]
Declare a dvar.
-}
dx1 = dvar1d 4 "X1"

shouldCompile :: a -> Expectation
shouldCompile _ = return ()

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
spec :: Spec
spec =
    describe "gradient diff tests" $ do
        specify "test 0" $ do
            simplify (diff [p "X1"] (x1 <.> x1 + x)) `shouldBe` dx1 <.> x1 * 2
            simplify (diff [p "X1"] (sin (x1 <.> x1))) `shouldBe`
                simplify (cos (x1 <.> x1) * dx1 <.> x1 * 2)
            simplify (diff [p "X1"] (x1 <.> x1 + y1 <.> y1)) `shouldBe` dx1 <.>
                x1 *
                2
            simplify (diff [p "X1"] (x1 <.> x1 * sin (x1 <.> x1))) `shouldBe`
                simplify
                    (sin (x1 <.> x1) * dx1 <.> x1 * 2 +
                     cos (x1 <.> x1) * x1 <.> x1 * dx1 <.> x1 * 2)
            simplify (diff [p "X1"] x) `shouldBe` 0
            simplify (diff [p "X1"] (y1 <.> y1)) `shouldBe` 0
            shouldCompile $
                simplify (diff [p "X1"] ((sin (x1 <.> x1)) * (cos (x1 <.> x1))))
