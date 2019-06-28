{-# LANGUAGE FlexibleInstances #-}

module Commons where

import Control.Monad (forM)
import Data.Function.HT (nest)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Set (Set, fromList, toList)
import HashedExpression
import HashedInterp
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
import HashedSimplify
import HashedUtils ((|>))
import Prelude hiding
    ( (*)
    , (+)
    , (-)
    , (/)
    , acos
    , acosh
    , asin
    , asinh
    , atan
    , atanh
    , const
    , cos
    , cosh
    , exp
    , log
    , negate
    , product
    , sin
    , sinh
    , sqrt
    , sum
    , tan
    , tanh
    )
import Test.Hspec
import Test.QuickCheck
import Control.Applicative (liftA2)

-- |
--
[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z] =
    map var
        [ "a"
        , "b"
        , "c"
        , "d"
        , "e"
        , "f"
        , "g"
        , "h"
        , "i"
        , "j"
        , "k"
        , "l"
        , "m"
        , "n"
        , "o"
        , "p"
        , "q"
        , "r"
        , "s"
        , "t"
        , "u"
        , "v"
        , "w"
        , "x"
        , "y"
        , "z"
        ]

[a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1, v1, w1, x1, y1, z1] =
    map (var1d 10)
        [ "A1"
        , "B1"
        , "C1"
        , "D1"
        , "E1"
        , "F1"
        , "G1"
        , "H1"
        , "I1"
        , "J1"
        , "K1"
        , "L1"
        , "M1"
        , "N1"
        , "O1"
        , "P1"
        , "Q1"
        , "R1"
        , "S1"
        , "T1"
        , "U1"
        , "V1"
        , "W1"
        , "X1"
        , "Y1"
        , "Z1"
        ]

[a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2, w2, x2, y2, z2] =
    map (var2d (10, 10))
        [ "A2"
        , "B2"
        , "C2"
        , "D2"
        , "E2"
        , "F2"
        , "G2"
        , "H2"
        , "I2"
        , "J2"
        , "K2"
        , "L2"
        , "M2"
        , "N2"
        , "O2"
        , "P2"
        , "Q2"
        , "R2"
        , "S2"
        , "T2"
        , "U2"
        , "V2"
        , "W2"
        , "X2"
        , "Y2"
        , "Z2"
        ]

[a3, b3, c3, d3, e3, f3, g3, h3, i3, j3, k3, l3, m3, n3, o3, p3, q3, r3, s3, t3, u3, v3, w3, x3, y3, z3] =
    map (var3d (10, 10, 10))
        [ "A3"
        , "B3"
        , "C3"
        , "D3"
        , "E3"
        , "F3"
        , "G3"
        , "H3"
        , "I3"
        , "J3"
        , "K3"
        , "L3"
        , "M3"
        , "N3"
        , "O3"
        , "P3"
        , "Q3"
        , "R3"
        , "S3"
        , "T3"
        , "U3"
        , "V3"
        , "W3"
        , "X3"
        , "Y3"
        , "Z3"
        ]

[ac, bc, cc, dc, ec, fc, gc, hc, ic, jc, kc, lc, mc, nc, oc, pc, qc, rc, sc, tc, uc, vc, wc, xc, yc, zc] =
    [ var "ar" +: var "ai"
    , var "br" +: var "bi"
    , var "cr" +: var "ci"
    , var "dr" +: var "di"
    , var "er" +: var "ei"
    , var "fr" +: var "fi"
    , var "gr" +: var "gi"
    , var "hr" +: var "hi"
    , var "ir" +: var "ii"
    , var "jr" +: var "ji"
    , var "kr" +: var "ki"
    , var "lr" +: var "li"
    , var "mr" +: var "mi"
    , var "nr" +: var "ni"
    , var "or" +: var "oi"
    , var "pr" +: var "pi"
    , var "qr" +: var "qi"
    , var "rr" +: var "ri"
    , var "sr" +: var "si"
    , var "tr" +: var "ti"
    , var "ur" +: var "ui"
    , var "vr" +: var "vi"
    , var "wr" +: var "wi"
    , var "xr" +: var "xi"
    , var "yr" +: var "yi"
    , var "zr" +: var "zi"
    ]

[zero, one] = map const [0, 1]

[zero1, one1] = map (const1d 10) [0, 1]

[zero2, one2] = map (const2d (10, 10)) [0, 1]

[zero3, one3] = map (const3d (10, 10, 10)) [0, 1]

-- | Remove duplicate but also sort
--
removeDuplicate :: (Ord a) => [a] -> [a]
removeDuplicate = toList . fromList

-- | Format
--
format :: [(String, String)] -> String
format = intercalate "\n" . map oneLine
  where
    oneLine (f, s) = f ++ ": " ++ s

-- |
--
relativeError :: Double -> Double -> Double
relativeError a b = abs (a - b) / max (abs a) (abs b)

-- |
--
sum :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
sum = fromJust . HashedOperation.sum

product :: (DimensionType d, NumType et) => [Expression d et] -> Expression d et
product = fromJust . HashedOperation.product

-- | Gen functions R
--
primitiveZeroR :: Gen (Expression Zero R, [String])
primitiveZeroR = do
    name <- elements . map pure $ ['a' .. 'z']
    dbl <- arbitrary
    elements [(var name, [name]), (const dbl, [])]

fromNaryZeroR ::
       ([Expression Zero R] -> Expression Zero R)
    -> Gen (Expression Zero R, [String])
fromNaryZeroR f = do
    numOperands <- elements [3 .. 6]
    -- 90% getting a primitive, 10% probably get a nested expression
    ons <-
        vectorOf numOperands $
        oneof (replicate 9 primitiveZeroR ++ replicate 1 genZeroR)
    let exp = f . map fst $ ons
        names = removeDuplicate . concatMap snd $ ons
    return (exp, names)

fromUnaryZeroR ::
       (Expression Zero R -> Expression Zero R)
    -> Gen (Expression Zero R, [String])
fromUnaryZeroR f = do
    on <- genZeroR
    let exp = f . fst $ on
        names = snd on
    return (exp, names)

fromBinaryZeroR ::
       (Expression Zero R -> Expression Zero R -> Expression Zero R)
    -> Gen (Expression Zero R, [String])
fromBinaryZeroR f = do
    on1 <- genZeroR
    on2 <- genZeroR
    let exp = f (fst on1) (fst on2)
        names = removeDuplicate $ snd on1 ++ snd on2
    return (exp, names)

genZeroR :: Gen (Expression Zero R, [String])
genZeroR = do
    let nary = map fromNaryZeroR [sum, product]
        binary = map fromBinaryZeroR [(*.)]
        unary = map fromUnaryZeroR [negate]
    oneof ([primitiveZeroR] ++ nary ++ binary ++ unary)

-- | Gen functions C
--
primitiveZeroC :: Gen (Expression Zero C, [String])
primitiveZeroC = do
    name1 <- elements . map pure $ ['a' .. 'z']
    name2 <- elements . map pure $ ['a' .. 'z']
    dbl <- arbitrary
    elements [(var name1 +: var name2, [name1, name2]), (const dbl +: const 0, [])]

fromNaryZeroC ::
       ([Expression Zero C] -> Expression Zero C)
    -> Gen (Expression Zero C, [String])
fromNaryZeroC f = do
    numOperands <- elements [3 .. 6]
    -- 90% getting a primitive, 10% probably get a nested expression
    ons <-
        vectorOf numOperands $
        oneof (replicate 9 primitiveZeroC ++ replicate 1 genZeroC)
    let exp = f . map fst $ ons
        names = removeDuplicate . concatMap snd $ ons
    return (exp, names)

fromUnaryZeroC ::
       (Expression Zero C -> Expression Zero C)
    -> Gen (Expression Zero C, [String])
fromUnaryZeroC f = do
    on <- genZeroC
    let exp = f . fst $ on
        names = snd on
    return (exp, names)

fromBinaryZeroC ::
       (Expression Zero C -> Expression Zero C -> Expression Zero C)
    -> Gen (Expression Zero C, [String])
fromBinaryZeroC f = do
    on1 <- genZeroC
    on2 <- genZeroC
    let exp = f (fst on1) (fst on2)
        names = removeDuplicate $ snd on1 ++ snd on2
    return (exp, names)

genZeroC :: Gen (Expression Zero C, [String])
genZeroC = do
    let nary = map fromNaryZeroC [sum, product]
        binary = map fromBinaryZeroC [(*.)]
        unary = map fromUnaryZeroC [negate]
--        fromReal = liftA2 (+:) genZeroR genZeroR
    oneof ([primitiveZeroC] ++ nary ++ binary ++ unary)

