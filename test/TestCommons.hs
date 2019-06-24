{-# LANGUAGE FlexibleInstances #-}

module TestCommons where

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

-- | Gen functions and Arbitrary instances
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
    ons <- vectorOf numOperands $ oneof (replicate 9 primitiveZeroR ++ replicate 1 genZeroR)
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

-- |
--
[x, y, z, t, u, v, w, s] = map var ["x", "y", "z", "t", "u", "v", "w", "s"]

[x1, y1, z1, t1, u1, v1, w1] =
    map (var1d 10) ["X1", "Y1", "Z1", "T1", "U1", "V1", "W1"]

[x2, y2, z2, t2, u2, v2, w2] =
    map (var2d (10, 10)) ["X2", "Y2", "Z2", "T2", "U2", "V2", "W2"]

[x3, y3, z3, t3, u3, v3, w3] =
    map (var3d (10, 10, 10)) ["X3", "Y3", "Z3", "T3", "U3", "V3", "W3"]

[xc, yc, zc, tc, uc, vc, wc, sc] =
    [ var "xr" +: var "xi"
    , var "yr" +: var "yi"
    , var "zr" +: var "zi"
    , var "tr" +: var "ti"
    , var "ur" +: var "ui"
    , var "vr" +: var "vi"
    , var "wr" +: var "wi"
    , var "sr" +: var "si"
    ]

[zero, one] = map const [0, 1]

[zero1, one1] = map (const1d 10) [0, 1]

[zero2, one2] = map (const2d (10, 10)) [0, 1]

[zero3, one3] = map (const3d (10, 10, 10)) [0, 1]
