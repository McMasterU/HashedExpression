{-# LANGUAGE FlexibleInstances #-}

module TestCommons where

import Control.Monad (forM)
import Data.Function.HT (nest)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import HashedExpression
import HashedOperation
import HashedPrettify
import HashedSimplify
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
    , sin
    , sinh
    , sqrt
    , sum
    , tan
    , tanh
    )
import Test.Hspec
import Test.QuickCheck

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

genVarZero :: Gen (Expression Zero R, String)
genVarZero = do
    name <- elements ["x", "y", "z", "t", "u", "v", "w", "s"]
    return (var name, name)

genTransformZeroR :: Gen (Expression Zero R -> Expression Zero R, Maybe String)
genTransformZeroR = do
    (other, name) <- genVarZero
    unaryOp <-
        elements [sin, cos, tan] :: Gen (Expression Zero R -> Expression Zero R)
    binaryWithOther <-
        elements
            [ (other *)
            , (other +)
            , (other *.)
            , (other -)
            , (-) other
            , (* other)
            , (*. other)
            ] :: Gen (Expression Zero R -> Expression Zero R)
    pickUnary <- arbitrary
    if pickUnary
        then return (unaryOp, Nothing)
        else return (binaryWithOther, Just name)

--    elements [unaryOp, binaryWithOther]
data SuiteZeroR =
    SuiteZeroR (Expression Zero R) [(String, Double)] String
    deriving (Show, Ord, Eq)

instance Arbitrary SuiteZeroR where
    arbitrary = do
        (baseExp, baseVarName) <- genVarZero
        numTransform <- elements [5 .. 15]
        transformsWithVar <- vectorOf numTransform genTransformZeroR
        let finalExp = ($ baseExp) . chain . map fst $ transformsWithVar
            varNames = baseVarName : mapMaybe snd transformsWithVar
        doubles <- vectorOf (length varNames) arbitrary
        return $ SuiteZeroR finalExp (zip varNames doubles) (prettify finalExp)
