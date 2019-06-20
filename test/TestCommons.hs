module TestCommons where

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

[x, y, z, u, v, w, s] = map var ["x", "y", "z", "u", "v", "w", "s"]

[x1, y1, z1, u1, v1, w1] = map (var1d 10) ["X1", "Y1", "Z1", "U1", "V1", "W1"]

[x2, y2, z2, u2, v2, w2] =
    map (var2d (10, 10)) ["X2", "Y2", "Z2", "U2", "V2", "W2"]

[zero, one] = map const [0, 1]

[zero1, one1] = map (const1d 10) [0, 1]

[zero2, one2] = map (const2d (10, 10)) [0, 1]
