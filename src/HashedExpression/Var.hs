module HashedExpression.Var where

import Data.Set (Set, fromList)
import HashedExpression.Expression

import HashedExpression.Operation
import Prelude hiding (const)

defaultDim1D = 10

default1stDim2D = 12

default2ndDim2D = 13

default1stDim3D = 10

default2ndDim3D = 10

default3rdDim3D = 10

[zero, one] = map const [0, 1]

[zero1, one1] = map (const1d defaultDim1D) [0, 1]

[zero2, one2] = map (const2d (default2ndDim2D, default1stDim2D)) [0, 1]

[zero3, one3] =
    map (const3d (default1stDim3D, default2ndDim3D, default3rdDim3D)) [0, 1]

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
        [ "a1"
        , "b1"
        , "c1"
        , "d1"
        , "e1"
        , "f1"
        , "g1"
        , "h1"
        , "i1"
        , "j1"
        , "k1"
        , "l1"
        , "m1"
        , "n1"
        , "o1"
        , "p1"
        , "q1"
        , "r1"
        , "s1"
        , "t1"
        , "u1"
        , "v1"
        , "w1"
        , "x1"
        , "y1"
        , "z1"
        ]

[a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2, w2, x2, y2, z2] =
    map (var2d (default2ndDim2D, default1stDim2D))
        [ "a2"
        , "b2"
        , "c2"
        , "d2"
        , "e2"
        , "f2"
        , "g2"
        , "h2"
        , "i2"
        , "j2"
        , "k2"
        , "l2"
        , "m2"
        , "n2"
        , "o2"
        , "p2"
        , "q2"
        , "r2"
        , "s2"
        , "t2"
        , "u2"
        , "v2"
        , "w2"
        , "x2"
        , "y2"
        , "z2"
        ]

[a3, b3, c3, d3, e3, f3, g3, h3, i3, j3, k3, l3, m3, n3, o3, p3, q3, r3, s3, t3, u3, v3, w3, x3, y3, z3] =
    map (var3d (default1stDim3D, default2ndDim3D, default3rdDim3D))
        [ "a3"
        , "b3"
        , "c3"
        , "d3"
        , "e3"
        , "f3"
        , "g3"
        , "h3"
        , "i3"
        , "j3"
        , "k3"
        , "l3"
        , "m3"
        , "n3"
        , "o3"
        , "p3"
        , "q3"
        , "r3"
        , "s3"
        , "t3"
        , "u3"
        , "v3"
        , "w3"
        , "x3"
        , "y3"
        , "z3"
        ]

allVars :: Set String
allVars =
    fromList $
    vars ++ map (++ "1") vars ++ map (++ "2") vars ++ map (++ "3") vars
  where
    vars = map return ['a' .. 'z']
