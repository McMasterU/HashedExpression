module HashedVar where

import HashedExpression
import HashedOperation
import Prelude hiding (const)

[zero, one] = map const [0, 1]

[zero1, one1] = map (const1d 10) [0, 1]

[zero2, one2] = map (const2d (10, 10)) [0, 1]

[zero3, one3] = map (const3d (10, 10, 10)) [0, 1]

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
    map (var2d (10, 10))
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
    map (var3d (10, 10, 10))
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
