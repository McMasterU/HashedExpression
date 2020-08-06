module Var where

import Data.Set (Set, fromList)
import HashedExpression.Internal.Expression
import HashedExpression.Operation
import Prelude hiding (const)

type Default1D = 4

defaultDim1D = valueFromNat @Default1D

type Default2D1 = 5

default1stDim2D = valueFromNat @Default2D1

type Default2D2 = 6

default2ndDim2D = valueFromNat @Default2D2

type Default3D1 = 5

default1stDim3D = valueFromNat @Default3D1

type Default3D2 = 5

default2ndDim3D = valueFromNat @Default3D2

type Default3D3 = 5

default3rdDim3D = valueFromNat @Default3D3

[zero, one] = map constant [0, 1]

[zero1, one1] = map (constant1D @Default1D) [0, 1]

[zero2, one2] = map (constant2D @Default2D1 @Default2D2) [0, 1]

[zero3, one3] = map (constant3D @Default3D1 @Default3D2 @Default3D3) [0, 1]

[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z] =
  map (variable . show) ['a' .. 'z']

[a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1, v1, w1, x1, y1, z1] =
  map (variable1D @Default1D . (++ "1") . show) ['a' .. 'z']

[a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2, w2, x2, y2, z2] =
  map (variable2D @Default2D1 @Default2D2 . (++ "2") . show) ['a' .. 'z']

[a3, b3, c3, d3, e3, f3, g3, h3, i3, j3, k3, l3, m3, n3, o3, p3, q3, r3, s3, t3, u3, v3, w3, x3, y3, z3] =
  map (variable3D @Default3D1 @Default3D2 @Default3D3 . (++ "3") . show) ['a' .. 'z']

allVars :: Set String
allVars = fromList $ vars ++ map (++ "1") vars ++ map (++ "2") vars ++ map (++ "3") vars
  where
    vars = map return ['a' .. 'z']
