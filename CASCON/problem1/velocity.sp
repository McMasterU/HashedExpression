

variables:
  t[50][50] = 0


constants:
  vx[50][50] = File("vx.txt")
  vy[50][50] = File("vy.txt")
  maskUp[50][50] = Pattern(LAST_ROW_0)
  maskRight[50][50] = Pattern(FIRST_COLUMN_0)

let:
  tUp = rotate (-1, 0) t
  tRight = rotate (0, 1) t
  vxUp = 0.5 *. (vx + rotate (-1, 0) vx)
  vxRight = 0.5 *. (vx + rotate (0, 1) vx)
  vyUp = 0.5 *. (vy + rotate (-1, 0) vy)
  vyRight = 0.5 *. (vy + rotate (0, 1) vy)
  matchUp =
      ((t - tUp) * (vxUp * vxUp + vyUp * vyUp) - vyUp) * vyUp
  matchRight =
      ((t - tRight) * (vxRight * vxRight + vyRight * vyRight) - vxRight) * vxRight


minimize:
  maskUp <.> (matchUp * matchUp) + maskRight <.> (matchRight * matchRight)

