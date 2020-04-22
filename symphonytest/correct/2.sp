variables:
  t[50][50] = 0


constants:
  vx[50][50] = File("vx.txt")
  vy[50][50] = File("vy.txt")
  maskUp[50][50] = Pattern(LAST_ROW_0)
  maskRight[50][50] = Pattern(FIRST_COLUMN_0)
  maskT[50][50] = Pattern(LAST_ROW_1)

let:
  tUp = rotate (-1, 0) t
  tRight = rotate (0, 1) t
  vxUp = (vx + rotate (-1, 0) vx) / 2
  vxRight = (vx + rotate (0, 1) vx) / 2
  vyUp = (vy + rotate (-1, 0) vy) / 2
  vyRight = (vy + rotate (0, 1) vy) / 2
  matchUp =
      ((t - tUp) * (vxUp * vxUp + vyUp * vyUp) - vyUp) * vyUp
  matchRight =
      ((t - tRight) * (vxRight * vxRight + vyRight * vyRight) - vxRight) * vxRight
  tZeroOnBottom = maskT <.> (t * t)


minimize:
  maskUp <.> (matchUp * matchUp) + maskRight <.> (matchRight * matchRight) + tZeroOnBottom
