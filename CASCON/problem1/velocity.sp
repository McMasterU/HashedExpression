

variables:
  t[50][50] = 0


constants:
  vx = File("vx.txt")
  vy = File("vy.txt")

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


minimize:
  maskUp <.> (matchUp * matchUp) + maskRight <.> (matchRight * matchRight)

