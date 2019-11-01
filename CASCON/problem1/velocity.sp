

variables:
  x[4][2]

constants:
  mask[4][2] = Pattern(FIRST_ROW_1)

minimize:
  norm2square (x - mask)

