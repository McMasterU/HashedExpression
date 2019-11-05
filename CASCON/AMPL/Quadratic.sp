
variables:
  x = 0

constants:
  a = 1
  b = 0
  c = 5
  n = -10
  m = 0

constraints:
  x^2 <= m

minimize:
  a * x^2 + b * x + c
