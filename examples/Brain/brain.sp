
variables:
  x[128][128] = 0

constants:
  // Loss and noised k-space data
  im[128][128] = Dataset("kspace.h5", "im")
  re[128][128] = Dataset("kspace.h5", "re")
  // Mask
  mask[128][128] = Dataset("mask.h5", "mask")
  // Lower bound and upper bound
  xLowerBound[128][128] = Dataset("bound.h5", "lb")
  xUpperBound[128][128] = Dataset("bound.h5", "ub")

constraints:
  x >= xLowerBound, x <= xUpperBound

let:
  smootherX = rotate (0, 1) x - x
  smootherY = rotate (1, 0) x - x
  regularization = norm2square smootherX + norm2square smootherY


minimize:
  norm2square ((mask +: 0) * (ft (x +: 0) - (re +: im))) + 3000 *. regularization

solver: lbfgs-b
