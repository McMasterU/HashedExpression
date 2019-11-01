

variables:
  x[256][256] = 0
  y[256][256] = 0

constants:
  im0[256][256] = Dataset("fruit.h5", "im0")
  re0[256][256] = Dataset("fruit.h5", "re0")
  im1[256][256] = Dataset("fruit.h5", "im1")
  re1[256][256] = Dataset("fruit.h5", "re1")
  filter[256][256] = Dataset("fruit.h5", "filter")

let:
  kspace = ft (x +: y)
  magn   = (xRe kspace) * (xRe kspace) + (xIm kspace) * (xIm kspace)

constraints:
  magn <.> filter <= 3000

minimize:
  norm2square ((ft (x +: y) - (re0 +: im0))) +
   norm2square ((ft (x +: y) - (re1 +: im1)))
