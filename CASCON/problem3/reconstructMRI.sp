
variables:
  a[256][256] = 0
  b[256][256] = 0

constants:
  im0[256][256] = Dataset("fruit.h5", "im0")
  re0[256][256] = Dataset("fruit.h5", "re0")
  im1[256][256] = Dataset("fruit.h5", "im1")
  re1[256][256] = Dataset("fruit.h5", "re1")
  im2[256][256] = Dataset("fruit.h5", "im2")
  re2[256][256] = Dataset("fruit.h5", "re2")
  im3[256][256] = Dataset("fruit.h5", "im3")
  re3[256][256] = Dataset("fruit.h5", "re3")
  im4[256][256] = Dataset("fruit.h5", "im4")
  re4[256][256] = Dataset("fruit.h5", "re4")
  im5[256][256] = Dataset("fruit.h5", "im5")
  re5[256][256] = Dataset("fruit.h5", "re5")
  im6[256][256] = Dataset("fruit.h5", "im6")
  re6[256][256] = Dataset("fruit.h5", "re6")
  im7[256][256] = Dataset("fruit.h5", "im7")
  re7[256][256] = Dataset("fruit.h5", "re7")
  filter[256][256] = Dataset("fruit.h5", "filter")

let:
  lambda = 1.0e5
  smootherAX = rotate (0, 1) a + rotate (0, -1) a - 2 *. a
  smootherAY = rotate (0, 1) a + rotate (0, -1) a - 2 *. a
  smootherBX = rotate (1, 0) b + rotate (-1, 0) b - 2 *. b
  smootherBY = rotate (1, 0) b + rotate (-1, 0) b - 2 *. b
  regularization = norm2square smootherAX + norm2square smootherAY
                  + norm2square smootherBX + norm2square smootherBY
  coilSum = (re0 +: im0)
              + (re1 +: im1)
              + (re2 +: im2)
              + (re3 +: im3)
              + (re4 +: im4)
              + (re5 +: im5)
              + (re6 +: im6)
              + (re7 +: im7)

minimize:
  norm2square (ft (a +: b) - coilSum)
    + lambda*regularization
