

variables:
  x[128][128] = 0


constants:

  im[128][128] = Dataset("im.h5", "im")                 
  re[128][128] = Dataset("re.h5", "re")                 
  signal[128][128] = Dataset("signal.h5", "signal")      

  xLowerBound[128][128] = Dataset("x_lb.h5", "x_lb")     
  xUpperBound[128][128] = Dataset("x_ub.h5", "x_ub")     
  
  
constraints:
  x >= xLowerBound, x <= xUpperBound
  
  
 
let:
  smootherX = rotate (0, 1) x + rotate (0, -1) x - 2 *. x
  smootherY = rotate (1, 0) x + rotate (-1, 0) x - 2 *. x
  regularization = norm2square smootherX + norm2square smootherY
  

minimize:
  norm2square ((signal +: 0) * (ft x - (re +: im))) + 3000 *. regularization
